package net.digitalbebop.pulsefilemodule


import java.io.{FileInputStream, FilenameFilter, File}
import java.nio.file._
import java.nio.file.attribute.{BasicFileAttributes, FileOwnerAttributeView}
import java.security.SecureRandom
import java.security.cert.X509Certificate
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicLong
import javax.net.ssl._

import com.google.protobuf.ByteString
import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.parser.PdfTextExtractor
import com.unboundid.ldap.sdk.{SearchScope, SimpleBindRequest, LDAPConnection}
import net.digitalbebop.ClientRequests.IndexRequest
import org.apache.commons.cli.{DefaultParser, Options}
import org.apache.commons.io.filefilter.TrueFileFilter
import org.apache.commons.io.{FileUtils, IOUtils}
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.ByteArrayEntity
import org.apache.http.impl.client.HttpClients

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.{Future, ExecutionContext}
import scala.util.parsing.json.JSONObject

class NaiveTrustManager extends X509TrustManager {
  /**
   * Doesn't throw an exception, so this is how it approves a certificate.
   * @see javax.net.ssl.X509TrustManager#checkClientTrusted(java.security.cert.X509Certificate[], String)
   **/
  def checkClientTrusted(cert: Array[X509Certificate], authType: String): Unit = { }


  /**
   * Doesn't throw an exception, so this is how it approves a certificate.
   * @see javax.net.ssl.X509TrustManager#checkServerTrusted(java.security.cert.X509Certificate[], String)
   **/
  def checkServerTrusted (cert: Array[X509Certificate], authType: String): Unit = { }

  /**
   * @see javax.net.ssl.X509TrustManager#getAcceptedIssuers()
   **/
   def getAcceptedIssuers(): Array[X509Certificate] = Array()
}

object Main {

  val startTime = System.currentTimeMillis() / 1000
  final val apiServer = "http://localhost:8080"
  final val ldapHost = "ldap.csh.rit.edu"
  final val ldapPort = 389
  final var username = "uid=jd,ou=Users,dc=csh,dc=rit,dc=edu"
  final var password: String = _

  val filesProcessed  = new AtomicLong(0)

  implicit val ec = new ExecutionContext {
    val threadPool = Executors.newCachedThreadPool()

    def execute(runnable: Runnable): Unit = threadPool.submit(runnable)

    def reportFailure(t: Throwable) {}
  }

  val processors = Map[String, (File, Map[String, String]) => IndexRequest](
    ".pdf" -> Extractor.processPdf
  )

  lazy val getSocketFactory: SSLSocketFactory = {
    val tm: Array[TrustManager] = Array(new NaiveTrustManager())
    val context = SSLContext.getInstance("SSL")
    context.init(Array[KeyManager](), tm, new SecureRandom())
    context.getSocketFactory
  }

  lazy val uidMap: Map[String, String] = {
    val ldapConnection = new LDAPConnection(ldapHost, ldapPort)
    ldapConnection.bind(username, password)

    val result = ldapConnection.search("ou=Users,dc=csh,dc=rit,dc=edu",
      SearchScope.SUBORDINATE_SUBTREE, "(uid=*)", "uid", "uidNumber")
    ldapConnection.close()

    val map = result.getSearchEntries.map { entity =>
      (entity.getAttributeValue("uidNumber"), entity.getAttributeValue("uid"))
    }.toMap
    println("generated uid map")
    map
  }

  def getFiles(dir: File, pool: ExecutorService): Unit = if (dir.isDirectory) {
    val children = dir.listFiles()
    if (children != null)
      children.foreach(f => getFiles(f, pool))
  } else if (dir.getName.toLowerCase.endsWith(".pdf")) {
    pool.submit(new Runnable() {
      def run() : Unit = processFile(dir)
    })
  }

  def processFile(file: File): Unit = try {
    val fileExt = file.getName.substring(file.getName.lastIndexOf(".")).toLowerCase
    processors.get(fileExt).map { extractor =>
      val message = extractor(file, uidMap)
      val post = new HttpPost(s"$apiServer/api/index")
      post.setEntity(new ByteArrayEntity(message.toByteArray))
      HttpClients.createDefault().execute(post).close()
      val amount = filesProcessed.incrementAndGet()
      val timeDiff = System.currentTimeMillis() / 1000 - startTime
      println(s"processed $amount files, ${(1.0 * amount) / timeDiff} files/sec")
    }
  } catch {
    case ex: Exception =>
  }

  def main(args: Array[String]): Unit = {

    val options = new Options()
    options.addOption("dir", true, "the directory to recursively look through")

    val parser = new DefaultParser()
    val cmd = parser.parse(options, args)

    val dir = if (cmd.hasOption("dir"))
      cmd.getOptionValue("dir")
    else
      "/"

    print(s"ldap password for ($username): ")
    password = System.console().readPassword().mkString("")

    /*val queue = new ArrayBlockingQueue[String](10)
    val POISON_PILL = "POISON PILL"

    val f =  new FutureTask[Unit](new Callable[Unit]() {
      def call(): Unit = {
        getFiles(new File(dir), queue)
        queue.put(POISON_PILL)
      }
    })

    ec.execute(f)

    val cores = Runtime.getRuntime.availableProcessors
    val pool = Executors.newFixedThreadPool(cores * 2)
    for (i <- 1 to cores) {
      ec.execute(new Runnable() {
        def run() : Unit = {
          while (true) {
            val file = queue.take()
            if (file == POISON_PILL) {
              queue.put(POISON_PILL)
              return
            }
            processFile(new File(file))
          }
        }
      })
    }

    ec.threadPool.shutdown()
    ec.threadPool.awaitTermination(Long.MaxValue, TimeUnit.DAYS)*/

    val cores = Runtime.getRuntime.availableProcessors
    val pool = Executors.newFixedThreadPool(cores * 2)
    getFiles(new File(dir), pool)
    pool.shutdown()
    pool.awaitTermination(Long.MaxValue, TimeUnit.DAYS)

    val endTime = System.currentTimeMillis() / 1000

    println(s"Processed ${filesProcessed.get()} files in ${endTime - startTime} seconds")

  }
}
