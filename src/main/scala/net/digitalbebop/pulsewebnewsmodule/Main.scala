package net.digitalbebop.pulsewebnewsmodule


import java.io.{FileInputStream, FilenameFilter, File}
import java.nio.file.{Paths, Files}
import java.nio.file.attribute.FileOwnerAttributeView
import java.security.SecureRandom
import java.security.cert.X509Certificate
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicLong
import javax.net.ssl._

import com.google.protobuf.ByteString
import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.parser.PdfTextExtractor
import net.digitalbebop.ClientRequests.IndexRequest
import org.apache.commons.cli.{DefaultParser, Options}
import org.apache.commons.io.IOUtils
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.ByteArrayEntity
import org.apache.http.impl.client.HttpClients

import scala.concurrent.{Future, ExecutionContext}
import scala.io.Source
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

class fileFilter extends FilenameFilter {

  val extensions = List(".pdf", ".txt", ".text")

  def accept(dir: File, name: String): Boolean = {
    for (ext <- extensions) {
      if (name.endsWith(ext)) {
        return true
      }
    }
    return false
  }
}

object Main {

  var startTime: Long = _
  final val moduleName = "files"
  final val apiServer = "http://localhost:8080"
  val filesProcessed  = new AtomicLong(0)

  implicit val ec = new ExecutionContext {
    val threadPool = Executors.newCachedThreadPool()

    def execute(runnable: Runnable): Unit = threadPool.submit(runnable)

    def reportFailure(t: Throwable) {}
  }

  val processors = Map(
    "application/pdf" -> new PdfExtractor(),
    "text/plain" -> new TextExtractor(),
    "text/html" -> new HtmlExtractor()
  )


  lazy val getSocketFactory: SSLSocketFactory = {
    val tm: Array[TrustManager] = Array(new NaiveTrustManager())
    val context = SSLContext.getInstance("SSL")
    context.init(Array[KeyManager](), tm, new SecureRandom())
    context.getSocketFactory
  }

  def getFiles(file: File): Iterator[File] = file.listFiles.toIterator.flatMap{ child =>
    if (child.isDirectory) {
      getFiles(child)
    } else {
      Stream(child)
    }
  }

  def processFile(file: File): Unit = try {
    val fileType = Files.probeContentType(Paths.get(file.getAbsolutePath))

    processors.get(fileType).map { extractor =>
      val (indexData, format, rawData) = extractor.processFile(file)
      val builder = IndexRequest.newBuilder()
      builder.setIndexData(indexData)
      rawData match {
        case Some(data) => builder.setRawData(ByteString.copyFrom(data))
        case None => {}
      }

      builder.setLocation(file.getAbsolutePath)
      builder.setMetaTags(new JSONObject(Map(("format", format), ("title", file.getName))).toString)
      builder.setTimestamp(file.lastModified())
      builder.setModuleId(file.getAbsolutePath)
      builder.setModuleName(moduleName)
      builder.setLocation(file.getAbsolutePath)
      val view = Files.getFileAttributeView(Paths.get(file.getAbsolutePath), classOf[FileOwnerAttributeView])
      builder.setUsername(view.getOwner.getName)
      val message = builder.build()
      val post = new HttpPost(s"$apiServer/api/index")
      post.setEntity(new ByteArrayEntity(message.toByteArray))
      HttpClients.createDefault().execute(post)
      val amount = filesProcessed.incrementAndGet()
      if (amount % 1000 == 0) {
        val timeDiff = System.currentTimeMillis() / 1000 - startTime
        println(s"processed $amount files, ${(1.0 * amount) / timeDiff} files/sec")
      }
    }
  } catch {
    case ex: Exception =>
      println(s"could not process file: $file")
      ex.printStackTrace()
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

    println(s"indexing directory: $dir")
    val startTime = System.currentTimeMillis() / 1000

    getFiles(new File(dir)).foreach(processFile)

    val endTime = System.currentTimeMillis() / 1000;

    println(s"Processed ${filesProcessed.get()} files in ${endTime - startTime} seconds")

  }
}
