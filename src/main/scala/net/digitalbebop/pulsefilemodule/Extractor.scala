package net.digitalbebop.pulsefilemodule

import java.io.{FileInputStream, File}
import java.nio.file.attribute.FileOwnerAttributeView
import java.nio.file.{Paths, Files}
import java.util.UUID

import com.google.protobuf.ByteString
import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.parser.PdfTextExtractor
import net.digitalbebop.ClientRequests.IndexRequest
import org.apache.commons.io.{FilenameUtils, IOUtils}

import scala.util.parsing.json.JSONObject


object Extractor {

  private final val fileModule = "files"

  private def splitCamelCase(s: String): String = s.replaceAll(String.format("%s|%s|%s",
    "(?<=[A-Z])(?=[A-Z][a-z])", "(?<=[^A-Z])(?=[A-Z])", "(?<=[A-Za-z])(?=[^A-Za-z])"), " ")

  private def replaceSplits(s: String) = s.replaceAll("[_|-]", " ")

  private def cleanString: String => String = splitCamelCase _ compose replaceSplits

  private def getTags(file: File, uidMap: Map[String, String]): Array[String] = file.getAbsolutePath.split("/")
    .dropWhile(s => !uidMap.contains(s))
    .flatMap { name =>
      if (name.endsWith(".pdf")) {
        cleanString(FilenameUtils.removeExtension(name)).split(" ")
      } else {
        cleanString(name).split(" ")
      }
    }

  // TODO add tags for file name
  def processPdf(file: File, uidMap: Map[String, String]): IndexRequest = {
    val reader = new PdfReader(file.getAbsolutePath)
    val indexData = (1 to reader.getNumberOfPages).foldLeft(new StringBuilder()) { (builder, page) =>
      builder.append(PdfTextExtractor.getTextFromPage(reader, page) + " ")
    }.toString()

    val indexBuilder = IndexRequest.newBuilder()
    indexBuilder.setIndexData(indexData)
    indexBuilder.setRawData(ByteString.readFrom(Files.newInputStream(file.toPath)))
    indexBuilder.setLocation(file.getAbsolutePath)
    indexBuilder.setMetaTags(new JSONObject(Map(("format", "pdf"), ("title", file.getName))).toString())
    indexBuilder.setTimestamp(file.lastModified())
    indexBuilder.setModuleId(UUID.nameUUIDFromBytes(file.getAbsolutePath.getBytes).toString)
    indexBuilder.setModuleName(fileModule)
    getTags(file, uidMap).foreach(indexBuilder.addTags)

    val view = Files.getFileAttributeView(Paths.get(file.getAbsolutePath), classOf[FileOwnerAttributeView])
    uidMap.get(view.getOwner.getName).map(indexBuilder.setUsername)
    reader.close()
    indexBuilder.build()
  }
}

