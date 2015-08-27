package net.digitalbebop.pulsefilemodule

import java.io.{FileInputStream, File}
import java.nio.file.attribute.FileOwnerAttributeView
import java.nio.file.{Paths, Files}

import com.google.protobuf.ByteString
import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.parser.PdfTextExtractor
import net.digitalbebop.ClientRequests.IndexRequest
import org.apache.commons.io.IOUtils

import scala.util.parsing.json.JSONObject


object Extracter {

  private final val fileModule = "files"

  def processPdf(file: File, uidMap: Map[String, String]): IndexRequest = {
    val reader = new PdfReader(file.getAbsolutePath)
    val pages = reader.getNumberOfPages
    val strBuilder = new StringBuilder()

    for (page <- 1 to pages) {
      strBuilder.append(PdfTextExtractor.getTextFromPage(reader, page) + " ")
    }
    val indexData = strBuilder.toString()
    val rawData = IOUtils.toByteArray(new FileInputStream(file))

    val indexBuilder = IndexRequest.newBuilder()

    indexBuilder.setIndexData(indexData)
    rawData.map(data => indexBuilder.setRawData(ByteString.copyFrom(rawData)))
    indexBuilder.setLocation(file.getAbsolutePath)
    indexBuilder.setMetaTags(new JSONObject(Map(("format", "pdf"), ("title", file.getName))).toString())
    indexBuilder.setTimestamp(file.lastModified())
    indexBuilder.setModuleId(file.getAbsolutePath)
    indexBuilder.setModuleName(fileModule)
    val view = Files.getFileAttributeView(Paths.get(file.getAbsolutePath), classOf[FileOwnerAttributeView])
    uidMap.get(view.getOwner.getName).map(indexBuilder.setUsername)
    indexBuilder.build()
  }
}

