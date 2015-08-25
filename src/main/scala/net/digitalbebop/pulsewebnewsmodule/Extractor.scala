package net.digitalbebop.pulsewebnewsmodule

import java.io.{FileInputStream, File}

import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.parser.PdfTextExtractor
import org.apache.commons.io.IOUtils
import org.jsoup.Jsoup

import scala.io.Source


trait Extracter {
  // index data, raw data, format
  def processFile(file: File): (String, String, Option[Array[Byte]])
}

class PdfExtractor extends Extracter {
  def processFile(file: File): (String, String, Option[Array[Byte]]) = {
    val reader = new PdfReader(file.getAbsolutePath)
    val pages = reader.getNumberOfPages
    val builder = new StringBuilder()

    for (page <- 1 to pages) {
      builder.append(PdfTextExtractor.getTextFromPage(reader, page) + " ")
    }
    val indexData = builder.toString()
    val rawData = IOUtils.toByteArray(new FileInputStream(file))
    (indexData, "pdf", Some(rawData))
  }
}

class TextExtractor extends Extracter {
  def processFile(file: File): (String, String, Option[Array[Byte]]) = {
    (Source.fromFile(file).mkString, "text", None)
  }
}

class HtmlExtractor extends Extracter {
  def processFile(file: File): (String, String, Option[Array[Byte]]) = {
    val rawData = Source.fromFile(file).mkString
    val indexedData = Jsoup.parse(rawData).body().text()
    (indexedData, "html", Some(rawData.getBytes))
  }
}

