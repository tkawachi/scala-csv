package com.github.tototoshi.csv

import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream}

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class CsvWriteReadSpec extends FunSpec with Checkers with Matchers with Using {
  trait ShowCsvFormat {
    self: CSVFormat =>

    override def toString: String =
      s"delimiter: $delimiter, quoteChar: $quoteChar, treatEmptyLineAsNil: $treatEmptyLineAsNil, escapeChar: $escapeChar, lineTerminator: $lineTerminator, quoting: $quoting"
  }

  val csvFormat = new DefaultCSVFormat with ShowCsvFormat {}
  val tsvFormat = new TSVFormat with ShowCsvFormat {}

  def quotingGen: Gen[Quoting] = Gen.oneOf(QUOTE_ALL, QUOTE_MINIMAL, QUOTE_NONE, QUOTE_NONNUMERIC)

  implicit lazy val quotingArb: Arbitrary[Quoting] = Arbitrary(quotingGen)

  def formatGen: Gen[CSVFormat] = for {
    del <- arbitrary[Char]
    qc <- arbitrary[Char] if del != qc
    emptyAsNil <- arbitrary[Boolean]
    escape <- arbitrary[Char] if escape != del && escape != qc
    lineTerm <- Gen.oneOf("\r", "\n", "\r\n")
    quo <- arbitrary[Quoting]
  } yield new CSVFormat with ShowCsvFormat {
      override val delimiter: Char = del
      override val quoteChar: Char = qc
      override val treatEmptyLineAsNil: Boolean = emptyAsNil
      override val escapeChar: Char = escape
      override val lineTerminator: String = lineTerm
      override val quoting: Quoting = quo
    }

  implicit lazy val formatArb: Arbitrary[CSVFormat] =
    Arbitrary(Gen.oneOf(Gen.const(csvFormat), Gen.const(tsvFormat), formatGen))

  it ("reads written contents") {
    val charset = "UTF-8"

    check { (format: CSVFormat, allLines: Seq[Seq[String]]) =>
      val out = new ByteArrayOutputStream()
      val writer = new OutputStreamWriter(out, charset)
      using(new CSVWriter(writer)(format)) { csvWriter =>
        csvWriter.writeAll(allLines)
      }
      val reader = new InputStreamReader(new ByteArrayInputStream(out.toByteArray), charset)
      val readLines = using(CSVReader.open(reader)(format)) { csvReader =>
        csvReader.all()
      }
      (readLines == allLines) :|
        s"written: $allLines, ${allLines.map(_.size)}, read: $readLines, ${readLines.map(_.size)}"
    }
  }

}