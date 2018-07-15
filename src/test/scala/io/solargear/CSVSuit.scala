package io.solargear

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

import scala.util.matching.Regex

class CSVSuit extends Properties("CSV") {

  val genCombinedKey: Gen[String] = for {
    delimiter <- Gen.oneOf("", " ")
    words <- Gen.listOf[String](Gen.alphaNumStr)
  } yield words.mkString(delimiter)

  val genKey: Gen[String] = Gen.oneOf(Gen.alphaNumStr, genCombinedKey, Gen.oneOf("", " "))

  property("convertKey") = forAll(genKey) {
    s: String =>
      csv.convertKey(s) == s.replace(" ", "").toLowerCase
  }

  val genCsvLine: Gen[String] = for {
    key <- genCombinedKey
    value <- Gen.alphaNumStr
  } yield s"$key,$value"

  property("lineToPair") = {
    val genTestData: Gen[String] = Gen.oneOf(
      genCsvLine, Gen.oneOf("", " "), Arbitrary.arbitrary[String]
    )
    forAll(genTestData){
      s: String =>
        val pattern: Regex = "(.+),(.+)".r

        val expected: Option[(String, String)] = s match {
          case pattern(a, b) => (Option(a), Option(b)) match {
            case (Some(a), Some(b)) => Some(a, b)
            case _ => None
          }
          case _ => None
        }

        csv.lineToPair(s) == expected
    }
  }

  property("lineToDataSet") = {
    val genTestData: Gen[String] = for{
      line <- genCsvLine
      p1 <- Gen.alphaNumStr
      p2 <- Gen.alphaNumStr
    } yield s"$line,$p1,$p2"

    forAll(genTestData){
      s: String =>
        val expected: Seq[(String, String)] = s.split(",").toList match {
          case a :: ls => ls.map(v => (a, v))
          case _ => Nil
        }

        csv.lineToDataSet(s) == expected
    }
  }

}
