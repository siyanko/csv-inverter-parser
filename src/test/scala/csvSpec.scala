import io.solargear.csv
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Gen, Properties}

import scala.annotation.tailrec

object csvSpec extends Properties("csv properties") {

  val genVowel =
    Gen.oneOf("A", "E", "I", "O", "U", "Y", "a", "e", "i", "o", "u", "y")

  val genConsonant =
    Gen.oneOf(
      "B", "b",
      "C", "c",
      "D", "d",
      "F", "f",
      "G", "g",
      "H", "h",
      "J", "j",
      "K", "k",
      "L", "l",
      "M", "m",
      "N", "n",
      "P", "p",
      "Q", "q",
      "R", "r",
      "S", "s",
      "T", "t",
      "V", "v",
      "W", "w",
      "X", "x",
      "Z", "z",
    )

  val genWord: Gen[String] = Gen.frequency(
    (3, genConsonant),
    (1, genVowel)
  )

  def genParameterNameN(n: Int) = {
    @tailrec
    def loop(m: Int)(acc: Gen[String]): Gen[String] =
      if (m <= 1) acc else loop(m - 1) {
        for {
          s <- acc
          w <- genWord
        } yield s + " " + w
      }

    loop(n)(genWord)
  }

  val getParameterName = Gen.oneOf(
    genParameterNameN(1),
    genParameterNameN(2),
    genParameterNameN(3),
    genParameterNameN(4),
    genParameterNameN(5),
    genParameterNameN(6)
  )

  val genLine = for {
    name <- getParameterName
    value <- genWord
  } yield s"$name,$value"

  property("convertKey") = forAll(genWord) {
    k: String =>
      val res = csv.convertKey(k)
      (!res.contains("(?=\\p{Lu})")) :| "result contains empty spaces" &&
        (res.split("(?=\\p{Lu})").length == 1) :| "result contains upper case letters"
  }


  property("lineToPair") = forAll(genLine) {
    s: String =>
      val pairs: List[String] = s.split(",").toList
      val result: Option[(String, String)] = for {
        n <- pairs.headOption
        v <- pairs.lastOption
        if (!n.isEmpty && !v.isEmpty)
      } yield (n, v)

      csv.lineToPair(s) == result
  }

}
