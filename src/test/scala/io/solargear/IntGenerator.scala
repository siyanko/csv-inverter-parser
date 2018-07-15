package io.solargear

import org.scalacheck.{Arbitrary, Gen}

trait IntGenerator {

  val genInt: Gen[Int] = Arbitrary.arbitrary[Int]

  val genStrInt: Gen[String] = Gen.oneOf(
    genInt.map(_.toString),
    Arbitrary.arbitrary[String]
  )
}
