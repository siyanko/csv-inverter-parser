package io.solargear

import org.scalacheck.{Arbitrary, Gen}

trait DoubleGenerator {

  val doublePattern: String = "-?\\d+(\\.\\d+(E\\d+)?)?"

  val roundDouble: Double => Double = d => Math.round(d * 100) / 100
  val doubleToString: Double => String = _.toString

  val genDouble: Gen[Double] =  Arbitrary.arbitrary[Double].map(roundDouble)

  val genOptionDouble: Gen[Option[Double]] = genDouble.map(Option.apply)

  val genStrDouble: Gen[String] = Gen.oneOf(
    genDouble.map(doubleToString),
    Arbitrary.arbitrary[String]
  )

}
