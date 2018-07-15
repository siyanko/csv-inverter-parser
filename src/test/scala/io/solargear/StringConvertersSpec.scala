package io.solargear

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class StringConvertersSpec extends Properties("StringConverters") with DoubleGenerator with IntGenerator {

  property("strToDouble") = forAll(genStrDouble){
    s: String =>
      val expected: Option[String] = if (s.matches(doublePattern)) Some(s) else None
      StringConverters.strToDouble(s).map(_.toString) == expected
  }

  property("strToInt") = forAll(genStrInt){
    s: String =>
      val expected: Option[String] = if (s.matches("-?\\d+")) Some(s) else None

      StringConverters.strToInt(s).map(_.toString) == expected
  }
}
