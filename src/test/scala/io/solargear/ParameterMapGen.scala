package io.solargear

import org.scalacheck.Gen

trait ParameterMapGen {

  val predefinedKeys: Gen[String] = Gen.oneOf("Key1", "Key2", "Key3")

  val genKey: Gen[String] = Gen.oneOf(Gen.alphaNumStr, predefinedKeys)

  val genMap: Gen[(String, String)] => Gen[Map[String, String]] = genPair => Gen.listOf[(String, String)](genPair).map(_.toMap)
}
