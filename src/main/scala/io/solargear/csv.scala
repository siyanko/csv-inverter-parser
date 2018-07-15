package io.solargear

object csv {

  type Line = String
  type ParameterName = String
  type ParameterValue = String
  type Key = String

  val lineToPair: Line => Option[(ParameterName, ParameterValue)] =
    _.split(",").filterNot(_.isEmpty).toList match {
      case a :: b :: _ => Some(a, b)
      case _ => None
    }

  val lineToDataSet: Line => List[(ParameterName, ParameterValue)] =
    _.split(",")
      .toList match {
      case a :: ls => ls.map(v => (a, v))
      case _ => Nil
    }

  val convertKey: Key => Key = _.replace(" ", "").toLowerCase
}