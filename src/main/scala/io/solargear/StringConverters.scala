package io.solargear

import scala.util.Try

object StringConverters {

  def strTo[A](s: String)(f: String => A): Option[A] = Try(f(s)).toOption

  val strToDouble: String => Option[Double] = s => strTo[Double](s)(_.toDouble)
  val strToInt: String => Option[Int] = s => strTo[Int](s)(_.toInt)

}
