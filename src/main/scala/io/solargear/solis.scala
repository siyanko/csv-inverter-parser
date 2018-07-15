package io.solargear

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import io.solargear.StringConverters._

import scala.util.Try
import scala.util.matching.Regex

object solis {

  val fromKilos: Option[Double] => Option[Double] = _.map(_ * 1000.0)

  def get[A](key: String)(ff: String => Option[A]): Map[String, String] => ValidatedNel[PropertyParseError, A] = data =>
    getValue(data, key)(ff)

  val parserRangeOfDouble: String => (Option[Double], Option[Double]) = s => {
    val doublePattern: String = "-?\\d+(\\.\\d+(E\\d+)?)?"
    val pattern: Regex = s"($doublePattern)-($doublePattern)".r
    s match {
      case pattern(a, _, _, b, _, _) =>
        (Option(a).flatMap(strToDouble), Option(b).flatMap(strToDouble))
      case _ => (None, None)
    }
  }

  def getRangeOfDoubles(key: String): Map[String, String] => ValidatedNel[PropertyParseError, (Option[Double], Option[Double])] = data =>
    getValue(data, key)(parserRangeOfDouble andThen { r => Option(r) })

  implicit val rawDataToInputData: Map[String, String] => ValidatedNel[PropertyParseError, InputData] = data => {
    val (minMpptVoltage, maxMpptVoltage) = getRangeOfDoubles("MPPT voltage range(V)")(data) match{
      case Invalid(errs) => (Invalid(errs), Invalid(errs))
      case Valid((Some(a), Some(b))) => (Valid(a), Valid(b))
      case Valid((_, _)) =>
        val invalid: ValidatedNel[PropertyParseError, Double] = Invalid(NonEmptyList(PropertyParseError("MPPT voltage range(V)"), Nil))
        (invalid, invalid)
    }

    Applicative[ValidatedNel[PropertyParseError, ?]].map7(
      get("MPPT number/Max input strings number")(s =>
        for {
          str <- s.split("/").headOption
          v <- Try(str.toInt).toOption
        } yield v)(data),
      getValue(data, "Start-up voltage(V)")(strToDouble),
      minMpptVoltage,
      maxMpptVoltage,
      getValue(data, "Max. DC input voltage(V)")(strToDouble),
      getValue(data, "Max. input current(A)") { s: String =>
        strToDouble(s.replaceAll("[a-zA-Z]", ""))
      },
      getValue(data, "Max. DC input power(kW)")(strToDouble andThen fromKilos)
    )(InputData)
  }

  implicit val rawDataToOFrequencyData: Map[String, String] => ValidatedNel[PropertyParseError, FrequencyData] = data =>
    Applicative[ValidatedNel[PropertyParseError, ?]].map3(
      getValue(data, "Frequency Hz")(strToDouble),
      getValue(data, "min Frequency Hz")(strToDouble),
      getValue(data, "max Frequency Hz")(strToDouble)
    )(FrequencyData)

  implicit val rawDataToOutputData: Map[String, String] => ValidatedNel[PropertyParseError, OutputData] = data => {
    val (minAcVoltage, maxAcVoltage) = getRangeOfDoubles("Grid voltage range(V)")(data) match {
      case Valid((Some(a), Some(b))) => (Valid(a), Valid(b))
      case Valid((_, _)) =>
        val invalid: ValidatedNel[PropertyParseError, Double] = Invalid(NonEmptyList(PropertyParseError("Grid voltage range(V)"), Nil))
        (invalid, invalid)
      case Invalid(err) => (Invalid(err), Invalid(err))
    }
    Applicative[ValidatedNel[PropertyParseError, ?]].map8(
      getValue(data, "Max. apparent output power(kVA)")(strToDouble andThen fromKilos),
      getValue(data, "Max. output current(A)")(strToDouble),
      getValue(data, "THDi (at rated output power)") {
        s: String => strToDouble(s.replaceAll("[<>%]", ""))
      },
      getValue(data, "Power Factor (at rated output power)")(s => Some(s)),
      minAcVoltage,
      maxAcVoltage,
      getValue(data, "Rated output power(kW)")(strToDouble andThen fromKilos),
      getValue(data, "Rated grid voltage(V)")(strToDouble)
    )(OutputData)
  }

  implicit val rawDataToEfficiency: Map[String, String] => ValidatedNel[PropertyParseError, Efficiency] = data =>
    Applicative[ValidatedNel[PropertyParseError, ?]].map2(
      getValue(data, "Max.efficiency") { s =>
        strToDouble(s.replace("%", "").trim)
      },
      getValue(data, "EU efficiency") { s =>
        strToDouble(s.replace("%", "").trim)
      }
    )(Efficiency)

  implicit val rawDataToGeneralData: Map[String, String] => ValidatedNel[PropertyParseError, GeneralData] = data => {

    val (w, h, d) = getValue(data, "Dimensins(mm)") {
      s =>
        s.replaceAll("[a-zA-Z()]", "")
          .trim
          .split("\\*").map(_.trim).toList.map(strToDouble) match {
          case Some(a) :: Some(b) :: Some(c) :: _ => Some(a, b, c)
          case _ => None
        }
    } match {
      case Valid((a, b, c)) => (Valid(a), Valid(b), Valid(c))
      case Invalid(errs) => (Invalid(errs), Invalid(errs), Invalid(errs))
    }

    val (minT, maxT) = getValue(data, "Operating ambient temperature range") {
      s =>
        s.replace("℃", "")
          .trim
          .split("~").map(_.trim).toList.map(strToDouble) match {
          case Some(min) :: Some(max) :: _ => Some(min, max)
          case _ => None
        }
    } match {
      case Valid((a, b)) => (Valid(a), Valid(b))
      case Invalid(errs) => (Invalid(errs), Invalid(errs))
    }

    val maxHumidity = getValue(data, "Relative humidity") {
      _.replace("%", "")
        .split("~").map(_.trim).toList.map(strToDouble) match {
        case Some(_) :: Some(max) :: _ => Some(max)
        case _ => None
      }
    }

    Applicative[ValidatedNel[PropertyParseError, ?]].map11(
      w,
      h,
      d,
      getValue(data, "Weight(kg)")(strToDouble),
      getValue(data, "Self consumption (night)") {
        s: String => strToDouble(s.replaceAll("[<>()a-zA-Z]", ""))
      },
      getValue(data, "Topology")(s => Some(s)),
      getValue(data, "Cooling concept")(s => Some(s)),
      minT,
      maxT,
      maxHumidity,
      getValue(data, "Ingress protection")(s => Some(s))
    )(GeneralData)
  }


}
