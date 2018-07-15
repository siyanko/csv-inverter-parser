package io

import cats.data.ValidatedNel
import cats.implicits._

import io.solargear.csv.Key

package object solargear {

  type PropertyName = String

  final case class PropertyParseError(name: PropertyName) {
    override def toString: String = s"[$name] - Could not be parsed"
  }

  def getValue[A](data: Map[String, String], key: String)(f: String => Option[A]): ValidatedNel[PropertyParseError, A] = {
    val dataMap: Map[Key, String] = data.map {
      case (k, v) => (csv.convertKey(k), v)
    }

    val result: Option[A] = for {
      strV <- dataMap.get(csv.convertKey(key))
      v <- f(strV)
    } yield v

    result.toValidNel(PropertyParseError(key))
  }


  case class InputData(
                        mpptNumber: Int,
                        startUpVoltage: Double,
                        minMpptVoltage: Double,
                        maxMpptVoltage: Double,
                        maxInputVoltage: Double,
                        maxInputCurrent: Double,
                        maxGeneratorPower: Double
                      )

  case class FrequencyData(
                            frequency: Double,
                            minFrequency: Double,
                            maxFrequency: Double
                          )

  // TODO include frequencyOptions
  case class OutputData(
                         maxApperentOutputPower: Double,
                         maxOutputCurrent: Double,
                         totalHarmonicDistortion: Double,
                         powerFactor: String,
                         minACVoltage: Double,
                         maxACVoltage: Double,
                         //                         frequencyOptions: List[FrequencyData],
                         ratedOutputPower: Double,
                         nominalACVoltage: Double
                       )

  case class Efficiency(
                         maxEfficiency: Double,
                         euEfficiency: Double
                       )

  case class GeneralData(
                          width: Double,
                          height: Double,
                          depth: Double,
                          weight: Double,
                          nightTimeConsumption: Double,
                          topology: String,
                          cooling: String,
                          minOperatingTemperature: Double,
                          maxOperatingTemperature: Double,
                          maxPermittedHumidity: Double,
                          protectionDegree: String
                        )

  // TODO include make and model
  case class InverterSpec(
                           inputData: InputData,
                           outputData: OutputData,
                           efficiency: Efficiency,
                           generalData: GeneralData
                         )

}
