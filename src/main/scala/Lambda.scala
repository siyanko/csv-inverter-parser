import java.nio.file.{Path, Paths}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.effect.{IO, Sync}
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import io.solargear._
import io.solargear.aws.{AmazonDDB, AmazonS3}

//TODO add lambda support
object Lambda {

  type Valid[A] = ValidatedNel[PropertyParseError, A]
  type FailMessage = String

  // TODO implement s3 interaction
  implicit val AmazonS3IO: AmazonS3[IO] = new AmazonS3[IO] {
    override def getFile(path: Path)(implicit S: Sync[IO]): IO[Map[String, String]] =
      fs2.io.file.readAll[IO](path, 4096)
        .through(fs2.text.utf8Decode)
        .through(fs2.text.lines)
        .map(csv.lineToPair)
        .compile.toList
        .map(_.flatten.toMap)
  }

  // TODO implement DDB interaction
  implicit val AmazonDDBIO: AmazonDDB[IO] = new AmazonDDB[IO] {
    override def save(path: Path, data: Json)(implicit S: Sync[IO]): IO[Unit] =
      fs2.Stream(data.toString).covary[IO]
        .through(fs2.text.utf8Encode)
        .through(fs2.io.file.writeAll(path))
        .compile.drain
  }

  def failLambda(msg: FailMessage): IO[Unit] = IO {
    throw new RuntimeException(msg)
  }

  def rawDataToSpec(implicit inputDataFrom: Map[String, String] => Valid[InputData],
                    outputDataFrom: Map[String, String] => Valid[OutputData],
                    efficiencyFrom: Map[String, String] => Valid[Efficiency],
                    generalDataFrom: Map[String, String] => Valid[GeneralData]
                   ): Map[String, String] => Valid[InverterSpec] = data =>
    Applicative[Valid].map4(
      inputDataFrom(data),
      outputDataFrom(data),
      efficiencyFrom(data),
      generalDataFrom(data)
    )(InverterSpec)

  def program(path: String)(implicit inputDataFrom: Map[String, String] => Valid[InputData],
                            outputDataFrom: Map[String, String] => Valid[OutputData],
                            efficiencyFrom: Map[String, String] => Valid[Efficiency],
                            generalDataFrom: Map[String, String] => Valid[GeneralData],
                            amazonS3: AmazonS3[IO],
                            amazonDDB: AmazonDDB[IO]): IO[Unit] = for {
    validatedSpec <- amazonS3.getFile(Paths.get(path)).map(rawDataToSpec)
    _ <- validatedSpec match {
      case Valid(spec) =>
        val specPath: Path = Paths.get(path.replace(".csv", ".json"))
        for {
          _ <- amazonDDB.save(specPath, spec.asJson)
          _ <- IO {
            println(s"DONE. Check ${specPath.toString}")
          }
        } yield ()

      case Invalid(errs) => for {
        _ <- IO {
          println("Failed to parse data:")
          errs.toList.foreach(println)
        }
        _ <- failLambda("Failed to process spec")
      } yield ()

    }
  }

    yield ()

}
