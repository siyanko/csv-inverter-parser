import java.nio.file.{Path, Paths}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
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

  private def failLambda(msg: FailMessage): IO[Unit] = IO {
    throw new RuntimeException(msg)
  }

  private def rawDataToSpec(implicit inputDataFrom: Map[String, String] => Valid[InputData],
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

  private def saveSpec(spec: InverterSpec, path: Path)(implicit amazonDDB: AmazonDDB[IO]): IO[Unit] = for {
    _ <- amazonDDB.save(path, spec.asJson)
    _ <- safePrint(s"DONE. Check ${path.toString}")
  } yield ()

  private def failedSpec(errs: NonEmptyList[PropertyParseError]): IO[Unit] = for {
    _ <- safePrint("Failed to parse data:")
    _ <- safePrint(errs.toList)
    _ <- failLambda("Failed to process spec")
  } yield ()


  private def processCsvFile(path: Path)(implicit inputDataFrom: Map[String, String] => Valid[InputData],
                                         outputDataFrom: Map[String, String] => Valid[OutputData],
                                         efficiencyFrom: Map[String, String] => Valid[Efficiency],
                                         generalDataFrom: Map[String, String] => Valid[GeneralData],
                                         amazonS3: AmazonS3[IO]): IO[Unit] = for {
    validatedSpec <- amazonS3.getFile(path).map(rawDataToSpec)
    _ <- validatedSpec match {
      case Valid(spec) =>
        val specPath: Path = Paths.get(path.toString.replace(".csv", ".json"))
        saveSpec(spec, specPath)

      case Invalid(errs) => failedSpec(errs)
    }
  } yield ()

  private def validate(path: String): IO[ValidatedNel[String, Path]] = IO {
    Validated.condNel(path.endsWith(".csv"), Paths.get(path), "Not a csv file: " + path)
  }

  private def safePrint(s: String): IO[Unit] = IO {
    println(s)
  }

  private def safePrint[A](ls: List[A]): IO[Unit] = IO {
    ls.foreach(println)
  }

  def program(path: String)(implicit inputDataFrom: Map[String, String] => Valid[InputData],
                            outputDataFrom: Map[String, String] => Valid[OutputData],
                            efficiencyFrom: Map[String, String] => Valid[Efficiency],
                            generalDataFrom: Map[String, String] => Valid[GeneralData]): IO[Unit] = for {
    pathValidationResult <- validate(path)
    _ <- pathValidationResult match {
      case Valid(path) => processCsvFile(path)
      case Invalid(err) => safePrint(err.toList)
    }
  } yield ()

}
