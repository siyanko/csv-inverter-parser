object SolisLambda {

  import Lambda._
  import io.solargear.solis._

  def main(args: Array[String]): Unit = program("data3.csv").unsafeRunSync()

}
