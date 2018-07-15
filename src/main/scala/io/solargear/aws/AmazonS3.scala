package io.solargear.aws

import java.nio.file.Path

import cats.effect.Sync

trait AmazonS3[F[_]] {

  def getFile(path: Path)(implicit S: Sync[F]): F[Map[String, String]]
}
