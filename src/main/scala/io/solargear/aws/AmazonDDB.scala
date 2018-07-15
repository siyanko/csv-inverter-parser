package io.solargear.aws

import java.nio.file.Path

import cats.effect.Sync
import io.circe.Json

trait AmazonDDB[F[_]] {
  def save(path: Path, data: Json)(implicit S: Sync[F]): F[Unit]
}
