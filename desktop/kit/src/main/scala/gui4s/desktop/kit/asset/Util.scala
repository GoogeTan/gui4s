package gui4s.desktop.kit.asset

import java.io.InputStream

import cats.effect.Sync

def readStreamBytes[F[_] : Sync as S](stream: InputStream): F[Array[Byte]] =
  S.blocking:
    stream.readAllBytes()
end readStreamBytes