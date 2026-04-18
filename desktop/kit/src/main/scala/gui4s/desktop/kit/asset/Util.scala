package gui4s.desktop.kit.asset

import cats.effect.Sync

import java.io.InputStream

def readStreamBytes[F[_] : Sync as S](stream: InputStream): F[Array[Byte]] =
  S.blocking:
    stream.readAllBytes()
end readStreamBytes