package catnip
package cats.effect

import _root_.cats.effect.*

type BiAsync[F[_, _]] = Bi[Async][F]
type BiConcurrent[F[_, _]] = Bi[Concurrent][F]
