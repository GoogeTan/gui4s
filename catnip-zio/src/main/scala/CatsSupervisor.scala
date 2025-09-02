package catnip.zio

import zio.{RIO, Scope, Task}
import zio.interop.catz.*

def CatsSupervisor : RIO[Scope, cats.effect.std.Supervisor[Task]] =
  resourceToScoped(cats.effect.std.Supervisor[Task])
end CatsSupervisor
