package gui4s.desktop.skija

import catnip.resource.SyncResource
import io.github.humbleui.skija.Data

def makeDataFromBytes[Resource[_] : SyncResource as S](bytes : Array[Byte]) : Resource[Data] =
  S.fromAutoCloseable(Data.makeFromBytes(bytes))
end makeDataFromBytes