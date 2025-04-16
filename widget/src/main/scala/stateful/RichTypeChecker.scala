package me.katze.gui4s.widget
package stateful

trait RichTypeChecker[+M]:
  def tryCast(value: Any) : Option[M]
end RichTypeChecker

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
def panic(text : String) : Nothing =
  throw Exception(text)
end panic

given eitherRTC[A : RichTypeChecker as ARTC, B: RichTypeChecker as BRTC]: RichTypeChecker[Either[A, B]] =
  (possibleValue : Any) => ARTC.tryCast(possibleValue).map(Left(_)).orElse(BRTC.tryCast(possibleValue).map(Right(_)))
end eitherRTC

@SuppressWarnings(Array("org.wartremover.warts.Any"))
given tupleRTC[A : RichTypeChecker as ARTC, B: RichTypeChecker as BRTC]: RichTypeChecker[(A, B)] = 
  {
    case (possibleA : Any, possibleB : Any) =>
      ARTC.tryCast(possibleA).zip(BRTC.tryCast(possibleB))
    case _ => None
  }
end tupleRTC
