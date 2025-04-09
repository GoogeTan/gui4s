package me.katze.gui4s.widget

@SuppressWarnings(Array("org.wartremover.warts.Any"))
final case class StateTree[+Dealloc](
  state: Any,
  leftComposition: Dealloc,
  childrenStates: Map[String, StateTree[Dealloc]]
)

def foldLeftComposition[Dealloc](
                                  oldTree: Map[String, StateTree[Dealloc]], 
                                  newTree: Map[String, StateTree[Dealloc]]
                                ): List[Dealloc] =
  oldTree.flatMap(
    (key, tree) =>
      newTree.get(key) match
        case Some(value) => foldLeftComposition(tree.childrenStates, value.childrenStates)
        case None => List(tree.leftComposition)
      end match
  ).toList
end foldLeftComposition
