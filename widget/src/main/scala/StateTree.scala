package me.katze.gui4s.widget

trait StateTree[Dealloc]:
  def state: Any

  def leftComposition: Dealloc

  def childrenStates: Map[String, StateTree[Dealloc]]
end StateTree

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
