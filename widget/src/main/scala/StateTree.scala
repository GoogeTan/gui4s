package me.katze.gui4s.widget

@SuppressWarnings(Array("org.wartremover.warts.Any"))
final case class StateTree[+QuitRecompositionReaction](
                                                        state: Any,
                                                        quitCompositionReaction: QuitRecompositionReaction,
                                                        childrenStates: Map[String, StateTree[QuitRecompositionReaction]]
                                                        )

def collectQuitCompositionReactions[Reaction](
                                              oldTree: Map[String, StateTree[Reaction]],
                                              newTree: Map[String, StateTree[Reaction]]
                                              ): List[Reaction] =
  oldTree.flatMap(
    (key, tree) =>
      newTree.get(key) match
        case Some(value) => collectQuitCompositionReactions(tree.childrenStates, value.childrenStates)
        case None => List(tree.quitCompositionReaction)
      end match
  ).toList
end collectQuitCompositionReactions
