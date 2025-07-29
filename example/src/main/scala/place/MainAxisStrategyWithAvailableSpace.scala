package me.katze.gui4s.example
package place

final case class ElementPlacementInInfiniteContainerAttemptError[Error](
                                                                          withSpaceBetweenStrategy : Error,
                                                                          withSpaceAroundStrategy : Error,
                                                                          withCenterStrategy : Error,
                                                                          withEndStrategy : Error,
)

val ENErrors = ElementPlacementInInfiniteContainerAttemptError(
  withCenterStrategy = "Tried to place elements in layout with Center mode. It requires container to be finite but infinite container found. You have tried to place something in the middle of infinity xD",
  withEndStrategy = "Tried to place elements in layout with End mode. It requires container to be finite but infinite container found. You have tried to place something in the end of infinity xD",
  withSpaceAroundStrategy = "Tried to place elements in layout with SpaceAround mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space around them xD",
  withSpaceBetweenStrategy = "Tried to place elements in layout with SpaceBetween mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space between them xD",
)