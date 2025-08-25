package gui4s.desktop.kit.cats

// TODO Переименовать поля
final case class ContainerPlacementError[Error](
  withSpaceBetweenStrategy : Error,
  withSpaceAroundStrategy : Error,
  withCenterStrategy : Error,
  withEndStrategy : Error,
)

object ContainerPlacementError:
  val English: ContainerPlacementError[String] = ContainerPlacementError(
    withCenterStrategy = "Tried to place elements in layout with Center mode. It requires container to be finite but infinite container found. You have tried to place something in the middle of infinity xD",
    withEndStrategy = "Tried to place elements in layout with End mode. It requires container to be finite but infinite container found. You have tried to place something in the end of infinity xD",
    withSpaceAroundStrategy = "Tried to place elements in layout with SpaceAround mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space around them xD",
    withSpaceBetweenStrategy = "Tried to place elements in layout with SpaceBetween mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space between them xD",
  )
  end English
  
  val Russian : ContainerPlacementError[String] = ContainerPlacementError(
    withCenterStrategy = "Произошла попытка поставить элементы в центре контейнера. Чтобы это произошло, контейнер должен быть конечного размера, а он оказался бесконечным. Друг, ты попытался поставить что-то по середине бесконечности xD",
    withEndStrategy = "Произошла попытка поставить элементы в конце контейнера. Чтобы это произошло, контейнер должен быть конечного размера, а он оказался бесконечным. Друг, ты попытался поставить что-то в конце бесконечности xD",
    withSpaceAroundStrategy = "Произошла попытка поставить элементы равномерно(SpaceAround) внутри контейнера. Чтобы это произошло, контейнер должен быть конечного размера, а он оказался бесконечным. Друг, ты попытался покрыть конечным числом виджетов бесконечность xD",
    withSpaceBetweenStrategy = "Произошла попытка поставить элементы в равномерно(SpaceBetween) контейнера. Чтобы это произошло, контейнер должен быть конечного размера, а он оказался бесконечным.  Друг, ты попытался покрыть конечным числом виджетов бесконечность xD",
  )
  end Russian
end ContainerPlacementError
