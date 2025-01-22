package me.katze.gui4s.example
package impl

import task.TaskSet

import me.katze.gui4s.widget.stateful.Path

enum RecompositionAction[Task]:
  case Task(task: Task)
  case KillTasksFor(path: Path)
end RecompositionAction

def runRecompositionActionInTaskSet[F[_], Task](taskSet: TaskSet[F, Task], recompositionAction: RecompositionAction[Task]) : F[Unit] =
  recompositionAction match
    case RecompositionAction.Task(task) => taskSet.pushTask(task)
    case RecompositionAction.KillTasksFor(path) => taskSet.killTasksFor(path)
  end match
end runRecompositionActionInTaskSet