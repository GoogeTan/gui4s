package me.katze.gui4s.widget
package stateful

trait CatchEvents[Update[+_, +_]]:
  extension[W, E](old : Update[W, E])
    def catchEvents : Update[(W, List[E]), Nothing]
  end extension
end CatchEvents
