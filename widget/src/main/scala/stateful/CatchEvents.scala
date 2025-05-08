package me.katze.gui4s.widget
package stateful

trait CatchEvents[Update[+_, +_]]:
  extension[Value, Event](old : Update[Value, Event])
    def catchEvents : Update[(Value, List[Event]), Nothing]
  end extension
end CatchEvents
