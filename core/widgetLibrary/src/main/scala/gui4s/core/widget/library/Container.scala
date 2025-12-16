package gui4s.core.widget.library

import gui4s.core.widget.handle.Layout

type ContainerWidget[PlacedWidget, Container[_], Place[_], Meta] =
  (children : Container[Place[PlacedWidget]], layout : Layout[Place, Container, PlacedWidget, Meta]) => Place[PlacedWidget]
