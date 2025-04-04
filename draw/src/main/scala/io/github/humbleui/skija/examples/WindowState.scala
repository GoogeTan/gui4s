package io.github.humbleui.skija.examples


final case class WindowState(
                              handle : Long,
                              width : Int,
                              height : Int,
                              dpi : Float,
                              mouse : Mouse,
                              vsyncEnabled : Float
                            )
