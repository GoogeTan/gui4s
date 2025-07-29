package me.katze.gui4s.glfw

import me.katze.gui4s.geometry.Rect

final case class WindowCreationSettings[MeasurementUnit](
                                                          title : String, 
                                                          size : Rect[MeasurementUnit], 
                                                          visible : Boolean, 
                                                          resizeable : Boolean, 
                                                          debugContext : Boolean
                                                        )
