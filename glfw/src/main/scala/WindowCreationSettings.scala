package gui4s.glfw

import gui4s.core.geometry.Rect

final case class WindowCreationSettings[MeasurementUnit](
                                                          title : String, 
                                                          size : Rect[MeasurementUnit], 
                                                          visible : Boolean, 
                                                          resizeable : Boolean, 
                                                          debugContext : Boolean
                                                        )
