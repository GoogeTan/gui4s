package me.katze.gui4s.glfw

final case class WindowCreationSettings(
                                          title : String, 
                                          size : Size, 
                                          visible : Boolean, 
                                          resizeable : Boolean, 
                                          debugContext : Boolean
                                        )
