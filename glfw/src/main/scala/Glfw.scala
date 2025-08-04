package me.katze.gui4s.glfw

import cats.effect.Resource
import org.lwjgl.glfw.GLFWErrorCallback

trait Glfw[F[_], Monitor, Window]:
  def createPrintErrorCallback : Resource[F, GLFWErrorCallback]
  def createWindow(windowCreationSettings: WindowCreationSettings[Float]) : Resource[F, Window]
  def primaryMonitor : F[Monitor]
  def monitorScale(monitor : Monitor) : F[Float]
  def primaryMonitorScale : F[Float]
  def swapInterval(interval : Int) : F[Unit]
  def pollEvents : F[Unit]
end Glfw

