package gui4s.desktop.windowing.jwm

import io.github.humbleui.jwm.KeyModifier

opaque type Modifiers = Int

object Modifiers:
  def apply(value : Int) : Modifiers = value

extension (value : Modifiers)
  def isDown(modifier : KeyModifier) : Boolean =
    (value & modifier._mask) != 0
  end isDown

  def capsLockDown : Boolean =
    isDown(KeyModifier.CAPS_LOCK)
  def shiftDown : Boolean =
    isDown(KeyModifier.SHIFT)
  def controlDown : Boolean =
    isDown(KeyModifier.CONTROL)
  def altDown : Boolean =
    isDown(KeyModifier.ALT)
  def winLogoDown : Boolean =
    isDown(KeyModifier.WIN_LOGO)
  def linuxMetaDown : Boolean =
    isDown(KeyModifier.LINUX_META)
  def linuxSuperDown : Boolean =
    isDown(KeyModifier.LINUX_SUPER)
  def macCommandDown : Boolean =
    isDown(KeyModifier.MAC_COMMAND)
  def macOptionDown : Boolean =
    isDown(KeyModifier.MAC_OPTION)
  def macFnDown : Boolean =
    isDown(KeyModifier.MAC_FN)
end extension
