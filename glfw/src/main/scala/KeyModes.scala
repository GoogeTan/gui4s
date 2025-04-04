package me.katze.gui4s.glfw

/**
 *
 * @param shiftActive True if one or more Shift keys were held down
 * @param controlActive True if one or more Control keys were held down.
 * @param altActive True if one or more Alt keys were held down.
 * @param superActive True if one or more Super keys were held down.
 * @param capsLockActive True if the Caps Lock key is enabled.
 * @param numLockActive True if the Num Lock key is enabled.
 */
final case class KeyModes(
                            shiftActive : Boolean,
                            controlActive : Boolean,
                            altActive : Boolean,
                            superActive : Boolean,
                            capsLockActive : Boolean,
                            numLockActive : Boolean
                          )

object KeyModes:
  def fromMask(mask : Int) : KeyModes =
    KeyModes(
      (mask & 0x0001) != 0,
      (mask & 0x0002) != 0,
      (mask & 0x0004) != 0,
      (mask & 0x0008) != 0,
      (mask & 0x0010) != 0,
      (mask & 0x0020) != 0,
    )
  end fromMask
end KeyModes
