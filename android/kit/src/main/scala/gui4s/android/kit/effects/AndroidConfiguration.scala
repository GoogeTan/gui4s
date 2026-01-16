package gui4s.android.kit.effects

import android.os.{LocaleList, Parcel, Parcelable}
import android.text.TextUtils
import android.util.DisplayMetrics
import android.view.View

import java.lang.annotation.{Retention, RetentionPolicy}
import java.util.Locale
import android.content.res.Configuration
import gui4s.android.kit.effects.AndroidConfiguration.UiModeType
import gui4s.android.kit.effects.AndroidConfiguration.LayoutDirection
import gui4s.android.kit.effects.AndroidConfiguration.ScreenLong
import gui4s.android.kit.effects.AndroidConfiguration.ScreenSize
import gui4s.android.kit.effects.AndroidConfiguration.Hdr
import gui4s.android.kit.effects.AndroidConfiguration.UiModeNight
import gui4s.android.kit.effects.AndroidConfiguration.WideColorGamut
import gui4s.android.kit.effects.AndroidConfiguration.GrammaticalGender
import gui4s.android.kit.effects.AndroidConfiguration.Touchscreen
import gui4s.android.kit.effects.AndroidConfiguration.Keyboard
import gui4s.android.kit.effects.AndroidConfiguration.KeyboardHidden
import gui4s.android.kit.effects.AndroidConfiguration.HardKeyboardHidden
import gui4s.android.kit.effects.AndroidConfiguration.Navigation
import gui4s.android.kit.effects.AndroidConfiguration.NavigationHidden
import gui4s.android.kit.effects.AndroidConfiguration.ScreenRound
import gui4s.core.geometry.InfinityOr

object AndroidConfiguration {
  enum GrammaticalGender(val id: Int):
    case NotSpecified extends GrammaticalGender(0)
    case Neutral extends GrammaticalGender(1)
    case Feminine extends GrammaticalGender(2)
    case Masculine extends GrammaticalGender(3)
  end GrammaticalGender

  enum WideColorGamut(val id: Int):
    case Undefined extends WideColorGamut(0)
    case No extends WideColorGamut(1)
    case Yes extends WideColorGamut(2)
  end WideColorGamut

  enum Hdr(val id: Int):
    case Undefined extends Hdr(0)
    case No extends Hdr(1)
    case Yes extends Hdr(2)
  end Hdr

  enum ScreenSize(val id: Int) extends Comparable[ScreenSize]:
    case Undefined extends ScreenSize(0)
    case Small extends ScreenSize(1)
    case Normal extends ScreenSize(2)
    case Large extends ScreenSize(3)
    case Xlarge extends ScreenSize(4)

    def compareTo(other: ScreenSize): Int = id - other.id
  end ScreenSize

  enum ScreenLong(val id: Int):
    case Undefined extends ScreenLong(0)
    case No extends ScreenLong(1)
    case Yes extends ScreenLong(2)
  end ScreenLong

  enum LayoutDirection(val id: Int):
    case Undefined extends LayoutDirection(0)
    case Ltr extends LayoutDirection(1)
    case Rtl extends LayoutDirection(2)
  end LayoutDirection

  enum ScreenRound(val id: Int):
    case Undefined extends ScreenRound(0)
    case No extends ScreenRound(1)
    case Yes extends ScreenRound(2)
  end ScreenRound

  enum HardKeyboardHidden(val id: Int):
    case Undefined extends HardKeyboardHidden(0)
    case No extends HardKeyboardHidden(1)
    case Yes extends HardKeyboardHidden(2)
  end HardKeyboardHidden

  enum Keyboard(val id: Int):
    case Undefined extends Keyboard(0)
    case NoKeys extends Keyboard(1)
    case Qwerty extends Keyboard(2)
    case TwelveKey extends Keyboard(3)
  end Keyboard

  enum KeyboardHidden(val id: Int):
    case Undefined extends KeyboardHidden(0)
    case No extends KeyboardHidden(1)
    case Yes extends KeyboardHidden(2)
  end KeyboardHidden

  enum Navigation(val id: Int):
    case Undefined extends Navigation(0)
    case NoNav extends Navigation(1)
    case Dpad extends Navigation(2)
    case Trackball extends Navigation(3)
    case Wheel extends Navigation(4)
  end Navigation

  enum NavigationHidden(val id: Int):
    case Undefined extends NavigationHidden(0)
    case No extends NavigationHidden(1)
    case Yes extends NavigationHidden(2)
  end NavigationHidden

  enum Orientation(val id: Int):
    case Undefined extends Orientation(0)
    case Portrait extends Orientation(1)
    case Landscape extends Orientation(2)
    @Deprecated
    case Square extends Orientation(3)
  end Orientation

  enum Touchscreen(val id: Int):
    case Undefined extends Touchscreen(0)
    case NoTouch extends Touchscreen(1)
    @Deprecated
    case Stylus extends Touchscreen(2)
    case Finger extends Touchscreen(3)
  end Touchscreen


  enum UiModeType(val id: Int) {
    case Undefined extends UiModeType(0)
    case Normal extends UiModeType(1)
    case Desk extends UiModeType(2)
    case Car extends UiModeType(3)
    case Television extends UiModeType(4)
    case Appliance extends UiModeType(5)
    case Watch extends UiModeType(6)
    case VrHeadset extends UiModeType(7)
  }

  enum UiModeNight(val id: Int) {
    case Undefined extends UiModeNight(0)
    case No extends UiModeNight(1)
    case Yes extends UiModeNight(2)
  }

  def fromJava(jc: Configuration): AndroidConfiguration[Bounds] = {
    val screenLayout = jc.screenLayout
    val uiMode = jc.uiMode
    val colorMode = jc.colorMode

    val screenSize = ScreenSize.values.find(_.id == (screenLayout & 0x0f)).getOrElse(ScreenSize.Undefined)
    val screenLong = ScreenLong.values.find(_.id == ((screenLayout & 0x30) >> 4)).getOrElse(ScreenLong.Undefined)
    val layoutDirection = LayoutDirection.values.find(_.id == ((screenLayout & 0xc0) >> 6)).getOrElse(LayoutDirection.Undefined)
    val screenRound = ScreenRound.values.find(_.id == ((screenLayout & 0x300) >> 8)).getOrElse(ScreenRound.Undefined)
    val uiModeType = UiModeType.values.find(_.id == (uiMode & 0x0f)).getOrElse(UiModeType.Undefined)
    val uiModeNight = UiModeNight.values.find(_.id == ((uiMode & 0x30) >> 4)).getOrElse(UiModeNight.Undefined)
    val wideColorGamut = WideColorGamut.values.find(_.id == (colorMode & 0x3)).getOrElse(WideColorGamut.Undefined)
    val hdr = Hdr.values.find(_.id == ((colorMode & 0xc) >> 2)).getOrElse(Hdr.Undefined)
    val grammaticalGender = GrammaticalGender.values.find(_.id == jc.getGrammaticalGender).getOrElse(GrammaticalGender.NotSpecified)
    val touchscreen = Touchscreen.values.find(_.id == jc.touchscreen).getOrElse(Touchscreen.Undefined)
    val keyboard = Keyboard.values.find(_.id == jc.keyboard).getOrElse(Keyboard.Undefined)
    val keyboardHidden = KeyboardHidden.values.find(_.id == jc.keyboardHidden).getOrElse(KeyboardHidden.Undefined)
    val hardKeyboardHidden = HardKeyboardHidden.values.find(_.id == jc.hardKeyboardHidden).getOrElse(HardKeyboardHidden.Undefined)
    val navigation = Navigation.values.find(_.id == jc.navigation).getOrElse(Navigation.Undefined)
    val navigationHidden = NavigationHidden.values.find(_.id == jc.navigationHidden).getOrElse(NavigationHidden.Undefined)
    val orientation = AndroidConfiguration.Orientation.values.find(_.id == jc.orientation).getOrElse(AndroidConfiguration.Orientation.Undefined)

    AndroidConfiguration(
      fontScale = jc.fontScale,
      mcc = jc.mcc,
      mnc = jc.mnc,
      localeList = jc.getLocales,
      grammaticalGender = grammaticalGender,
      touchscreen = touchscreen,
      keyboard = keyboard,
      keyboardHidden = keyboardHidden,
      hardKeyboardHidden = hardKeyboardHidden,
      navigation = navigation,
      navigationHidden = navigationHidden,
      orientation = orientation,
      screenSize = screenSize,
      screenLong = screenLong,
      layoutDirection = layoutDirection,
      screenRound = screenRound,
      uiModeType = uiModeType,
      uiModeNight = uiModeNight,
      bounds = Bounds(jc.screenWidthDp.toFloat, jc.screenHeightDp.toFloat),
      smallestScreenWidthDp = jc.smallestScreenWidthDp,
      densityDpi = jc.densityDpi,
      wideColorGamut = wideColorGamut,
      hdr = hdr,
      fontWeightAdjustment = jc.fontWeightAdjustment,
    )
  }
}


case class AndroidConfiguration[Bounds](
  fontScale: Float = 0.0f,
  mcc: Int = 0,
  mnc: Int = Configuration.MNC_ZERO,
  localeList: LocaleList = LocaleList.getEmptyLocaleList,
  grammaticalGender: AndroidConfiguration.GrammaticalGender = AndroidConfiguration.GrammaticalGender.NotSpecified,
  touchscreen: AndroidConfiguration.Touchscreen = AndroidConfiguration.Touchscreen.Undefined,
  keyboard: AndroidConfiguration.Keyboard = AndroidConfiguration.Keyboard.Undefined,
  keyboardHidden: AndroidConfiguration.KeyboardHidden = AndroidConfiguration.KeyboardHidden.Undefined,
  hardKeyboardHidden: AndroidConfiguration.HardKeyboardHidden = AndroidConfiguration.HardKeyboardHidden.Undefined,
  navigation: AndroidConfiguration.Navigation = AndroidConfiguration.Navigation.Undefined,
  navigationHidden: AndroidConfiguration.NavigationHidden = AndroidConfiguration.NavigationHidden.Undefined,
  orientation: AndroidConfiguration.Orientation = AndroidConfiguration.Orientation.Undefined,
  screenSize: AndroidConfiguration.ScreenSize = AndroidConfiguration.ScreenSize.Undefined,
  screenLong: AndroidConfiguration.ScreenLong = AndroidConfiguration.ScreenLong.Undefined,
  layoutDirection: AndroidConfiguration.LayoutDirection = AndroidConfiguration.LayoutDirection.Undefined,
  screenRound: AndroidConfiguration.ScreenRound = AndroidConfiguration.ScreenRound.Undefined,
  uiModeType: AndroidConfiguration.UiModeType = AndroidConfiguration.UiModeType.Undefined,
  uiModeNight: AndroidConfiguration.UiModeNight = AndroidConfiguration.UiModeNight.Undefined,
  bounds: Bounds,
  smallestScreenWidthDp: Int = Configuration.SMALLEST_SCREEN_WIDTH_DP_UNDEFINED,
  densityDpi: Int = Configuration.DENSITY_DPI_UNDEFINED,
  wideColorGamut: AndroidConfiguration.WideColorGamut = AndroidConfiguration.WideColorGamut.Undefined,
  hdr: AndroidConfiguration.Hdr = AndroidConfiguration.Hdr.Undefined,
  fontWeightAdjustment: Int = Configuration.FONT_WEIGHT_ADJUSTMENT_UNDEFINED,
):

  def isScreenRound: Boolean = screenRound == AndroidConfiguration.ScreenRound.Yes
  def isScreenWideColorGamut: Boolean = wideColorGamut == AndroidConfiguration.WideColorGamut.Yes
  def isScreenHdr: Boolean = hdr == AndroidConfiguration.Hdr.Yes
  def isNightModeActive: Boolean = uiModeNight == AndroidConfiguration.UiModeNight.Yes

  def isLayoutSizeAtLeast(size: AndroidConfiguration.ScreenSize): Boolean =
    screenSize.id >= size.id
  end isLayoutSizeAtLeast

  def withLayoutDirection(loc: Locale): AndroidConfiguration[Bounds] =
    val dir = if (loc != null && TextUtils.getLayoutDirectionFromLocale(loc) == View.LAYOUT_DIRECTION_RTL) AndroidConfiguration.LayoutDirection.Rtl else AndroidConfiguration.LayoutDirection.Ltr
    this.copy(layoutDirection = dir)
  end withLayoutDirection

  def withBounds(bounds: Bounds): AndroidConfiguration[Bounds] = this.copy(bounds = bounds)
  def withTransformedBounds[NewBounds](f: Bounds => NewBounds): AndroidConfiguration[NewBounds] =
     this.copy(bounds = f(bounds))
  end withTransformedBounds
end AndroidConfiguration
