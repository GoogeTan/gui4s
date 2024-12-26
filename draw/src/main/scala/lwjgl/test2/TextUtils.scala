package me.katze.gui4s.draw
package lwjgl.test2

import java.util.regex.Pattern

def linesOfText(text : String) : Int =
  var lc = 0
  val m = Pattern.compile("^.*$", Pattern.MULTILINE).matcher(text)
  while m.find do
    lc += 1
  end while
  lc
end linesOfText