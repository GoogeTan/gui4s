package gui4s.core.widget.library.animation

@FunctionalInterface
trait Easing:
  def transform(fraction: Double): Double
end Easing

object Easing:
  val Linear: Easing = identity

  def CubicBezier(p1x: Double, p1y: Double, p2x: Double, p2y: Double): Easing =
    def evaluateX(t: Double): Double =
      3 * (1 - t) * (1 - t) * t * p1x + 3 * (1 - t) * t * t * p2x + t * t * t
    end evaluateX

    def evaluateY(t: Double): Double =
      3 * (1 - t) * (1 - t) * t * p1y + 3 * (1 - t) * t * t * p2y + t * t * t
    end evaluateY

    def binarySearch(x: Double, f : Double => Double, low: Double, high : Double): Double =
      var llow = low
      var hhigh = high
      for _ <- 0 until 20 do
        val mid = (low + high) / 2
        val xMid = f(mid)
        if xMid < x then
            llow = mid
        else
            hhigh = mid
        end if
      end for
      (llow + hhigh) / 2
    end binarySearch

    (x: Double) =>
      if x <= 0 then
        0.0
      else if x >= 1 then
        1.0
      else
        evaluateY(binarySearch(x, evaluateX, 0.0, 1.0))
      end if
  end CubicBezier

  val FastOutSlowIn: Easing = CubicBezier(0.4, 0.0, 0.2, 1.0)
end Easing

