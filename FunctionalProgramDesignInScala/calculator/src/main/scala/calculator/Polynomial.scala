package calculator

object Polynomial {
  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def roots(x: Double) = {
      (-b() + (x * math.sqrt(delta()))) / (2 * a())
    }

    def res: Set[Double] = {
      if (delta() >= 0) {
        Set(1, -1).map(roots(_))
      } else {
        Set()
      }
    }

    Signal(res)
  }

  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal((b() * b()) - (4 * a() * c()))
  }
}
