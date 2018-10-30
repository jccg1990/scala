object test {
  def testAssoc: Double = {
    val r = new scala.util.Random
    val lst = List.fill(400)(r.nextDouble * 0.002)
    err(lst)
  }

  def err(lst: List[Double]): Double =
    lst.reduceLeft(f) - lst.reduceRight(f)

  def f(u: Double, v: Double): Double =
    (u + v) / (1.0 + u * v)

  testAssoc
}