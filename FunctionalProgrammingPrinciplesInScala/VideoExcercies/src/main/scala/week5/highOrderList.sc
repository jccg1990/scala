object highOrderList {
  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")

  def squareList1(xs: List[Int]): List[Int] =
    xs match {
      case Nil => xs
      case y :: ys => (y * y) :: squareList1(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
  }

  def posElems2(xs: List[Int]): List[Int] = xs filter (x => x > 0)

  nums filter (x => x > 0)
  nums filterNot (x => x > 0)
  nums partition (x => x > 0)

  nums takeWhile (x => x > 0)
  nums dropWhile (x => x > 0)
  nums span (x => x > 0)

  pack(List("a", "a", "a", "b", "c", "c", "a"))

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: _ =>
      val (first, rest) = xs span (y => x == y)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map (ys => (ys.head, ys.length))

  encode(List("a", "a", "a", "b", "c", "c", "a"))
}