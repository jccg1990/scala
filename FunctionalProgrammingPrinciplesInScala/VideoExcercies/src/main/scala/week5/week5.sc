object week5 {
  val test = List("a", "a", "a", "b", "c", "c", "a")

  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => Nil
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case Nil => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def concat2[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys) (_ :: _)


  removeAt(5, List('a', 'b', 'c', 'd')) // List(a, c, d)

  def reverse[T](xs: List[T]): List[T] = xs match {
    case Nil => xs
    case y :: ys => reverse(ys) ++ List(y)
  }

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))

  def removeAt[T](n: Int, xs: List[T]) = xs.take(n) ++ xs.drop(n + 1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case Nil => Nil
    case (y: List[Any]) :: ys => flatten(y) ++ flatten(ys)
    case y :: ys => y :: flatten(ys)
  }

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sum(ys)
  }

  def sum2(xs: List[Int]): Int = (0 :: xs) reduceLeft (_ + _)

  def sum3(xs: List[Int]): Int = (xs foldLeft 0) (_ + _)

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]()) (f(_) :: _)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0) ((_, y2) => y2 + 1)

  mapFun(test, x => "test" + x)

  lengthFun(test)

}