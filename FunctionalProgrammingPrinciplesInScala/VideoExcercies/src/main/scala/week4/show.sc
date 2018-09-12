import week4._

object show {
  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Var(s) => s
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Prod(Sum(x1, x2), x3) => "(" + show(x1) + " + " + show(x2) + ")" + " * " + show(x3)
    case Prod(e1, e2) => show(e1) + " * " + show(e2)
  }

  show(Sum(Number(1), Number(44)))
  show(Sum(Prod(Number(2), Var("x")), Var("y")))
  show(Prod(Sum(Number(2), Var("x")), Var("y")))

  def isort(xs: scala.List[Int]): scala.List[Int] = xs match {
    case scala.List() => scala.List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: scala.List[Int]): List[Int] = xs match {
    case scala.List() => x :: Nil
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }
}