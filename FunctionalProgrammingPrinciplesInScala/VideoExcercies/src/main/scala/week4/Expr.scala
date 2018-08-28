package week4

trait Expr {
  def isNumber: scala.Boolean

  def isSum: scala.Boolean

  def numValue: Int

  def leftOP: Expr

  def rightOp: Expr
}

class Number(n: Int) extends Expr {
  override def isNumber: scala.Boolean = true

  override def isSum: scala.Boolean = false

  override def numValue: Int = n

  override def leftOP: Expr = throw new Error("Number.leftOp")

  override def rightOp: Expr = throw new Error("Number.rightOp")
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  override def isNumber: scala.Boolean = false

  override def isSum: scala.Boolean = true

  override def numValue: Int = throw new Error("Sum.numValue")

  override def leftOP: Expr = e1

  override def rightOp: Expr = e2
}

