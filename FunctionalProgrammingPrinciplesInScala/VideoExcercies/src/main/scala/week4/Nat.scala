package week4

abstract class Nat {
  def isZero: scala.Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  override def isZero = true;

  override def predecessor = throw new NoSuchElementException

  override def +(that: Nat) = that

  override def -(that: Nat) = if (that.isZero) Zero else throw new NoSuchElementException
}

class Succ(n: Nat) extends Nat {
  override def isZero = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}
