package week4

trait ListA[+T] {
  def isEmpty: scala.Boolean

  def head: T

  def tail: ListA[T]

  def prepend [U >: T](elem: U): ListA[U] = new ConsA(elem, this)
}

class ConsA[T](val head: T, val tail: ListA[T]) extends ListA[T] {
  override def isEmpty: scala.Boolean = false
}

object NilA extends ListA[Nothing] {
  override def isEmpty: scala.Boolean = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: Nothing = throw new NoSuchElementException("Ni.tail")
}

object test {
  val x: ListA[String] = NilA;

}
