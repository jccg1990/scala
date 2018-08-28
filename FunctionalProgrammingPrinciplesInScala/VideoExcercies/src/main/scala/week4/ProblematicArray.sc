object ProblematicArray{

  val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
  val b: Array[IntSet] = a
  b(0) = E


  abstract class IntSet {
    def incl(x: Int): IntSet

    def contains(x: Int): Boolean

    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    override def incl(x: Int) = new NonEmpty(x, Empty, Empty)

    override def contains(x: Int) = false

    override def toString: String = "."

    override def union(other: IntSet) = other
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    override def incl(x: Int) =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    override def contains(x: Int) =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    override def toString: String = "{" + left + elem + right + "}"

    override def union(other: IntSet) =
      ((left union right) union other) incl elem
  }
}