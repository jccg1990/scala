import week1._

object generators {

  val integers = new Generator[Int] {
    def generate: Int = scala.util.Random.nextInt()
  }

  val booleans = integers.map(_ >= 0)

  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)

  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree

  trees.generate
}