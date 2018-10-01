import week1._

object randtest {
  val integers = new Generator[Int] {
    def generate: Int = scala.util.Random.nextInt()
  }

  val booleans = for (x <- integers) yield x > 0

  def single[T](x: T) = new Generator[T] {
    override def generate: T = x
  }

  def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for {
    x <- t
    y <- u
  } yield (x, y)

  def emptyLists = single(Nil)

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  def lists: Generator[List[Int]] = for {
    cutoff <- booleans
    lists <- if (cutoff) emptyLists else nonEmptyLists
  } yield lists

  def test[T](r: Generator[T], notimes: Int = 100)(test: T => Boolean): Unit = {
    for (_ <- 0 until notimes) {
      val value = r.generate
      assert(test(value), "Test failed for: " + value)
    }
    println("Test passed " + notimes + " times")
  }

  test(pairs(lists, lists)) {
    case (xs, ys) => (xs ++ ys).length > xs.length
  }
}