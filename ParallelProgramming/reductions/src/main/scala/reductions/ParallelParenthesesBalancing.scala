package reductions

import common._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)
  @volatile var seqResult = false
  @volatile var parResult = false

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    if (chars.isEmpty) true
    else {
      var i = 0
      var b = 0
      while (i < chars.length) {
        if (chars(i) == '(') b += 1
        else if (chars(i) == ')') {
          b -= 1
          if (b < 0) return false
        }
        i += 1
      }
      b == 0
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (until <= idx) {
        (0, 0)
      } else {
        var i = idx
        var x = arg1
        var y = arg2

        while (i < until) {
          if (chars(i) == '(') x += 1
          else if (chars(i) == ')') {
            if (x > 0) x -= 1
            else y += 1
          }
          i += 1
        }
        (x, y)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from) / 2
        val ((l1, l2), (r1, r2)) = parallel(reduce(from, mid), reduce(mid, until))

        def closed = {
          scala.math.min(l1, r2)
        }

        (l1 + r1 - closed, l2 + r2 - closed)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
