package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balancedParenthesis(x: Int, y: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty && (x == y))
        true
      else if (chars.isEmpty && (x != y))
        false
      else {
        val c = chars.head
        if (c.equals('('))
          balancedParenthesis(x + 1, y, chars.tail)
        else if (c.equals(')')) {
          if (x < y + 1) false
          else balancedParenthesis(x, y + 1, chars.tail)
        }
        else
          balancedParenthesis(x, y, chars.tail)
      }
    }

    if (chars.isEmpty)
      false
    else
      balancedParenthesis(0, 0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money < 0 || coins.isEmpty)
      0
    else if (money == 0)
      1
    else
      countChange(money-coins.head, coins) + countChange(money, coins.tail)
  }
}