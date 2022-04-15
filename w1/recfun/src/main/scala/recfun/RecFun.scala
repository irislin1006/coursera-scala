package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def _count_numsOfOpens(chars: List[Char], numsOfOpens: Int): Boolean = {
      if (chars.isEmpty) numsOfOpens == 0
      else {
        val head_chr = chars.head
        val updates_open_paran_counts = {
          if (head_chr == '(') numsOfOpens + 1
          else if (head_chr == ')') numsOfOpens - 1
          else numsOfOpens
        }
        if (updates_open_paran_counts >= 0) _count_numsOfOpens(chars.tail, updates_open_paran_counts)
        else false
      }
    }
    _count_numsOfOpens(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
