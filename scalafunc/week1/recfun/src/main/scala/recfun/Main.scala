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
      if (c==0 || c == r) 1
      else pascal(c-1,r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance(chars: List[Char], acc: List[Char]): Boolean = {
        if (chars.isEmpty) {
          acc.isEmpty
        } else if (chars.head != '(' && chars.head != ')') {
          balance(chars.tail, acc)
        } else {
          if (chars.head == '(') {
            balance(chars.tail, chars.head :: acc)
          } else {
            if (acc.nonEmpty && acc.head == '(') {
              balance(chars.tail, acc.tail)
            } else false
          }
        }
      }
      balance(chars, List[Char]())
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0 ) 1
      else if (money < 0) 0
      else if (coins.isEmpty && money >    0) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }

  }
