package recfun
import common._

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
    if(c == 0 || c == r) 1
    else pascal(c-1,r-1) +  pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    def balance_aux(chars: List[Char], sum: Int): Int = {
      if(chars.isEmpty || sum < 0) sum
      else{
        if( chars.head == '(') balance_aux(chars.tail, sum + 1) 
        else if (chars.head == ')') balance_aux(chars.tail, sum - 1)
        else balance_aux(chars.tail, sum)
      } 
    }
    
    balance_aux(chars,0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
  
    def countChangeAux(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(coins.isEmpty) 0
      else if(money < 0 && !coins.isEmpty) 0
      else countChangeAux(money- coins.head,coins) + countChangeAux(money, coins.tail)
    }
    
    if(coins.isEmpty || money <= 0) 0
    else countChangeAux(money,coins)
  }
}
