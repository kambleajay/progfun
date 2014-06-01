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
  def pascal(c: Int, r: Int): Int = if(c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1) 

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
   
    def checkCount(xs: List[Char], ys:List[Char]):Boolean = {
    	if(count(xs) < 0) false
    	else if(ys.isEmpty) true
    	else checkCount(xs ::: (ys.head :: Nil), ys.tail)
	}                                         
	
	def count(xs:List[Char]):Int = {
		if(xs.isEmpty) 0
		else if(xs.head == '(') 1 + count(xs.tail)
		else if(xs.head == ')') -1 + count(xs.tail)
		else count(xs.tail)
	} 
	
	if(chars.isEmpty) true else checkCount(chars.head :: Nil, chars.tail) && count(chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if(money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins);
  }
}
