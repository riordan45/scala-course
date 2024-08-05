package recfun

import java.util.Stack
import java.util.HashMap

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
    def factorial(n: Int): Int = n match {
      case 0 => 1
      case 1 => 1
      case _ if n > 1 => n * factorial(n - 1)
      case _ => throw new IllegalArgumentException("Input must be a non-negative integer")
    }
    return factorial(r) / (factorial(r - c) * factorial(c))
  }
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var s = Stack[Char]()
    val set = Set('(', ')')
    def traverse(word: List[Char]): Boolean = {
      if (word.isEmpty) return true

      if (!set.contains(word.head)) return true && traverse(word.tail)

      if (word.head.compare('(') == 0) {s.push('(') ; return true && traverse(word.tail)}

      if (word.head.compare(')') == 0) {
        if (s.isEmpty()) return false else {s.pop(); return true && traverse(word.tail)}
      } else return true
    }

    return traverse(chars) && s.isEmpty()
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
  // Create a HashMap to store intermediate results
  val memo = new HashMap[(Int, List[Int]), Int]()

  // Define the recursive function
  def count(money: Int, coins: List[Int]): Int = {
    if (money == 0) return 1 // Base case: one way to make 0 money
    if (money < 0) return 0  // Base case: no way to make negative money
    if (coins.isEmpty) return 0 // Base case: no coins left but money is not 0

    // Check if the result is already in the memo
    val key = (money, coins)
    if (memo.containsKey(key)) {
      memo.get(key).asInstanceOf[Int]
    } else {
      // Calculate the result and store it in the memo
      val result = count(money - coins.head, coins) + count(money, coins.tail)
      memo.put(key, result)
      result
    }
  }

  // Call the recursive function with initial parameters
  count(money, coins)
}
