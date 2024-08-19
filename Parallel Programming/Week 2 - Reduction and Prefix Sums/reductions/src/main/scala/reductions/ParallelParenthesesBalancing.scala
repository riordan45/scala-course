package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
  @tailrec
  def traverse(idx: Int, openCount: Int): Boolean = {
    if (idx == chars.length) openCount == 0
    else if (openCount < 0) false
    else if (chars(idx) == '(') traverse(idx + 1, openCount + 1)
    else if (chars(idx) == ')') traverse(idx + 1, openCount - 1)
    else traverse(idx + 1, openCount)
  }
  traverse(0, 0)
}


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

  def traverse(idx: Int, until: Int, openCount: Int, closeCount: Int): (Int, Int) = {
    var open = openCount
    var close = closeCount

    for (i <- idx until until) {
      if (chars(i) == '(') open += 1
      else if (chars(i) == ')') {
        if (open > 0) open -= 1
        else close += 1
      }
    }
    (open, close)
  }

  def reduce(from: Int, until: Int): (Int, Int) = {
    if (until - from <= threshold) {
      traverse(from, until, 0, 0)
    } else {
      val mid = from + (until - from) / 2
      val ((openLeft, closeLeft), (openRight, closeRight)) = parallel(
        reduce(from, mid),
        reduce(mid, until)
      )
      val matched = Math.min(openLeft, closeRight)
      (openLeft + openRight - matched, closeLeft + closeRight - matched)
    }
  }

  reduce(0, chars.length) == (0, 0)
}


  // For those who want more:
  // Prove that your reduction operator is associative!

