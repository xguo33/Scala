  // GENERATED
/* INSTRUCTIONS
 *
 * Complete the exercises below.  For each "EXERCISE" comment, add code
 * immediately below the comment.
 *
 * Please see README.md for instructions, including compilation and testing.
 *
 * GRADING
 *
 * 1. Submissions MUST compile using SBT with UNCHANGED configuration and tests
 *    with no compilation errors.  Submissions with compilation errors will
 *    receive 0 points.  Note that refactoring the code will cause the tests to
 *    fail.
 *
 * 2. You MUST NOT edit the SBT configuration and tests.  Altering it in your
 *    submission will result in 0 points for this assignment.
 *
 * 3. You MUST NOT use while loops or (re)assignment to variables (you can use
 *    "val" declarations, but not "var" declarations).  You must use recursion
 *    instead.
 *
 * 4. You may declare auxiliary functions if you like.
 *
 * SUBMISSION
 *
 * 1. Submit this file on D2L before the deadline.
 *
 * 2. Late submissions will not be permitted because solutions will be discussed
 *    in class.
 *
 */
import java.util.NoSuchElementException
object fp1:

  // Here is a utility function for logging recursive functions.
  // It may be helpful while debugging
  // Don't use this in the solutions you include in your final submission.
  def log[X](prefix: String)(computeResult: => X) =
    println(prefix)
    val result = computeResult
    println(s"$prefix : $result")
    result
  // Note: computeResult is a "by-name" parameter.  We will discuss these
  // later in the course.  Short version: a by-name parameter is non-strict;
  // it is reevaluated every time it is used in the function body.

  // EXAMPLE: here is the definition of the factorial function.
  def fact(n: Int): Int =
    if n <= 1 then 1
    else n * fact(n - 1)

  // EXAMPLE: factorial with logging
  def factLog(n: Int): Int =
    log(s"fact($n)") {
      if n <= 1 then 1
      else n * factLog(n - 1)
    }

  // Note that the fact computes as follows (leaving out some steps):
  //
  // fact (5)
  // --> if 5 <= 1 then 1 else 5 * fact (5 - 1)
  // --> if false then 1 else 5 * fact (5 - 1)
  // --> 5 * fact (5 - 1)
  // --> 5 * fact (4)
  // --> 5 * (if 4 <= 1 then 1 else 4 * fact (4 - 1))
  // --> 5 * (4 * fact (3))
  // --> 5 * (4 * (3 * fact (2))
  // --> 5 * (4 * (3 * (2 * fact (1)))
  // --> 5 * (4 * (3 * (2 * (if 1 <= 1 then 1 else 1 * fact (1 - 1)))))
  // --> 5 * (4 * (3 * (2 * (if true then 1 else 1 * fact (1 - 1)))))
  // --> 5 * (4 * (3 * (2 * 1)))
  // --> 5 * (4 * (3 * 2))
  // --> 5 * (4 * 6)
  // --> 5 * 24
  // --> 120
  //
  // We can get the same answer with less work by starting at the base case and
  // computing up:
  //
  // fact (1) --> 1
  // fact (2) --> 2 * fact (1) --> 2 * 1 --> 2
  // fact (3) --> 3 * fact (2) --> 3 * 2 --> 6
  // fact (4) --> 4 * fact (3) --> 4 * 6 --> 14
  // fact (5) --> 5 * fact (4) --> 5 * 24 --> 120

  // EXERCISE 1: complete the following definition, so that factTest is the list
  // of integers List(1,2,6,24,120).
  //
  // You must call the "fact" function (five times) defined above instead of
  // hardcoding the numbers 1,2,6,24,120.
  val factTest: List[Int] =List(fact(1),fact(2),fact(3),fact(4),fact(5))

  // EXERCISE 2: complete the following definition of the Fibonacci function.
  // You can use the mathematical definition of Fib:
  // https://en.wikipedia.org/wiki/Fibonacci_number
  //
  // fib(0) == 0
  // fib(1) == 1
  // fib(n) == fib(n-1) + fib(n-2), if n>1
  def fib(n: Int): Int = if n<=1 then n
    else fib(n-1)+fib(n-2)  	


  // EXERCISE 3: declare the identifier "p1" with a pair consisting of the
  // Int 7 and the String "hello"
  val p1: (Int, String) =
    // TODO: Provide definition here.
    (7, "hello")

  // EXERCISE 4: declare the identifier "t1" with a triple consisting of the
  // Int 7, the String "hello", and the Boolean false
  val t1: (Int, String, Boolean) =
    // TODO: Provide definition here.
    (7, "hello", false)

  // EXERCISE 5: write a function "swap" that takes a pair of an Int and a
  // String, and returns a pair of a String and an Int (with the values from
  // the pair passed an argument.
  //
  // E.g., swap (p1) should return ("hello", 7).  You can use "p(0)" and
  // "p(1)" to access the first and second components of a pair.
  def swap(p: (Int, String)): (String, Int) =(p(1),p(0))

  // EXERCISE 6: write a function "sum" that takes a list of integers and
  // sums them.
  //
  // As with all of the exercises in this assignment, your function MUST be
  // recursive and MUST NOT use a while loop.
  def sum(xs: List[Int]): Int = 
   if (xs.isEmpty) then 0 
   else xs.head+sum(xs.tail)
    // TODO: Provide definition here.
    //throw UnsupportedOperationException()


  // EXERCISE 7: given the definition of the function "sumTailAux" below,
  // complete the definition of the function "sumTail" so that it also sums 
  // a list of integers.
  //
  // You must not alter the definition of "sumTailAux".
  //
  // Your definition for "sumTail" must call "sumTailAux" directly, and must
  // not call "sum"
  def sumTailAux(xs: List[Int], accumulator: Int): Int =
    xs match
      case Nil     => accumulator
      case y :: ys => sumTailAux(ys, accumulator + y)

  def sumTail(xs: List[Int]): Int =sumTailAux(xs,0)

  // EXERCISE 8: complete the following definition of the function "max" that
  // finds the maximum integer in a list of integers.
  //
  // If the list is empty, throw a java.util.NoSuchElementException (with //no
  // argument).
  //
  // Your function MUST be recursive and MUST NOT use a while loop.
  //
  // You MUST NOT use the "max" method on lists, but can use the "max" method
  // on integers: That is, you cannot use (xs.max) or similar.  But you can
  // use (1 max 2).
  def maxList(xs: List[Int]): Int ={
  if (xs.isEmpty) then
    throw new java.util.NoSuchElementException
  if xs.size == 1 then  xs.head 
    else if xs.head > xs.tail.max then xs.head
    else xs.tail.max 
}
  // EXERCISE 9: given the definition of the function "maxTail" below,
  // complete the definition of the function "maxTailAux" so that "maxTail"
  // also finds the maximum of a list of integers.
  //
  // You must not alter the definition of "maxTail".
  //
  // Your definition for "maxTailAux" must be recursive and not use while
  // loops.
  def maxTailAux(xs: List[Int], accumulator: Int): Int =
    if xs.isEmpty then accumulator
    else if xs.head > accumulator then maxTailAux(xs.tail,xs.head)
    else // xs.head< accumulator then
    maxTailAux(xs.tail,accumulator)

  def maxTail(xs: List[Int]): Int =
    xs match
      case Nil     => throw NoSuchElementException()
      case y :: ys => maxTailAux(ys, y)


  // EXERCISE 10: Write a recursive function "otpu" ("upto" backwards) that
  // takes two Int parameters "start" and "end" and produces a "List[Int]"
  // that counts DOWN from "start" to "end" (inclusive at both ends) one at //a
  // time.
  //
  // If "start < end", the empty list must be returned.
  //
  // For example:
  // - otpu(10,5) == List(10,9,8,7,6,5)
  // - otpu(10,11) == Nil
  def otpu(start: Int, end: Int): List[Int] =
    if start < end then List()
    else start :: otpu(start-1,end)