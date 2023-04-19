  // GENERATED
/* INSTRUCTIONS
 *
 * Complete the exercises below.  For each "EXERCISE" comment, add
 * code immediately below the comment.
 *
 * Please see README.md for instructions, including compilation and testing.
 *
 * GRADING
 *
 * 1. Submissions MUST compile using SBT with UNCHANGED configuration and
 *    tests with no compilation errors.  Submissions with compilation errors
 *    will receive 0 points.  Note that refactoring the code will cause the
 *    tests to fail.
 *
 * 2. You MUST NOT edit the SBT configuration and tests.  Altering it in your
 *    submission will result in 0 points for this assignment.
 *
 * 3. You MUST NOT use while loops.  You must use recursion or higher-order
 *    functions instead.
 *
 * 4. You may declare auxiliary functions if you like.
 *
 * SUBMISSION
 *
 * 1. Submit this file on D2L before the deadline.
 *
 * 2. Late submissions will not be permitted because solutions will be
 *    discussed in class.
 *
 */

object subtyping:

  // Instances of Counter have a integer field that can be incremented,
  // decremented, or read.
  class Counter:
    private var n = 0
    def increment() = { n = n + 1 }
    def decrement() = { n = n - 1 }
    def get(): Int = n

  // EXERCISE 1: complete the following function.  The observeCounter
  // function has one parameter f: a function that accepts (a reference to) a
  // Counter instance but returns nothing.
  //
  // The observeCounter function should call f with (a reference to) an
  // object (of a class extending Counter).
  //
  // Your class that extends Counter must keep track of the total number of
  // times that increment/decrement have been called.
  // I.e., if the increment method is called 3 times on an instance, and the
  // decrement method is called 2 times on the same instance, then it should
  // store 5 (somewhere other than the existing field n).
  //
  // observeCounter should call f, and then return the total number of times
  // that increment/decrement were called on the instance by f.
  def observeCounter(f: Counter => Unit): Int = {
    // TODO: Provide definition here.
    //throw UnsupportedOperationException()
    class CounterEX1 extends Counter{
      private var ex1 = 0
      override def decrement() ={ex1= ex1+1}
      override def increment() ={ex1= ex1+1}
      override def get(): Int = ex1
    }
    val x = new CounterEX1();
    f(x)
    x.get()
  }

  // EXERCISE 2: complete the following function.  It is the same as
  // observeCounter except that f has a parameter of type List[Counter] not
  // Counter.
  //
  // f will insist that the List[Counter] has length 3.
  //
  // You must return a List[Int] not an Int.
  //
  // The first element of the result List[Int] must correspond to the number
  // of times that increment/decrement were called on the first element of
  // type List[Counter], similarly for the second and third elements.
  def observeCounterList(f: List[Counter] => Unit): List[Int] ={
    // TODO: Provide definition here.
    //throw UnsupportedOperationException()
    class CounterEX2 extends Counter{
      private var ex2 = 0
      override def decrement() ={ex2= ex2+1}
      override def increment() ={ex2= ex2+1}
      override def get(): Int = ex2
    }

    var  ex2List : List[Int] =Nil
    val a = new CounterEX2()
    val b = new CounterEX2()
    val c = new CounterEX2()
    f(List(a,b,c))
    ex2List = List(a.get(),b.get(),c.get())
    ex2List
  }

  // EXERCISE 3: complete the following function.  It is the same as
  // observeCounterList except that f has a parameter of type Array[Counter]
  // not List[Counter].
  //
  // f will insist that the Array[Counter] has length 3.
  //
  // You must return a Array[Int] not a List[Int].
  //
  // The first element of the result Array[Int] must correspond to the number
  // of times that increment/decrement were called on the first element of
  // type Array[Counter], similarly for the second and third elements.
  //
  // In this exercise you are NOT allowed to asInstanceOf.
  def observeCounterArray(f: Array[Counter] => Unit): Array[Int] ={
   class CounterEX3 extends Counter {
      private var ex3 = 0
      override def decrement() = { ex3 = ex3 + 1 }
      override def increment() = { ex3 = ex3 + 1 }
      override def get(): Int = ex3
    }

    var ex3Array: Array[Int] = new Array[Int](3)

    val d = new CounterEX3()
    val e = new CounterEX3()
    val g = new CounterEX3()

    f(Array(d, e, g))
    ex3Array = Array(d.get(), e.get(), g.get())
    ex3Array
  }
    // TODO: Provide definition here.
    //throw UnsupportedOperationException()
    // I use val f which is wrong also I need to return Arrat[Counter]
    //f alredy exsit 

