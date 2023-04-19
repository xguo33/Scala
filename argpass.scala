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
 * 1. Submissions MUST compile using SBT with UNCHANGED configuration and
 *    tests with no compilation errors.  Submissions with compilation errors
 *    will receive 0 points.  Note that refactoring the code will cause the
 *    tests to fail.
 *
 * 2. You MUST NOT edit the SBT configuration and tests.  Altering it in your
 *    submission will result in 0 points for this assignment.
 *
 * 3. You MUST NOT use while loops or (re)assignment to variables (you can
 *    use "val" declarations, but not "var" declarations).  You must use
 *    recursion instead.
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

object argpass:

  class RefInt(initial: Int):
    private var n: Int = initial
    def get(): Int = n
    def set(m: Int): Unit = { n = m }

  // EXERCISE 1: complete the following higher-order function.  It has one
  // parameter f: a function that takes an instance of RefInt (see above for
  // the definition of the class RefInt) and returns Unit (i.e., nothing
  // interesting).
  //
  // Your code must create ONLY ONE instance of RefInt, then call f three times.
  // f will update the integer stored in the instances of RefInt it is given.
  //
  // Your code must return a tuple of the three integers provided by f in the
  // order that they came back from calls, i.e., the integer from the first
  // call to f is the first integer in the returned tuple.
  def refint1(f: RefInt => Unit): (Int, Int, Int) ={
    // TODO: Provide definition here.
    // Example call:
    // val r = RefInt (0)
    // f (r)
    // val n : Int = r.get()
    //throw UnsupportedOperationException()
    val r= RefInt(0)
    f(r)
    val n : Int = r.get()
    f(r)
    val o : Int = r.get()
    f(r)
    val p : Int = r.get()
    
    (n,o,p)
  
  }

  // EXERCISE 2: complete the following higher-order function.  It has one
  // parameter f: a function that takes an instance of RefInt (see above for
  // the definition of the class RefInt) and returns Unit (i.e., nothing
  // interesting).
  //
  // Your code must create EXACTLY THREE instances of RefInt, then call f
  // three times.  f will update the integer stored in the instances of
  // RefInt it is given.  However, f will not do this update immediately.  It
  // will only do it after the third call to f: at that point it updates all
  // three instances of RefInt that it has received as arguments so far.
  //
  // Your code must return a tuple of the three integers provided by f in the
  // order that they came back from calls, i.e., the integer from the first
  // call to f is the first integer in the returned tuple.
  def refint2(f: RefInt => Unit): (Int, Int, Int) ={
    // TODO: Provide definition here.
    //throw UnsupportedOperationException()
    val r1= RefInt(0)
    f(r1)
    val r2= RefInt(0)
    f(r2)
    val r3 = RefInt(0)
    f(r3)
    val a: Int =r1.get()
    val b: Int =r2.get()
    val c: Int = r3.get()
    (a,b,c)
  }


  // EXERCISE 3: complete the following function.  It has one parameter r: (a
  // reference to) an instance of RefInt (see above for the definition of the
  // class RefInt) and returns (a reference to) an instance of RefInt.
  //
  // Your code must increment (add 1 to) the RefInt it receives and return
  // double the original value (stored in a separate RefInt instance) as the
  // result.
  def refint3(r: RefInt): RefInt ={
    // TODO: Provide definition here.
    //throw UnsupportedOperationException()
    val a: Int = r.get()
    val double = new RefInt(a*2)
    r.set(a+1)
    double
}

  // EXERCISE 4: complete the following function.
  // It has two parameters
  // - r: (a reference to) an instance of RefInt (see above for the
  //      definition of the class RefInt); AND
  // - f: a function that accepts (a reference to) an instance of RefInt.
  //
  // Your code must call f with a *copy* of the RefInt r, i.e., it must make
  // a new instance of RefInt with the same Int.
  //
  // Your code should return true if f has NOT changed the Int stored in the
  // copy of r.  Otherwise it should return false.
  def refint4(r: RefInt, f: RefInt => Unit): Boolean ={
    // TODO: Provide definition here.
    //throw UnsupportedOperationException()
    val copy = new RefInt(r.get())
    f(copy)
    if copy.get()==r.get()
    then true
    else
      false
  }

  // EXERCISE 5: complete the following function.  It has one parameter: a
  // list of (references to) RefInt instances.
  //
  // Your code must CHANGE the argument it receives in a way that is visible
  // to the caller.  Note that this is the Scala List type, which is
  // immutable!  You can assume that the list you receive is not empty.
  def refint5(xs: List[RefInt]): Unit = xs match{
   // TODO: Provide definition here.
    //throw UnsupportedOperationException()
    case Nil => ()//assume empty
    case x :: xs => x.set(x.get()+1)
    refint5(xs) 
  }
   

