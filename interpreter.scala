  // GENERATED
/* INSTRUCTIONS
 *
 * In this assignment, you will gain experience with several different concepts:
 * - case classes and objects
 * - representation of programs as data structures
 * - interpreters
 * - behavior of closures
 *
 * You will do this by using (and extending) an interpreter for a
 * dynamically-typed language similar to the language with closures discussed in
 * class.
 *
 * Complete the exercises below.  The exercises occur after the code for the
 * interpreter below. Search for "EXERCISE".  Most of the exercises should be
 * completed by replacing "todoExp" or "todoVal" with your own code.
 *
 * HINT: read the whole of this file before writing any code.  You might find
 * helpful examples buried within.
 *
 * HINT: there are few tests inside this file.  This is deliberate.  You should
 * write your own tests to verify your work as you go.  You can print abstract
 * syntax trees for expression and values using the "prettyPrintExp" and
 * "prettyPrintVal" functions.  You can evaluate your expressions using the
 * "evaluate" function (or just using "testExample" which calls "evaluate").
 */

object interpreter:

  enum Val:
    // VInt represents integer literals.
    case VInt(n: Int)
    // Str represents string literals.
    case VStr(s: String)
    // Nil represents the Nil pointer.
    case VNil
    // Pair represents a pair with evaluated members, i.e., "fst" and "snd" have
    // type "Val". Contrast with ExpPair, where "fst" and "snd" have type "Exp",
    // further below.
    case VPair(fst: Val, snd: Val)
    // PrimFun represents a primitve function, i.e., a function written in Scala
    case VPrimFun(display: Option[String], f: Val => Val)
    // Closure represents a closure (not a lambda abstraction), i.e., there is a
    // stored "env" environment in addition to the argNm variable and the body
    // of the closure.
    case VClosure(env: Env, argNm: String, body: Exp)
    // NEW: RecClosure represents a closure for a recursively-defined function.
    // This differs from Closure by also storing the function name itself.
    case VRecClosure(env: Env, funNm: String, argNm: String, body: Exp)

  enum Exp:
    // EVal allows values to be used as expressions.
    case EVal(v: Val)
    // EVar represents variables.
    case EVar(nm: String)
    // EPrint represents an expression that prints the value of the expression,
    // and then returns VNil.
    case EPrint(e: Exp)
    // EPair represents a pair with members to be evaluated.  When the "fst" and
    // "snd" have been evaluated, a VPair is returned.
    case EPair(fst: Exp, snd: Exp)
    // operator - applied to a single argument "arg").
    // EApp represents function application (a function "op" - short for
    case EApp(op: Exp, arg: Exp)
    // ELet represents an immutable binding of the value of an expression "e" to
    // a variable "nm".  The binding is only in scope in the expression "body".
    case ELet(nm: String, e: Exp, body: Exp)
    // EAbs creates an Anonymous function (also known as a lambda abstraction,
    // hence the "Abs" in "ExpAbs").
    case EAbs(argNm: String, body: Exp)
    // NEW: ERecAbs adds a function definition syntax as an expression (not a
    // value).  In order to allow recursion, the function is named (so it is not
    // an anonymous function / lambda abstraction).  This construct is more
    // expressive than ExpAbs above.  Note that it only allows taking a single
    // argument.
    case ERecAbs(funNm: String, argNm: String, body: Exp)
    // NEW: get first element of a pair
    case EFst(e: Exp)
    // NEW: get second element of a pair
    case ESnd(e: Exp)
    // NEW: Conditional expression representing "if (b) then tt else ff", where
    // "b" is an Exp that is expected to evaluate to an integer. If the integer
    // is non-zero, then the value of the whole expression is the value
    // resulting from evaluating "tt". If the integer is zero,  then the value
    // of the whole expression is the value resulting from evaluating "ff".
  // TODO: Uncomment the following line.
  //   case EIf (b:Exp, tt:Exp, ff:Exp)

  import Val.*
  import Exp.*

  type Env = scala.collection.immutable.Map[String, Val]
  val emptyEnv: Env = scala.collection.immutable.Map.empty

  def error(s: String) = throw new RuntimeException(s)

  def evaluate(env: Env, e: Exp): Val =

    // OPTIONAL: if you want to see the intermediate steps of evaluation, uncomment the following line.
    // println (e + " --> ?   in environment " + env)

    val result: Val = e match
      case EVal(v) => v

      case EVar(nm) =>
        val envContents: Option[Val] = env.get(nm)
        envContents match
          case None    => error("variable " + nm + " not found in environment")
          case Some(v) => v

      case EPrint(e) =>
        val eVal: Val = evaluate(env, e)
        println(prettyPrintVal(eVal))
        VNil

      case EPair(fst, snd) =>
        val fstVal: Val = evaluate(env, fst)
        val sndVal: Val = evaluate(env, snd)
        VPair(fstVal, sndVal)

      case EApp(op, arg) =>
        val opVal: Val = evaluate(env, op)
        opVal match
          case VPrimFun(_, f) =>
            val argVal: Val = evaluate(env, arg)
            f(argVal)

          // The application of a closure to an argument restores the
          // environment from the closure during the evaluation of the body (and
          // also adds the binding of "nm" to the argument value "argVal").
          case VClosure(envC, argNm, body) =>
            val argVal: Val = evaluate(env, arg)
            evaluate(envC + ((argNm, argVal)), body)

          // NEW: Similar to VClosure, but also adds the recursive function back
          // into the environment used to run the body of the
          // recursively-defined function.  Consequently, the body of the
          // function can call itself (by referencing its name "funNm").
          case VRecClosure(envC, funNm, argNm, body) =>
            val argVal: Val = evaluate(env, arg)
            evaluate(envC + ((argNm, argVal)) + ((funNm, opVal)), body)

          case _ => error("expected function in application")

      case ELet(nm, e, body) =>
        val eVal: Val = evaluate(env, e)
        evaluate(env + ((nm, eVal)), body)

      // A lambda abstraction expression evaluates to a closure value with the
      // same argument variable "nm" and expression "body". Additionally, the
      // closure value captures the current environment, so that it can be
      // restored when "body" is evaluated.
      case EAbs(argNm, body) =>
        VClosure(env, argNm, body)

      // NEW: Behaves as for lambda abstractions EAbs above, except that the
      // function name funNm is also captured.
      case ERecAbs(funNm, argNm, body) =>
        VRecClosure(env, funNm, argNm, body)

      case EFst(e) =>
        val eVal: Val = evaluate(env, e)
        eVal match
          case VPair(fst, _) => fst
          case _             => error("expected pair in fst")

      case ESnd(e) =>
        val eVal: Val = evaluate(env, e)
        eVal match
          case VPair(_, snd) => snd
          case _             => error("expected pair cell in snd")

    // TODO: Add case for EIf below.

    // OPTIONAL: if you want to see the intermediate steps of evaluation and
    // their results, uncomment the following line.
    // println (e + " --> " + result + "   in environment " + env)

    result

  // Pretty print a value as a string.
  def prettyPrintVal(v: Val): String = v match
    case VInt(n) => Integer.toString(n)
    case VStr(s) => s
    case VNil    => "Nil"
    case VPair(fst, snd) =>
      "(" + prettyPrintVal(fst) + ", " + prettyPrintVal(snd) + ")"
    case VPrimFun(display, f) => "[fun: " + display.getOrElse("?") + "]"
    case VClosure(env, argNm, body) =>
      "[" + env + "; " + argNm + " => " + prettyPrintExp(body) + "]"
    case VRecClosure(env, funNm, argNm, body) =>
      "[" + env + "; " + funNm + "; " + argNm + " => " + prettyPrintExp(
        body
      ) + "]"

  // Pretty print an expression as a string.
  def prettyPrintExp(e: Exp): String = e match
    case EVal(v)   => prettyPrintVal(v)
    case EVar(nm)  => nm
    case EPrint(e) => "(print " + prettyPrintExp(e) + ")"
    case EPair(fst, snd) =>
      "(" + prettyPrintExp(fst) + ", " + prettyPrintExp(snd) + ")"
    case EApp(op, arg) =>
      "(" + prettyPrintExp(op) + " " + prettyPrintExp(arg) + ")"
    case ELet(nm, e, body) =>
      "(let " + nm + " = " + prettyPrintExp(e) + " in " + prettyPrintExp(
        body
      ) + ")"
    case EAbs(argNm, body) => "(" + argNm + " => " + prettyPrintExp(body) + ")"
    case ERecAbs(funNm, argNm, body) =>
      "(" + funNm + " " + argNm + " => " + prettyPrintExp(body) + ")"
    case EFst(e) => "(fst " + prettyPrintExp(e) + ")"
    case ESnd(e) => "(snd " + prettyPrintExp(e) + ")"
  // TODO: Add case for EIf below.

  /////////////////////////////////////////////////////////////////////////
  // BEGIN EXAMPLES                                                                                            //
  /////////////////////////////////////////////////////////////////////////

  // A primitive function for adding two values.  It will throw an exception if
  // the values are not both integers.
  //
  // NOTE: it is not necessary to add primSub for subtraction, because one can
  // substitute uses of addition using, for example:
  //
  // i.    (x - 5) = (x + (-5)) ii.   (x - y) = (x + (-1 * y))
  val primAdd: Val = VPrimFun(
    Some("_+_"),
    {
      case VInt(n1) =>
        VPrimFun(
          None,
          {
            case VInt(n2) => VInt(n1 + n2)
            case _ =>
              throw new RuntimeException("expected two integer arguments")
          }
        )
      case _ => throw new RuntimeException("expected two integer arguments")
    }
  )

  // A primitive function for multiplying two values.  It will throw an exception if
  // the values are not both integers.
  val primMul: Val = VPrimFun(
    Some("_*_"),
    {
      case VInt(n1) =>
        VPrimFun(
          None,
          {
            case VInt(n2) => VInt(n1 * n2)
            case _ =>
              throw new RuntimeException("expected two integer arguments")
          }
        )
      case _ => throw new RuntimeException("expected two integer arguments")
    }
  )

  // A primitive function for testing equality of two values.  It will throw an
  // exception if the values are not both integers.
  val primEqInt = VPrimFun(
    Some("_==_"),
    {
      case VInt(n1) =>
        VPrimFun(
          None,
          {
            case VInt(n2) => VInt(if n1 == n2 then 1 else 0)
            case _ =>
              throw new RuntimeException("expected two integer arguments")
          }
        )
      case _ => throw new RuntimeException("expected two integer arguments")
    }
  )

  // Sample program: (let x = 5 + 6; x)
  val prog01: Exp =
    ELet(
      "x",
      EApp(EApp(EVal(primAdd), EVal(VInt(5))), EVal(VInt(6))),
      EVar("x")
    )

  // Sample program: (let x = 5 + 6; y = 20; x * y)
  val prog02: Exp =
    ELet(
      "x",
      EApp(EApp(EVal(primAdd), EVal(VInt(5))), EVal(VInt(6))),
      ELet(
        "y",
        EVal(VInt(20)),
        EApp(EApp(EVal(primMul), EVar("x")), EVar("y"))
      )
    )

  // Sample program: ((let x = 5 in (y => x + y)) 6)
  val prog03: Exp =
    EApp(
      ELet(
        "x",
        EVal(VInt(5)),
        EAbs(
          "y",
          EApp(EApp(EVal(primAdd), EVar("x")), EVar("y"))
        )
      ),
      EVal(VInt(6))
    )

  // Sample program corresponding to the Scala program:
  //
  //   val fact:Int=>Int = def fact(n:Int):Int = if (n != 0) n * fact(n + -1)
  //     else 1 fact
  //
  //   fact (5)
  //
  // Note that this program has the same effect as:
  //
  //   def fact(n:Int):Int = if (n != 0) n * fact(n + -1) else 1 fact (5)
  //
  // But there is a slight mismatch between Scala's form of recursive function
  // definitions and the language used in this interpreter.  The one in this
  // interpreter is simpler (and chosen for that reason), whereas Scala's form
  // is easier to use in practice, but slightly more complex to specify.
  //
  // NOTE: this example is commented out.  You can uncomment it once you have
  // added EIf in one of the exercises below. BEGIN-PRIVATE END-PRIVATE
  // TODO: Remove the start comment characters below.
  /*
  val recTest:Exp =
    ELet (
      "fact",
      ERecAbs (
        "fact",
        "n",
        EIf (
          EVar ("n"),
          EApp (
            EApp (
              EVal (primMul),
              EVar ("n")
            ),
            EApp (
              EVar ("fact"),
              EApp (
                EApp (
                  EVal (primAdd),
                  EVar ("n")
                ),
                EVal (VInt (-1))
              )
            )
          ),
          EVal (VInt (1))
        )
      ),
      EApp (EVar ("fact"), EVal (VInt (5)))
    )
  // TODO: Remove the end comment characters below.
   */

  // def counter (n) = m => (n, counter (n + 1));
  val counterFunction: Exp =
    ERecAbs(
      "counterFunction",
      "n",
      EAbs(
        "m",
        EPair(
          EVar("n"),
          EApp(
            EVar("counterFunction"),
            EApp(
              EApp(
                EVal(primAdd),
                EVar("n")
              ),
              EVal(VInt(1))
            )
          )
        )
      )
    )

  val counter: Exp = EApp(counterFunction, EVal(VInt(4)))

  val printTest: Exp =
    EPrint(EVal(VInt(5)))

  /////////////////////////////////////////////////////////////////////////
  // END EXAMPLES                                                          
  /////////////////////////////////////////////////////////////////////////

  val todoVal: Val = VNil
  val todoExp: Exp = EVal(todoVal)
  val todoEnv: Env = emptyEnv

  // EXERCISE 1: write abstract syntax for the integer 50.  Your abstract syntax
  // should have type "Val".
  // TODO: Change the body of the definition below.
  val exFiftyVal: Val = todoVal

  // EXERCISE 2: write abstract syntax for the integer 50.  Your abstract syntax
  // should have type "Exp".  
  // TODO: Change the body of the definition below.
  val exFiftyExp: Exp = todoExp

  // EXERCISE 3: write an environment that maps variable "x" to the integer
  // value 50.  
  // TODO: Change the body of the definition below.
  val exX: Env = todoEnv

  // EXERCISE 4: write abstract syntax corresponding to (x + 50).
  // HINT: use the primitive function "primAdd" (search for it in this file).
  // HINT: see the sample abstract syntax trees for uses of "primAdd".
  // TODO: Change the body of the definition below.
  val exXPlusFifty: Exp = todoExp

  // HINT: you can test your code for "exX" and "xPlusFifty" by adding a call to
  // the following function to the "main" method below (this has already been
  // done for you).
  def testXPlusFifty() =
    println("EVALUATING: " + prettyPrintExp(exXPlusFifty))
    val v = evaluate(exX, exXPlusFifty)
    println("RESULT: " + prettyPrintVal(v))

  // EXERCISE 5: write abstract syntax corresponding to ( (x:Int) => x + 50 ),
  // i.e., an anonymous function that takes an argument "x" and returns (x+50).
  //
  // NOTE: the programming language for which you are writing abstract syntax is
  // dynamically-typed and so the Scala type annotation ":Int" cannot be
  // represented in the abstract syntax (so do not try to represent it!).
  // TODO: Change the body of the definition below.
  val exXFunPlusFifty: Exp = todoExp

  // EXERCISE 6: write abstract syntax corresponding to (( (x:Int) => x + 50 )
  // (30) ), i.e., applying an anonymous function (that takes an argument "x"
  // and returns (x+50)) to the argument (30).  You can reference
  // "exXFunPlusFifty" in your answer to eliminate the need to copy that
  // abstract syntax tree.
  // TODO: Change the body of the definition below.
  val exXFunPlusFiftyApplied: Exp = todoExp

  // EXERCISE 7: write abstract syntax corresponding to:
  //   let x = 5;
  //   let y = 6;
  //   x + y
  // (in Scala this would be: (val x = 5; val y = 6; x + y))
  // TODO: Change the body of the definition below.
  val exLetXLetY: Exp = todoExp

  // EXERCISE 8: write abstract syntax corresponding to:
  //   let f = ( (x:Int) => x + 50 );
  //   f (5)
  // TODO: Change the body of the definition below.
  val exLetF: Exp = todoExp

  // EXERCISE 9: extend the abstract syntax with a conditional expression.  You
  // should uncomment the line "case class EIf (b:Exp, tt:Exp, ff:Exp)" and then
  // add support for EIf to the "evaluate" and "prettyPrintExp" functions.  Once
  // your support is complete, you can uncomment the "recTest" example and run
  // it using "testExample (recTest)".

  // EXERCISE 10: write abstract syntax corresponding to:
  //   (5, (x) => 6)
  // That is, a pair consisting of an integer and an anonymous function (that
  // takes an unused argument x, and returns 6 when run).    
  // TODO: Change the body of the definition below.
  val exPair: Exp = todoExp

  // EXERCISE 11: look for the definition of "counter:Exp" above.  The abstract
  // syntax represents the application of a function "counterFunction" to the
  // integer 4.  When applied to a number n, "counterFunction" returns an
  // anonymous function that takes an unused argument x, and returns a pair when
  // it is invoked. The first element of the pair is the given number n.  The
  // second element of the pair is the result of running "counterFunction"
  // applied to (n+1).
  //
  // The definition of "counterTest:Exp" below shows how to extract the integer
  // from the first component of the pair. It evaluates to the value 4 when run.
  val counterTest: Exp =
    ELet("counter", EApp(counter, EVal(VNil)), EFst(EVar("counter")))
  //
  // Write abstract syntax that extracts the integer from the first element of
  // the second element of the pair returned from "counter:Exp". It should
  // evaluate to the value 5 when run. You MUST use "counter:Exp".  You MUST NOT
  // write some other expression that evaluates to 5, e.g., (5) or (4+1), etc.
  // TODO: Change the body of the definition below.
  val exCounter: Exp = todoExp

  // EXERCISE 12: explain in words why the functions used in exercise 11 require
  // an implementation to use closures (or something similar). Your answer
  // should be specific to the example, i.e., you should explain what would
  // happen when running this code if the naive (incorrect) implementation of
  // functions was used.
  //
  // ANSWER: PUT YOUR ANSWER TO EXERCISE 12 HERE.

  def testExample(e: Exp) =
    try
      println("EVALUATING: " + prettyPrintExp(e))
      val v = evaluate(emptyEnv, e)
      println("RESULT: " + prettyPrintVal(v))
      println("+=" * 40)
    catch
      case e: Exception =>
        println("ERROR: ")
        e.printStackTrace

  def main(args: Array[String]) =
    // Test the sample programs.
    testExample(prog01)
    testExample(prog02)
    testExample(prog03)
    testExample(printTest)
    testExample(counterTest)

    println("EXERCISE 2 TEST")
    testExample(exFiftyExp)

    println("EXERCISE 4 TEST")
    testXPlusFifty()
    println("+=" * 40)

    println("EXERCISE 5 TEST")
    testExample(exXFunPlusFifty)

    println("EXERCISE 6 TEST")
    testExample(exXFunPlusFiftyApplied)

    println("EXERCISE 7 TEST")
    testExample(exLetXLetY)

    println("EXERCISE 8 TEST")
    testExample(exLetF)

    println("EXERCISE 9 TEST")
    // TODO: UNCOMMENT THIS LINE ONCE EIf IS FINISHED
    // testExample (recTest)

    println("EXERCISE 10 TEST")
    testExample(exPair)

    println("EXERCISE 11 TEST")
    testExample(exCounter)

