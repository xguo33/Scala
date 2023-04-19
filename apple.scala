sealed abstract class Exp 
case class ExpInt (n: Int) extends Exp
case class ExpAdd (el: Exp, e2: Exp) extends Exp 
case class ExpMul (el: Exp, e2: Exp) extends Exp 

def f (e: Exp) : Int= e match { 
  case ExpInt (m) => m 
  case ExpAdd (el, e2) => f (el) + f (e2) 
  case ExpMul (el, e2) => f (el) * f (e2) 
}