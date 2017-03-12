package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

    def cyclic(nm: String, expr: Expr): Boolean = {
      expr match {
        case Literal(v) => false
        case Ref(n) => {
          if (n == nm) true
          else {
            if (namedExpressions.isDefinedAt(n)) {
              val ee = namedExpressions(n)()
              cyclic(nm, ee)
            } else {
              false
            }
          }
        }
        case Plus(a, b) => cyclic(nm, a) || cyclic(nm, b)
        case Minus(a, b) => cyclic(nm, a) || cyclic(nm, b)
        case Times(a, b) => cyclic(nm, a) || cyclic(nm, b)
        case Divide(a, b) => cyclic(nm, a) || cyclic(nm, b)
      }
    }

    for ( (a, b) <- namedExpressions) yield {
      val sig = Signal{
        val expr = b()
        val _selfDeps = cyclic(a, expr)
        if (_selfDeps) Double.NaN
        else {
          eval(expr, namedExpressions)
        }
      }
      (a, sig)
    }

  }

//  private def cyclic(nm: String, expr: Expr, references: Map[String, Signal[Expr]]): Boolean = {
//    expr match {
//      case Literal(v) => false
//      case Ref(n) => {
//        if (n == nm) true
//        else {
//          if (references.isDefinedAt(n)) {
//            val ee = references(n)()
//            cyclic(nm, ee, references)
//          } else {
//            false
//          }
//        }
//      }
//      case Plus(a, b) => cyclic(nm, a, references) || cyclic(nm, b, references)
//      case Minus(a, b) => cyclic(nm, a, references) || cyclic(nm, b, references)
//      case Times(a, b) => cyclic(nm, a, references) || cyclic(nm, b, references)
//      case Divide(a, b) => cyclic(nm, a, references) || cyclic(nm, b, references)
//    }
//  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(n) => {
        val ee = getReferenceExpr(n, references)
        eval(ee, references)
      }
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
