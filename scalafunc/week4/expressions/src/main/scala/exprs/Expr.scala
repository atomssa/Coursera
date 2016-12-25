package exprs

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }
  def show: String = this match {
    case Number(n) => n.toString
    case Sum(e1, e2) => e1 + " + " + e2
    case Prod(e1, e2) => e1 + " * " + e2
  }
}

case class Number(n: Int) extends Expr {
  override def toString: String = n.toString
}

case class Sum(e1: Expr, e2: Expr) extends Expr {
  override def toString: String = e1 + " + " + e2
}

case class Prod(e1: Expr, e2: Expr) extends Expr {
  override def toString: String = e1 + " * " + e2
}