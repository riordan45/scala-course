package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
      namedExpressions.map { case (name, exprSignal) =>
      name -> Signal {
        eval(getReferenceExpr(name, namedExpressions), namedExpressions)
      }
    }

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double = expr match {
    case Literal(v) => v

    case Ref(name) =>
      val referenceExpr = getReferenceExpr(name, references)
      eval(referenceExpr, references - name) // Recursively evaluate the reference

    case Plus(a, b) =>
      eval(a, references) + eval(b, references)

    case Minus(a, b) =>
      eval(a, references) - eval(b, references)

    case Times(a, b) =>
      eval(a, references) * eval(b, references)

    case Divide(a, b) =>
      val denominator = eval(b, references)
      if denominator == 0.0 then Double.NaN
      else eval(a, references) / denominator
  }


  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
