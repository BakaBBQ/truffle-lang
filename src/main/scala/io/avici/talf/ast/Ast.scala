package io.avici.talf.ast

/**
  * Created by Baqiao (Charles) Liu on 1/10/2016.
  */

/**
  * Copyright 2016 Baqiao (Charles) Liu
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  * http://www.apache.org/licenses/LICENSE-2.0
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */


object Ast {
  case class Identifier(name: String)
  case class Arguments(args: Seq[Expr])

  sealed trait Stmt
  sealed trait Expr
  object Stmt {
    case class FunctionDef(name: Identifier, args: Arguments, body: Seq[Stmt]) extends Stmt
    case class Assign(targets: Expr, value: Expr) extends Stmt
    case class ExprStmt(value: Expr) extends Stmt
  }

  object Expr {
    case class Call(func: Expr, args: Seq[Expr]) extends Expr
    case class Str(s: String) extends Expr
    case class Num(s: BigInt) extends Expr
    case class List(elts: Seq[Expr]) extends Expr
    case class BinOp(left: Expr, op: Operator, right: Expr) extends Expr
    case class IfExp(test: Expr, body: Expr, orelse: Expr) extends Expr
    case class UnaryOp(op: Ast.UnaryOp, operand: Expr) extends Expr
    case class BoolOp(op: Ast.BoolOp, values: Seq[Expr]) extends Expr
    case class Name(id: Identifier) extends Expr
    case class Attribute(value: Expr, attr: Identifier) extends Expr
    case class Compare(left: Expr, ops: Seq[CmpOp], comparators: Seq[Expr]) extends Expr
    case class Tuple(elts: Seq[Expr]) extends Expr
  }

  sealed trait Operator
  object Operator {
    case object Add extends Operator
    case object Sub extends Operator
    case object Mult extends Operator
    case object Div extends Operator
    case object Pow extends Operator
  }

  sealed trait BoolOp
  object BoolOp {
    case object And extends BoolOp
    case object Or extends BoolOp
  }

  sealed trait UnaryOp
  object UnaryOp {
    case object Invert extends UnaryOp
    case object Not extends UnaryOp
  }

  sealed trait CmpOp
  object CmpOp {
    case object Eq extends CmpOp
    case object NotEq extends CmpOp
    case object Lt extends CmpOp
    case object LtE extends CmpOp
    case object Gt extends CmpOp
    case object GtE extends CmpOp
  }

  case class Value(value: String) extends Expr
  case class Addition(lhs: Expr, rhs: Expr) extends Expr
  case class Subtraction(lhs: Expr, rhs: Expr) extends Expr
  case class Multiplication(lhs: Expr, rhs: Expr) extends Expr
  case class Division(lhs: Expr, rhs: Expr) extends Expr
}
