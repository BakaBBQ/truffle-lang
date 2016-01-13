package io.avici.talf.parser


import io.avici.talf.ast.Ast

import fastparse.Implicits.Sequencer
import fastparse.all._
import io.avici.talf.ast.Ast.Identifier

/**
  * Created by Baqiao (Charles) Liu on 1/13/2016.
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
object Expressions {
  import Lexical.kw

  def tuplize(xs: Seq[Ast.Expr]) = xs match {
    case Seq(x) => x
    case xs => Ast.Expr.Tuple(xs)
  }

  val NAME: P[Ast.Identifier] = Lexical.identifier
  val NUMBER = P(Lexical.decimalinteger).map(Ast.Expr.Num)
  val STRING = P(Lexical.stringliteral)

  def op[T](s: P0, rhs: T) = s.!.map(_ => rhs)
  val Add = op("+", Ast.Operator.Add)
  val Sub = op("-", Ast.Operator.Sub)
  val Mult = op("*", Ast.Operator.Mult)
  val Div = op("/", Ast.Operator.Div)
  val Lt = op("<", Ast.CmpOp.Lt)
  val Gt = op(">", Ast.CmpOp.Gt)
  val GtE = op(">=", Ast.CmpOp.GtE)
  val LtE = op("<=", Ast.CmpOp.LtE)
  val Eq = op("==", Ast.CmpOp.Eq)
  val CompOp = P( LtE|GtE|Eq|Gt|Lt )

  def chain(p: P[Ast.Expr], op: P[Ast.Operator]) = {
    P( p ~ (op ~ p).rep).map{
      case (lhs, chunks) =>
        chunks.foldLeft(lhs){
          case (lhs, (op, rhs)) => Ast.Expr.BinOp(lhs, op, rhs)
        }
    }
  }

  val expr: P[Ast.Expr] = P(arithExpr)

  val arithExpr = P( chain(term, Add | Sub) )
  val term = P( chain(factor, Mult | Div))
  val factor: P[Ast.Expr] = P( ("+"|"-") ~ factor)

  val test: P[Ast.Expr] = {
    val ternary = P( orTest ~ (kw("if") ~ orTest ~ kw("else") ~ test).?).map {
      case (x, None) => x
      case (x, Some((test, neg))) => Ast.Expr.IfExp(test, x, neg)
    }
    P( ternary )
  }

  val orTest = P( andTest.rep(1, kw("or")) ).map{
    case Seq(x) => x
    case xs => Ast.Expr.BoolOp(Ast.BoolOp.Or, xs)
  }

  val andTest = P( notTest.rep(1, kw("and")) ).map{
    case Seq(x) => x
    case xs => Ast.Expr.BoolOp(Ast.BoolOp.And, xs)
  }

  val notTest: P[Ast.Expr] = P(("not" ~ notTest).map(Ast.Expr.UnaryOp(Ast.UnaryOp.Not, _)) | comparison)

  val comparison: P[Ast.Expr] = P( expr ~ (CompOp ~ expr).rep).map{
    case (lhs, Nil) => lhs
    case (lhs, chunks) =>
      val (ops, vals) = chunks.unzip
      Ast.Expr.Compare(lhs, ops, vals)
  }

  val atom: P[Ast.Expr] = {
    val emptyList = ("[" ~ "]").map(_ => Ast.Expr.List(Nil))
    P(
      emptyList |
      STRING.rep(1).map(_.mkString).map(Ast.Expr.Str) |
      NAME.map(Ast.Expr.Name) |
      NUMBER
    )
  }

  val listContents = P( test.rep(1, ",") ~ ",".?)
  val list = P( listContents ).map(Ast.Expr.List)

  val trailer: P[Ast.Expr => Ast.Expr] = {
    val call = P("(" ~ arglist ~ ")").map{case (args) => (lhs: Ast.Expr) => Ast.Expr.Call(lhs, args)}
    P( call )
  }

  val arglist = {
    val inits = P( (test ~ !"=").rep(0, ",") )
    P(inits)
  }

  val testlist: P[Seq[Ast.Expr]] = P( test.rep(1, sep = ",") ~ ",".?)

  val argslist: P[Ast.Arguments] = {
    val namedArg = P( fpdef ~ ("=" ~ test).? )
    val x = P( namedArg.rep(sep = ",") ~ ",".? ~ ("*" ~ NAME).? ~ ",".? ~ ("**" ~ NAME).? ).map {
      case (normalArgs, starargs, kwargs) =>
        val (args, defaults) = normalArgs.unzip
        Ast.Arguments(args)
    }

    P( x )
  }

  val fpdef: P[Ast.Expr] = P( NAME.map(Ast.Expr.Name))
  val fplist: P[Ast.Expr] = P( fpdef.rep(sep = ",") ~ ",".? ).map(Ast.Expr.Tuple)
}
