package io.avici.talf.parser


import fastparse.all
import fastparse.noApi._
import White.WsApi._
import io.avici.talf.ast.Ast
import Expressions._
import Lexical.kw
import io.avici.talf.ast.Ast.Expr

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
class Statements {
  import Expressions._
  val space = P( CharIn(" \n") )
  val NEWLINE: P0 = P( "\n" | End)
  val ENDMARKER: P0 = P( End )

  val dottedName = P( NAME.rep(1, "."))

  val singleInput: P[Seq[Ast.Stmt]] = P(
    NEWLINE.map(_ => Nil) |
    simpleStmt
  )

  val exprStmt: P[Ast.Stmt] = {
    val assign = P( testlist ~ ("=" ~ testlist.map(_.head)))
    P(
      assign.map{
        case (a, null) => Ast.Stmt.ExprStmt(a.head)
        case (a, b) => Ast.Stmt.Assign(a.head, b)
        case _ => Ast.Stmt.Assign(Ast.Expr.Name(Ast.Identifier("x")), Ast.Expr.Name(Ast.Identifier("x")))
      }
    )
  }

  val smallStmt = P( exprStmt )

  val simpleStmt: P[Seq[Ast.Stmt]] = P (smallStmt.rep(1, sep=";") ~ ";".?)

  // TODO: Include Compound Statement
  val stmt: P[Seq[Ast.Stmt]] = P( simpleStmt )

  val funcDef: P[Ast.Stmt.FunctionDef] = P( kw("def") ~/ NAME ~ parameters).map {
    case (name, args) => Ast.Stmt.FunctionDef(name, args, List())
  }

  val parameters: P[Ast.Arguments] = P( "(" ~ argslist ~ ")")

  def collapseDottedName(name: Seq[Ast.Identifier]): Ast.Expr = {
    name.tail.foldLeft[Ast.Expr](Ast.Expr.Name(name.head))(
      (x, y) => Ast.Expr.Attribute(x, y)
    )
  }

  def funcCall: P[Ast.Expr] = P(dottedName ~ ("(" ~ arglist ~ ")" ).?).map{
    case (name, None) => collapseDottedName(name)
    case (name, Some((args))) =>
      val x = collapseDottedName(name)
      Ast.Expr.Call(x, args)
  }
}
