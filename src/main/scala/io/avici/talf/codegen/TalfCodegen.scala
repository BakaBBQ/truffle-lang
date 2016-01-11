package io.avici.talf.codegen

import io.avici.talf.ast._
import argonaut._, Argonaut._

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
class TalfCodegen {
  def codegen(ast : Expr) : Json = {
    Json("type" := "Program", "body" := List(
      Json(
        "type" := "ExpressionStatement",
        "expression" := talfExpr2Js(ast)
      )
    ))
  }

  def convertToBinaryExpr(operator: String, lhs: Expr, rhs: Expr) : Json = {
    Json("type" := "BinaryExpression", "operator" := operator, "left" := talfExpr2Js(lhs), "right" := talfExpr2Js(rhs))
  }

  def talfExpr2Js(expr : Expr) : Json = {
    expr match {
      case Value(x) => Json("type" := "Literal", "value" := x.toInt, "raw" := x)
      case Addition(lhs, rhs) => convertToBinaryExpr("+", lhs, rhs)
      case Subtraction(lhs, rhs) => convertToBinaryExpr("-", lhs, rhs)
      case Division(lhs, rhs) => convertToBinaryExpr("/", lhs, rhs)
      case Multiplication(lhs, rhs) => convertToBinaryExpr("*", lhs, rhs)
    }
  }
}
