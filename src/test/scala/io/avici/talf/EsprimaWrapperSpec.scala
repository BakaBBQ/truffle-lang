package io.avici.talf


import org.scalacheck.Properties
import org.scalacheck._
import org.scalacheck.Gen._
import Arbitrary.arbitrary
import io.avici.talf.esprima.JsAst

import argonaut._
import Argonaut._

/**
  * Created by Baqiao (Charles) Liu on 1/18/2016.
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
object EsprimaWrapperSpec extends Properties("EsprimaWrapper"){
  import JsAst._

  val genRegexpFlag: Gen[String] = for {
    str <- listOf(oneOf('g','i','m','u','i'))
  } yield str.mkString("")

  val genIdentifier = for {
    s <- alphaStr
  } yield JsAst.Identifier(s)

  val genStringLiteral: Gen[Expression] = for {
    str <- alphaStr
  } yield StringLiteral(str)

  val genRegexLiteral: Gen[Expression] = for {
    flags <- genRegexpFlag
    pattern <- alphaStr
  } yield RegExpLiteral(RegexContents(pattern, flags))

  val genLiteral: Gen[Expression] = oneOf(genStringLiteral, genRegexLiteral)

  val genUnaryOp = {
    import JsAst.UnaryOperator._
    val a = List(UnaryOperator.`+`, UnaryOperator.`-`, `typeof`, UnaryOperator.`~`, `delete`, `void`, UnaryOperator.`!`)
    oneOf(a)
  }

  val genBinOp = {
    import JsAst.BinaryOperator._
    oneOf(
      BinaryOperator.`!=`,
      BinaryOperator.`!==`,
      BinaryOperator.`%`,
      BinaryOperator.`&`,
      BinaryOperator.`*`,
      BinaryOperator.`+`,
      BinaryOperator.`-`,
      BinaryOperator.`/`,
      BinaryOperator.`<`,
      BinaryOperator.`<<`,
      BinaryOperator.`<=`,
      BinaryOperator.`==`,
      BinaryOperator.`===`,
      BinaryOperator.`>`,
      BinaryOperator.`>=`,
      BinaryOperator.`>>`,
      BinaryOperator.`>>>`,
      BinaryOperator.`^`,
      BinaryOperator.`in`,
      BinaryOperator.`instanceof`
    )
  }

  val genUpdateOp = {
    oneOf(List(
      UpdateOperator.`++`, UpdateOperator.`--`
    ))
  }

  val genLogicalOp = {
    oneOf(LogicalOperator.`&&`, LogicalOperator.`||`)
  }


  val genRegexpContents: Gen[String] = alphaStr

  val genUnaryExpression = for {
    prefix <- oneOf(true, false)
    operator <- genUnaryOp
    exp <- genLiteral
  } yield UnaryExpression(operator, prefix, exp)

  val genUpdateExpression: Gen[Expression] = for {
    op <- genUpdateOp
    exp <- genLiteral
    prefix <- oneOf(true, false)
  } yield UpdateExpression(op, exp, prefix)

  val genBinaryExpression: Gen[Expression] = for {
    op <- genBinOp
    lhs <- genLiteral
    rhs <- genLiteral
  } yield BinaryExpression(op, lhs, rhs)

  val genAssignmentExpression: Gen[Expression] = for {
    op <- {
      import AssignmentOperator._
      oneOf(`%=`, `&=`, `*=`, `+=`, `-=`, `/=`, `<<=`, `=`, `>>=`, `>>>=`, `^=`, `|=`)
    }
    left <- genIdentifier
    right <- genLiteral
  } yield AssignmentExpression(op, left, right)

  val genLogicalExpression: Gen[Expression] = for {
    op <- genLogicalOp
    lhs <- genIdentifier
    rhs <- genIdentifier
  } yield LogicalExpression(op, lhs, rhs)

  val genMemberExpression: Gen[Expression] = for {
    obj <- genIdentifier
    property <- genIdentifier
    computed <- oneOf(true, false)
  } yield MemberExpression(obj, property, computed)

  val genConditionalExpression: Gen[Expression] = for {
    test <- genBinaryExpression
    alternate <- genIdentifier
    consequent <- genIdentifier
  } yield ConditionalExpression(test, alternate, consequent)

  val genCallExpression: Gen[Expression] = for {
    callee <- genIdentifier
    arguments <- listOf(genLiteral)
  } yield CallExpression(callee, arguments)

  val genSequenceExpression: Gen[Expression] = for {
    exps <- listOf(genLiteral)
  } yield SequenceExpression(exps)

  val genArrayExpression: Gen[Expression] = for {
    exp <- genLiteral
  } yield ArrayExpression(Some(exp))

  lazy val genExpression: Gen[Expression] = oneOf(
    genUpdateExpression,
    genBinaryExpression,
    genAssignmentExpression,
    genLogicalExpression,
    genConditionalExpression,
    genCallExpression,
    genSequenceExpression,
    genLiteral,
    genArrayExpression
  )

  property("genExpression") = Prop.forAll(genExpression){(x : Expression) =>
    x.asJson.spaces2
    true
  }
}
