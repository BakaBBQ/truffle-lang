package io.avici.talf.esprima

/**
  * Created by Baqiao (Charles) Liu on 1/14/2016.
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

object JsAst {
  sealed trait Node {
    def `type`: String
  }

  case class SourceLocation(source: Option[String], start: Position, end: Position)

  case class Position(line: Int, column: Int)


  sealed trait Expression extends Node

  case class Identifier(name: String) extends Expression {
    val `type` = "Identifier"
  }

  sealed trait Literal extends Expression {
    val `type` = "Literal"
  }

  case class RegExpLiteral() extends Literal {
    def regex = 1
  }

  case class Program(body: Seq[Statement]) extends Node{
    val `type` = "Program"
  }

  sealed trait Function extends Node {
    def id: Option[Identifier]
    def params: Seq[Pattern]
    def body: BlockStatement
  }

  case class ExpressionStatement(expression: Expression) extends Statement {
    val `type` = "ExpressionStatement"
  }

  case class BlockStatement(body: Seq[Statement]) extends Statement {
    val `type` = "BlockStatement"
  }

  case object EmptyStatement extends Statement {
    val `type` = "EmptyStatement"
  }

  case class DebuggerStatement() extends Statement {
    val `type` = "DebuggerStatement"
  }

  case class WithStatement(`object`: Expression, body: Statement) extends Statement{
    val `type` = "WithStatement"
  }

  case class ReturnStatement(argument: Option[Expression]) extends Statement {
    val `type` = "ReturnStatement"
  }

  case class LabeledStatement(label: Identifier, body: Statement) extends Statement {
    val `type` = "LabeledStatement"
  }

  case class BreakStatement(label: Option[Identifier]) extends Statement {
    val `type` = "BreakStatement"
  }

  case class ContinueStatement(label: Option[Identifier]) extends Statement {
    val `type` = "ContinueStatement"
  }

  case class IfStatement(test: Expression, consequent: Statement, alternate: Option[Statement]) {
    val `type` = "IfStatement"
  }

  case class SwitchStatement(discriminant: Expression, cases: Seq[SwitchCase]) extends Statement {
    val `type` = "SwitchStatement"
  }

  case class SwitchCase(test: Option[Expression], consequent: Seq[Statement]) extends Node {
    val `type` = "SwitchCase"
  }

  case class ThrowStatement(argument: Expression) extends Statement {
    val `type` = "ThrowStatement"
  }

  case class TryStatement(block: BlockStatement, handler: Option[CatchClause], finalizer: Option[BlockStatement]) extends Statement {
    val `type` = "TryStatement"
  }

  case class CatchClause(param: Pattern, body: BlockStatement) extends Node {
    val `type` = "CatchClause"
  }

  case class WhileStatement(test: Expression, body: Statement) extends Statement {
    val `type` = "WhileStatement"
  }

  case class DoWhileStatement(body: Statement, test: Expression) extends Statement {
    val `type` = "DoWhileStatement"
  }

  val ForStatement = ???

  sealed trait Declaration extends Statement

  case class FunctionDeclaration(id: Identifier, params: Seq[Pattern], body: BlockStatement) extends Function with Declaration {
    val `type` = "FunctionDeclaration"
  }

  case class VariableDeclaration(declarations: Seq[VariableDeclarator]) extends Declaration{
    val kind = "var"
    val `type` = "VariableDeclaration"
  }

  case class VariableDeclarator(id: Pattern, init: Option[Expression]) extends Node {
    val `type` = "VariableDeclarator"
  }


  case object ThisExpression extends Expression {
    val `type` = "ThisExpression"
  }

  case class ArrayExpression(elements: Option[Expression]) extends Expression {
    val `type` = "ArrayExpression"
  }

  case class ObjectExpression(properties: Property) extends Expression {
    val `type` = "ObjectExpression"
  }

  case class Property(key: Either[Literal, Identifier], value: Expression, kind: PropertyKind)

  case class FunctionExpression(id: Option[Identifier], params: Seq[Pattern], body: Statement) extends Function with Expression {
    val `type` = "FunctionExpression"
  }

  case class UnaryExpression(operator: UnaryOperator, prefix: Boolean, argument: Expression) extends Expression {
    val `type` = "UnaryExpression"
  }

  sealed trait UnaryOperator
  object UnaryOperator {
    case object `-` extends UnaryOperator
    case object `+` extends UnaryOperator
    case object `!` extends UnaryOperator
    case object `~` extends UnaryOperator
    case object `typeof` extends UnaryOperator
    case object `void` extends UnaryOperator
    case object `delete` extends UnaryOperator
  }

  case class UpdateExpression(operator: UpdateOperator, argument: Expression, prefix: Boolean) {
    val `type` = "UpdateExpression"
  }

  sealed trait UpdateOperator
  object UpdateOperator {
    case object `++` extends UpdateOperator
    case object `--` extends UpdateOperator
  }

  sealed trait BinaryOperator
  object BinaryOperator {
    case object `==` extends BinaryOperator
    case object `!=` extends BinaryOperator
    case object `===` extends BinaryOperator
    case object `!==` extends BinaryOperator
    case object `<` extends BinaryOperator
    case object `<=` extends BinaryOperator
    case object `>` extends BinaryOperator
    case object `>=` extends BinaryOperator
    case object `<<` extends BinaryOperator
    case object `>>` extends BinaryOperator
    case object `>>>` extends BinaryOperator
    case object `+` extends BinaryOperator
    case object `-` extends BinaryOperator
    case object `*` extends BinaryOperator
    case object `/` extends BinaryOperator
    case object `%` extends BinaryOperator
    case object `|` extends BinaryOperator
    case object `^` extends BinaryOperator
    case object `&` extends BinaryOperator
    case object `in` extends BinaryOperator
    case object `instanceof` extends BinaryOperator
  }


  case class BinaryExpression(operator: BinaryOperator, left: Expression, right: Expression) extends Expression {
    val `type` = "BinaryExpression"
  }

  case class AssignmentExpression(operator: AssignmentOperator, left: Either[Pattern, Expression], right: Expression)

  sealed trait AssignmentOperator
  object AssignmentOperator {
    case object `=` extends AssignmentOperator
    case object `+=` extends AssignmentOperator
    case object `-=` extends AssignmentOperator
    case object `*=` extends AssignmentOperator
    case object `/=` extends AssignmentOperator
    case object `%=` extends AssignmentOperator
    case object `<<=` extends AssignmentOperator
    case object `>>=` extends AssignmentOperator
    case object `>>>=` extends AssignmentOperator
    case object `|=` extends AssignmentOperator
    case object `^=` extends AssignmentOperator
    case object `&=` extends AssignmentOperator
  }

  case class LogicalExpression(operator: LogicalOperator, left: Expression, right: Expression) extends Expression {
    val `type` = "LogicalExpression"
  }

  case class MemberExpression(`object`: Expression, property: Expression, computed: Boolean) extends Expression {
    val `type` = "MemberExpression"
  }

  case class ConditionalExpression(test: Expression, alternate: Expression, consequent: Expression) extends Expression {
    val `type` = "ConditionalExpression"
  }

  sealed trait CallExpression extends Expression {
    val `type` = "CallExpression"
    def callee: Expression
    def arguments: Seq[Expression]
  }

  case class NewExpression(callee: Expression, arguments: Seq[Expression]) extends CallExpression {
    override val `type` = "NewExpression"
  }

  case class SequenceExpression(expressions: Seq[Expression]) extends Expression {
    val `type` = "SequenceExpression"
  }

  sealed trait LogicalOperator
  object LogicalOperator {
    case object `||` extends LogicalOperator
    case object `&&` extends LogicalOperator
  }

  sealed trait PropertyKind
  object PropertyKind {
    case object Init extends PropertyKind
    case object Get extends PropertyKind
    case object Set extends PropertyKind
  }
  sealed trait Pattern extends Node
  sealed trait Statement extends Node
}
