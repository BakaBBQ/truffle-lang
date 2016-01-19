package io.avici.truffle.esprima


import argonaut._
import shapeless._
import Argonaut._

import scala.Either

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

  implicit def OptionExpEncodeJson: EncodeJson[Option[Expression]] =
    EncodeJson{it: Option[Expression] =>
      it match {
        case Some(exp) => exp.asJson
        case None => jNull
      }}


  implicit def StmtEncodeJson: EncodeJson[Statement] =
    EncodeJson{it: Statement =>
      it match {
        case s: ExpressionStatement => ExpressionStatementEncodeJson(s).asJson
        case s: BlockStatement => BlockStatementEncodeJson(s).asJson
        case EmptyStatement => EmptyStatement.asJson
        case DebuggerStatement => DebuggerStatement.asJson
        case s: WithStatement => WithStatementEncodeJson(s).asJson
        case s: ReturnStatement => ReturnStatementEncodeJson(s).asJson
        case s: LabeledStatement => LabeledStatementEncodeJson(s).asJson
        case s: BreakStatement => BreakStatementEncodeJson(s).asJson
        case s: ContinueStatement => ContinueStatementEncodeJson(s).asJson
        case s: IfStatement => IfStatementEncodeJson(s).asJson
        case s: SwitchStatement => SwitchStatementEncodeJson(s).asJson
        case s: ThrowStatement => ThrowStatementEncodeJson(s).asJson
        case s: TryStatement => TryStatementEncodeJson(s).asJson
        case s: WhileStatement => WhileStatementEncodeJson(s).asJson
        case s: DoWhileStatement => DoWhileStatementEncodeJson(s).asJson
        case s: Declaration => DeclarationEncodeJson(s).asJson
        case s: ForStatement => ForStatementEncodeJson(s).asJson
      }
    }

  implicit def DeclarationEncodeJson = EncodeJson{it: Declaration =>
    it match {
      case it: FunctionDeclaration => FunctionDeclarationEncodeJson(it).asJson
      case it: VariableDeclaration => VariableDeclarationEncodeJson(it).asJson
    }
  }

  implicit def SeqVariableDeclaratorEncodeJson = EncodeJson{it: Seq[VariableDeclarator] =>
    jArray(it.map(_.asJson).toList)
  }

  implicit def SeqSwitchCaseEncodeJson = EncodeJson{it: Seq[SwitchCase] =>
    jArray(it.map(_.asJson).toList)
  }

  implicit def SeqStmtEncodeJson = EncodeJson{it: Seq[Statement] =>
    jArray(it.map(_.asJson).toList)
  }
  implicit def SeqExpEncodeJson: EncodeJson[Seq[Expression]] =
    EncodeJson{it: Seq[Expression] =>
      jArray(it.map(_.asJson).toList)
    }

  implicit def ExpressionEncodeJson: EncodeJson[Expression] =
    EncodeJson{it: Expression =>
      it match {
        case e: UnaryExpression => UnaryExpressionEncodeJson(e).asJson
        case e: UpdateExpression => UpdateExpressionEncodeJson(e).asJson
        case e: BinaryExpression => BinaryExpressionEncodeJson(e).asJson
        case e: AssignmentExpression => AssignmentExpressionEncodeJson(e).asJson
        case e: LogicalExpression => LogicalExpressionEncodeJson(e).asJson
        case e: MemberExpression => MemberExpressionEncodeJson(e).asJson
        case e: ConditionalExpression => ConditionalExpressionEncodejJson(e).asJson
        case e: CallExpression => CallExpressionEncodeJson(e).asJson
        case e: SequenceExpression => SequenceExpressionEncodeJson(e).asJson
        case e: Identifier => IdentifierEncodeJson(e).asJson
        case e: Literal => LiteralEncodeJson(e).asJson
        case e: ArrayExpression => ArrayExpressionEncodeJson(e).asJson
        case e: ObjectExpression => ObjectExpressionEncodeJson(e).asJson
      }
    }

  implicit def LiteralEncodeJson: EncodeJson[Literal] =
    EncodeJson{it: Literal =>
      it match {
        case x: StringLiteral => StringLiteralEncodeJson(x).asJson
        case x: RegExpLiteral => RegExpLiteralEncodeJson(x).asJson
      }
    }

  case class SourceLocation(source: Option[String], start: Position, end: Position)

  implicit def SourceLocationEncodeJson: EncodeJson[SourceLocation] =
    EncodeJson((it: SourceLocation) =>
      ("source" := it.source) ->: ("start" := it.start) ->: ("end" := it.end) ->: jEmptyObject)

  case class Position(line: Int, column: Int)

  implicit def PositionEncodeJson: EncodeJson[Position] =
    EncodeJson((it: Position) =>
      ("line" := it.line) ->: ("column" := it.column) ->: jEmptyObject)

  sealed trait Expression extends Node

  case class Identifier(name: String) extends Expression{
    val `type` = "Identifier"
  }

  implicit def IdentifierEncodeJson: EncodeJson[Identifier] =
    EncodeJson((it: Identifier) =>
      ("type" := it.`type`) ->: ("name" := it.name) ->: jEmptyObject)

  sealed trait Literal extends Expression {
    val `type` = "Literal"
  }

  case class RegexContents(pattern: String, flags: String)

  implicit def RegexContentsEncodeJson: EncodeJson[RegexContents] =
    jencode2L((it: RegexContents) => (it.pattern, it.flags))("pattern", "flags")

  case class RegExpLiteral(regex: RegexContents) extends Literal

  implicit def RegExpLiteralEncodeJson: EncodeJson[RegExpLiteral] =
    jencode2L((it: RegExpLiteral) => (it.`type`, it.regex))("type", "regex")

  case class StringLiteral(str: String) extends Literal {
    def value = str
    def raw = "\"" + str + "\""
  }

  implicit def StringLiteralEncodeJson: EncodeJson[StringLiteral] =
    EncodeJson((it: StringLiteral) =>
      ("type" := it.`type`) ->: ("value" := it.value) ->: ("raw" := it.raw)
        ->: jEmptyObject)

  case class Program(body: Seq[Statement]) extends Node{
    val `type` = "Program"
  }

  implicit def ProgramEncodeJson: EncodeJson[Program] =
    EncodeJson((it: Program) =>
      ("type" := it.`type`) ->: ("body" := it.body) ->: jEmptyObject)

  sealed trait Function extends Node {
    def id: Option[Identifier]
    def params: Seq[Pattern]
    def body: BlockStatement
  }

  case class ExpressionStatement(expression: Expression) extends Statement {
    val `type` = "ExpressionStatement"
  }

  implicit def ExpressionStatementEncodeJson: EncodeJson[ExpressionStatement] =
    EncodeJson((it: ExpressionStatement) =>
      ("type" := it.`type`) ->: ("expression" := it.expression) ->: jEmptyObject)

  case class BlockStatement(body: Seq[Statement]) extends Statement {
    val `type` = "BlockStatement"
  }

  implicit def BlockStatementEncodeJson: EncodeJson[BlockStatement] =
    EncodeJson((it: BlockStatement) =>
      ("type" := it.`type`) ->: ("body" := it.body) ->: jEmptyObject)

  case object EmptyStatement extends Statement {
    val `type` = "EmptyStatement"
  }

  case object DebuggerStatement extends Statement {
    val `type` = "DebuggerStatement"
  }

  case class WithStatement(`object`: Expression, body: Statement) extends Statement{
    val `type` = "WithStatement"
  }

  implicit def WithStatementEncodeJson: EncodeJson[WithStatement] =
    jencode3L((it: WithStatement) => (it.`type`, it.`object`, it.body))("type", "object", "body")

  case class ReturnStatement(argument: Option[Expression]) extends Statement {
    val `type` = "ReturnStatement"
  }

  implicit def ReturnStatementEncodeJson: EncodeJson[ReturnStatement] =
    jencode2L((it: ReturnStatement) => (it.`type`, it.argument))("type", "argument")

  case class LabeledStatement(label: Identifier, body: Statement) extends Statement {
    val `type` = "LabeledStatement"
  }

  implicit def LabeledStatementEncodeJson: EncodeJson[LabeledStatement] =
    jencode3L((it: LabeledStatement) => (it.`type`, it.label, it.body))("type", "label", "body")

  case class BreakStatement(label: Option[Identifier]) extends Statement {
    val `type` = "BreakStatement"
  }

  implicit def BreakStatementEncodeJson: EncodeJson[BreakStatement] =
    jencode2L{it: BreakStatement => (it.`type`, it.label)}("type", "label")

  case class ContinueStatement(label: Option[Identifier]) extends Statement {
    val `type` = "ContinueStatement"
  }

  implicit def ContinueStatementEncodeJson: EncodeJson[ContinueStatement] =
    jencode2L{it: ContinueStatement => (it.`type`, it.label)}("type", "label")

  case class IfStatement(test: Expression, consequent: Statement, alternate: Option[Statement]) {
    val `type` = "IfStatement"
  }

  implicit def IfStatementEncodeJson: EncodeJson[IfStatement] =
    jencode4L{it: IfStatement => (it.`type`, it.test, it.consequent, it.alternate)}("type", "test", "consequent", "alternate")

  case class SwitchStatement(discriminant: Expression, cases: Seq[SwitchCase]) extends Statement {
    val `type` = "SwitchStatement"
  }

  implicit def SwitchStatementEncodeJson: EncodeJson[SwitchStatement] =
    jencode3L{it: SwitchStatement => (it.`type`, it.discriminant, it.cases)}("type", "discriminant", "cases")

  case class SwitchCase(test: Option[Expression], consequent: Seq[Statement]) extends Node {
    val `type` = "SwitchCase"
  }

  implicit def SwitchCaseEncodeJson: EncodeJson[SwitchCase] =
    jencode3L{it: SwitchCase => (it.`type`, it.test, it.consequent)}("type", "test", "consequent")

  case class ThrowStatement(argument: Expression) extends Statement {
    val `type` = "ThrowStatement"
  }

  implicit def ThrowStatementEncodeJson: EncodeJson[ThrowStatement] =
    jencode2L{it: ThrowStatement => (it.`type`, it.argument)}("type", "argument")

  case class TryStatement(block: BlockStatement, handler: Option[CatchClause], finalizer: Option[BlockStatement]) extends Statement {
    val `type` = "TryStatement"
  }

  implicit def TryStatementEncodeJson: EncodeJson[TryStatement] =
    jencode4L{it: TryStatement => (it.`type`, it.block, it.handler, it.finalizer)}("type", "block", "handler", "finalizer")

  case class CatchClause(param: Pattern, body: BlockStatement) extends Node {
    val `type` = "CatchClause"
  }

  implicit def CatchClauseEncodeJson: EncodeJson[CatchClause] =
    jencode3L{it: CatchClause => (it.`type`, it.param, it.body)}("type", "param", "body")

  case class WhileStatement(test: Expression, body: Statement) extends Statement {
    val `type` = "WhileStatement"
  }

  implicit def WhileStatementEncodeJson: EncodeJson[WhileStatement] =
    jencode3L{it: WhileStatement => (it.`type`, it.test, it.body)}("type", "test", "body")

  case class DoWhileStatement(body: Statement, test: Expression) extends Statement {
    val `type` = "DoWhileStatement"
  }

  implicit def DoWhileStatementEncodeJson: EncodeJson[DoWhileStatement] =
    jencode3L{it: DoWhileStatement => (it.`type`, it.body, it.test)}("type", "body", "test")

  case class ForStatement(init: VariableDeclaration, test: Option[Expression], update: Option[Expression], body: Statement) extends Statement {
    val `type` = "ForStatement"
  }

  implicit def ForStatementEncodeJson: EncodeJson[ForStatement] =
    jencode5L{it: ForStatement => (it.`type`, it.init, it.test, it.update, it.body)}("type", "init", "test", "update", "body")

  sealed trait Declaration extends Statement

  case class FunctionDeclaration(id: Option[Identifier], params: Seq[Pattern], body: BlockStatement) extends Function with Declaration {
    val `type` = "FunctionDeclaration"
  }

  implicit def FunctionDeclarationEncodeJson: EncodeJson[FunctionDeclaration] =
    jencode4L {it: FunctionDeclaration => (it.`type`, it.id, it.params, it.body)}("type", "id", "params", "body")

  case class VariableDeclaration(declarations: Seq[VariableDeclarator]) extends Declaration{
    val kind = "var"
    val `type` = "VariableDeclaration"
  }

  implicit def VariableDeclarationEncodeJson: EncodeJson[VariableDeclaration] =
    jencode3L {it: VariableDeclaration => (it.`type`, it.kind, it.declarations)}("type", "kind", "declaration")

  case class VariableDeclarator(id: Pattern, init: Option[Expression]) extends Node {
    val `type` = "VariableDeclarator"
  }

  implicit def VariableDeclaratorEncodeJson: EncodeJson[VariableDeclarator] =
    jencode3L {it: VariableDeclarator => (it.`type`, it.init, it.id)}("type", "init", "id")

  case object ThisExpression extends Expression {
    val `type` = "ThisExpression"
  }

  case class ArrayExpression(elements: Option[Expression]) extends Expression {
    val `type` = "ArrayExpression"
  }

  implicit def ArrayExpressionEncodeJson: EncodeJson[ArrayExpression] =
    jencode2L {it: ArrayExpression => (it.`type`, it.elements)}("type", "elements")

  case class ObjectExpression(properties: Property) extends Expression {
    val `type` = "ObjectExpression"
  }

  implicit def ObjectExpressionEncodeJson: EncodeJson[ObjectExpression] =
    jencode2L {it: ObjectExpression => (it.`type`, it.properties)}("type", "properties")

  case class Property(key: Either[Literal, Identifier], value: Expression, kind: PropertyKind) extends Node {
    val `type` = "Property"
  }

  implicit def PropertyEncodeJson: EncodeJson[Property] =
    jencode4L {it: Property => (it.`type`, it.key, it.value, it.kind)}("type", "key", "value", "kind")

  case class FunctionExpression(id: Option[Identifier], params: Seq[Pattern], body: BlockStatement) extends Function with Expression {
    val `type` = "FunctionExpression"
  }

  implicit def FunctionExpressionEncodeJson: EncodeJson[FunctionExpression] =
    jencode4L {it: FunctionExpression => (it.`type`, it.id, it.params, it.body)}("type", "id", "params", "body")

  case class UnaryExpression(operator: UnaryOperator, prefix: Boolean, argument: Expression) extends Expression {
    val `type` = "UnaryExpression"
  }

  implicit def UnaryExpressionEncodeJson: EncodeJson[UnaryExpression] =
    jencode4L {it: UnaryExpression => (it.`type`, it.operator, it.prefix, it.argument)}("type", "operator", "prefix", "argument")

  sealed trait UnaryOperator {
    def name: String
  }
  object UnaryOperator {
    case object `-` extends UnaryOperator {
      val name = "-"
    }
    case object `+` extends UnaryOperator {
      val name = "+"
    }
    case object `!` extends UnaryOperator {
      val name = "!"
    }
    case object `~` extends UnaryOperator {
      val name = "~"
    }
    case object `typeof` extends UnaryOperator {
      val name = "typeof"
    }
    case object `void` extends UnaryOperator {
      val name = "void"
    }
    case object `delete` extends UnaryOperator {
      val name = "delete"
    }
  }

  implicit def UnaryOperatorEncodeJson: EncodeJson[UnaryOperator] =
    EncodeJson((it: UnaryOperator) =>
      jString(it.name))


  case class UpdateExpression(operator: UpdateOperator, argument: Expression, prefix: Boolean) extends Expression {
    val `type` = "UpdateExpression"
  }

  implicit def UpdateExpressionEncodeJson: EncodeJson[UpdateExpression] =
    jencode4L {it: UpdateExpression => (it.`type`, it.operator, it.argument, it.prefix)}("type", "operator", "argument", "prefix")

  sealed trait UpdateOperator {
    def name: String
  }

  object UpdateOperator {
    case object `++` extends UpdateOperator {
      val name = "++"
    }
    case object `--` extends UpdateOperator {
      val name = "--"
    }
  }

  implicit def UpdateOperatorEncodeJson: EncodeJson[UpdateOperator] = EncodeJson{it: UpdateOperator =>
    it match {
      case UpdateOperator.`++` => jString("++")
      case UpdateOperator.`--` => jString("--")
    }
  }

  sealed trait BinaryOperator {
    def name: String
  }

  implicit def BinaryOperatorEncodeJson: EncodeJson[BinaryOperator] =
    EncodeJson((it: BinaryOperator) =>
      jString(it.name))

  object BinaryOperator {
    case object `==` extends BinaryOperator {
      val name = "=="
    }

    case object `!=` extends BinaryOperator {
      val name = "!="
    }

    case object `===` extends BinaryOperator {
      val name = "==="
    }

    case object `!==` extends BinaryOperator {
      val name = "!=="
    }
    case object `<` extends BinaryOperator {
      val name = "<"
    }
    case object `<=` extends BinaryOperator {
      val name = "<="
    }
    case object `>` extends BinaryOperator {
      val name = ">"
    }
    case object `>=` extends BinaryOperator {
      val name = ">="
    }
    case object `<<` extends BinaryOperator {
      val name = "<<"
    }
    case object `>>` extends BinaryOperator {
      val name = ">>"
    }
    case object `>>>` extends BinaryOperator {
      val name = ">>>"
    }
    case object `+` extends BinaryOperator {
      val name = "+"
    }
    case object `-` extends BinaryOperator {
      val name = "-"
    }
    case object `*` extends BinaryOperator {
      val name = "*"
    }
    case object `/` extends BinaryOperator {
      val name = "/"
    }
    case object `%` extends BinaryOperator {
      val name = "%"
    }
    case object `|` extends BinaryOperator {
      val name = "|"
    }
    case object `^` extends BinaryOperator {
      val name = "^"
    }
    case object `&` extends BinaryOperator {
      val name = "&"
    }
    case object `in` extends BinaryOperator {
      val name = "in"
    }
    case object `instanceof` extends BinaryOperator {
      val name = "instanceof"
    }
  }


  case class BinaryExpression(operator: BinaryOperator, left: Expression, right: Expression) extends Expression {
    val `type` = "BinaryExpression"
  }

  implicit def BinaryExpressionEncodeJson: EncodeJson[BinaryExpression] =
    jencode4L{it: BinaryExpression => (it.`type`, it.operator, it.left, it.right)}("type", "operator", "left", "right")

  case class AssignmentExpression(operator: AssignmentOperator, left: Expression, right: Expression) extends Expression {
    val `type` = "AssignmentExpression"
  }

  implicit def AssignmentExpressionEncodeJson: EncodeJson[AssignmentExpression] =
    jencode4L{it: AssignmentExpression => (it.`type`, it.operator, it.left, it.right)}("type", "operator", "left", "right")

  sealed trait AssignmentOperator {
    def name: String
  }

  object AssignmentOperator {
    case object `=` extends AssignmentOperator {
      val name = "="
    }
    case object `+=` extends AssignmentOperator {
      val name = "+="
    }
    case object `-=` extends AssignmentOperator {
      val name = "-="
    }
    case object `*=` extends AssignmentOperator {
      val name = "*="
    }
    case object `/=` extends AssignmentOperator {
      val name = "/="
    }
    case object `%=` extends AssignmentOperator {
      val name = "%="
    }
    case object `<<=` extends AssignmentOperator {
      val name = "<<="
    }
    case object `>>=` extends AssignmentOperator {
      val name = ">>="
    }
    case object `>>>=` extends AssignmentOperator {
      val name = ">>>="
    }
    case object `|=` extends AssignmentOperator {
      val name = "|="
    }
    case object `^=` extends AssignmentOperator {
      val name = "^="
    }
    case object `&=` extends AssignmentOperator {
      val name = "&="
    }
  }

  implicit def AssignmentOperatorEncodeJson: EncodeJson[AssignmentOperator] =
    EncodeJson{it: AssignmentOperator => jString(it.name)}

  case class LogicalExpression(operator: LogicalOperator, left: Expression, right: Expression) extends Expression {
    val `type` = "LogicalExpression"
  }

  implicit def LogicalExpressionEncodeJson: EncodeJson[LogicalExpression] =
    jencode4L{it: LogicalExpression => (it.`type`, it.operator, it.left, it.right)}("type", "operator", "left", "right")

  case class MemberExpression(`object`: Expression, property: Expression, computed: Boolean) extends Expression {
    val `type` = "MemberExpression"
  }

  implicit def MemberExpressionEncodeJson: EncodeJson[MemberExpression] =
    jencode4L{it: MemberExpression => (it.`type`, it.`object`, it.property, it.computed)}("type", "object", "property", "computed")

  case class ConditionalExpression(test: Expression, alternate: Expression, consequent: Expression) extends Expression {
    val `type` = "ConditionalExpression"
  }

  implicit def ConditionalExpressionEncodejJson: EncodeJson[ConditionalExpression] =
    jencode4L{it: ConditionalExpression => (it.`type`, it.test, it.alternate, it.consequent)}("type", "test", "alternate", "consequent")

  sealed trait BaseCallExpression extends Expression {
    val `type` = "CallExpression"
    def arguments: Seq[Expression]
    def callee: Expression
  }

  case class CallExpression(callee: Expression, arguments: Seq[Expression]) extends BaseCallExpression

  implicit def CallExpressionEncodeJson: EncodeJson[CallExpression] =
    jencode3L{it: CallExpression => (it.`type`, it.callee, it.arguments)}("type", "arguments", "callee")

  case class NewExpression(override val callee: Expression, override val arguments: Seq[Expression]) extends BaseCallExpression {
    override val `type` = "NewExpression"
  }

  implicit def NewExpressionEncodeJson: EncodeJson[NewExpression] =
    jencode3L{it: NewExpression => (it.`type`, it.callee, it.arguments)}("type", "callee", "arguments")

  case class SequenceExpression(expressions: Seq[Expression]) extends Expression {
    val `type` = "SequenceExpression"
  }

  implicit def SequenceExpressionEncodeJson: EncodeJson[SequenceExpression] =
    jencode2L{it: SequenceExpression => (it.`type`, it.expressions)}("type", "expressions")

  sealed trait LogicalOperator {
    def name: String
  }

  implicit def LogicalOperatorEncodeJson: EncodeJson[LogicalOperator] =
    EncodeJson{it: LogicalOperator =>
      it match {
        case LogicalOperator.`||` => jString("||")
        case LogicalOperator.`&&` => jString("&&")
      }
    }

  object LogicalOperator {
    case object `||` extends LogicalOperator {
      val name = "||"
    }
    case object `&&` extends LogicalOperator {
      val name = "&&"
    }
  }

  sealed trait PropertyKind {
    def name: String
  }
  object PropertyKind {
    case object Init extends PropertyKind {
      val name = "init"
    }
    case object Get extends PropertyKind {
      val name = "get"
    }
    case object Set extends PropertyKind {
      val name = "set"
    }
  }

  implicit def PropertyKindEncodeJson: EncodeJson[PropertyKind] =
    EncodeJson((it: PropertyKind) =>
      jString(it.name))

  type Pattern = Identifier

  sealed trait Statement extends Node
}
