package io.avici.truffle

import fastparse.core.Parsed
import io.avici.truffle.ast.Ast
import io.avici.truffle.parser.Expressions
import org.scalacheck.Properties
import org.scalacheck._

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
object ExpressionsSpec extends Properties("Expressions"){
  val randomStringLiteral = Gen.alphaStr

  property("stringLiteral2string") = Prop.forAll(randomStringLiteral){it: String =>
    val literal = "\"" + it + "\""
    val r = Expressions.atom.parse(literal)

    r match {
      case Parsed.Failure(_,_,_) => false
      case Parsed.Success(Ast.Expr.Str(x),_) => x == it
      case _ => false
    }
  }

  property("numLiteral2num") = Prop.forAll(Gen.numStr){it: String =>
    val literal = it
    val supposed = literal.toInt

    val r = Expressions.atom.parse(literal)

    r match {
      case Parsed.Failure(_,_,_) => false
      case Parsed.Success(Ast.Expr.Num(x),_) => x == supposed
      case _ => false
    }
  }

  val nonEmptyList = for (
    contents <- Gen.nonEmptyListOf(Gen.choose(1,1000))
  ) yield contents

  property("nonEmptyList") = Prop.forAll(nonEmptyList){it: List[Int] =>
    val literal = "[" + it.mkString(", ") + "]"
    val r = Expressions.list.parse(literal)
    r match {
      case Parsed.Failure(_,_,_) => false
      case Parsed.Success(Ast.Expr.List(x),_) => x == it
      case _ => false
    }
  }

  val simpleBinOpNums = for{
    j <- Gen.choose(0, 1000)
    k <- Gen.choose(0, 1000)
  } yield Tuple2[Int, Int](j, k)

  property("simpleAddition") = Prop.forAll(simpleBinOpNums){it: (Int, Int) =>
    it match {
      case (i1, i2) =>
        val literal = s"$i1 + $i2"
        val r = Expressions.arithExpr.parse(literal)
        r match {
          case Parsed.Failure(_,_,_) => false
          case Parsed.Success(Ast.Expr.BinOp(Ast.Expr.Num(x), Ast.Operator.Add, Ast.Expr.Num(y)),_) => {
            (x == i1) && (y == i2)
          }
          case _ => false
        }
    }
  }
}
