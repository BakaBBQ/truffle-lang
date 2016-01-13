package io.avici.talf.parser

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
object Lexical {
  import fastparse.all._
  import io.avici.talf.ast.Ast

  val comment = P( "--" ~ CharsWhile(_ != '\n', min = 0))
  val lowercase = P( CharIn('a' to 'z') )
  val uppercase = P( CharIn('A' to 'Z') )
  val digit = P( CharIn('0' to '9' ) )
  val letter = P( lowercase | uppercase )
  val identifier: P[Ast.Identifier] = {
    P((letter|"_") ~ (letter | digit | "_").rep)
      .!.filter(!keywordList.contains(_)).map(Ast.Identifier)
  }

  def kw(s: String) = s ~ !(letter | digit | "_")

  val stringliteral: P[String] = P( shortstring )

  val nonzerodigit: P0 = P( CharIn('1' to '9') )
  val decimalinteger: P[BigInt] = P( nonzerodigit ~ digit.rep | "0" ).!.map(scala.BigInt(_))

  val shortstring: P[String] = P( shortstring0("'") | shortstring0("\"") )
  def shortstring0(delimiter: String) = P( delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
  def shortstringitem(quote: String): P0 = P( shortstringchar(quote) |  escapeseq)
  def shortstringchar(quote: String): P0 = P( CharsWhile(!s"\\\n${quote(0)}".contains(_)) )
  val escapeseq: P0 = P( "\\" ~ AnyChar )

  val keywordList = Set(
    "def"
  )
}
