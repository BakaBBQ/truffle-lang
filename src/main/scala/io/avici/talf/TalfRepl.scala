package io.avici.talf

import io.avici.talf.parser.{Statements, Expressions}

import scala.collection.immutable.HashMap

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
object TalfRepl extends App{
  val stmt = new Statements
  val parsers = HashMap(
    "All" -> stmt.singleInput,
    "Atom" -> Expressions.atom,
    "Test" -> Expressions.test,
    "Comparison" -> Expressions.comparison
  )
  println("Repl Running")

  var ok = true
  while (ok) {
    val ln = scala.io.StdIn.readLine()
    ok = (ln != null) && (ln != "")
    if (ok) {
      println("-----Parsed-----")
      for ( (k, v) <- parsers) {
        val result = v.parse(ln)
        println(s"$k -> $result")
      }
      println("-----------------")
    }
  }
}
