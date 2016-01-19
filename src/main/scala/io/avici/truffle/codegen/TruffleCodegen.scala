package io.avici.truffle.codegen

import io.avici.truffle.ast._
import io.circe.{Encoder, Json}

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

import io.avici.truffle.ast.Ast._
import io.circe._
import io.circe.syntax._
class TruffleCodegen {
  import Json._

  type TruffleProgram = Seq[Ast.Stmt]

  def codegen(ast : Expr) : Json = {
    Map("a" -> "b").asJson
  }
}
