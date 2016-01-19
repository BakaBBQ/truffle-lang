package io.avici.truffle

import io.circe.Json
import io.avici.truffle.ast.Ast.Expr
import io.avici.truffle.codegen.{TruffleCodegen}

import scala.util.{Success, Try}


/**
  * Created by Baqiao (Charles) Liu on 1/11/2016.
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
class TruffleCompiler {
  def compile(str : String) : Try[Json] = {
    val parser = new TruffleParser()
    val ast: Try[Expr] = Success(parser.parse(str))
    val codegen = new TruffleCodegen()
    ast.map(codegen.codegen)
  }
}