package io.avici.talf.utils

import java.io.FileReader
import javax.script.{Invocable, ScriptEngine}

import argonaut.Json


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
class EscodegenWrapper(engine: ScriptEngine){
  val escodegenSrc = getClass.getResource("/escodegen.browser.js")
  val wrapperSrc = getClass.getResource("/escodegenWrapper.js")
  val fr = new FileReader(escodegenSrc.getFile)
  engine.eval(fr)
  engine.eval(new FileReader(wrapperSrc.getFile))
  def ast2js(ast: Json): String = {
    val invocable = engine.asInstanceOf[Invocable]
    val result = invocable.invokeFunction("codegen",ast.spaces2).asInstanceOf[String]
    return result
  }
}
