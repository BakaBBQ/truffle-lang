package io.avici.talf

import io.avici.talf.parser.Statements
import io.avici.talf.utils.EscodegenWrapper
import org.scalacheck.Properties

import org.scalacheck._
import jdk.nashorn.api.scripting.NashornScriptEngineFactory
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
object TalfCompilerSpec extends Properties("TalfCompiler"){
  val simpleNumber = Gen.choose(0, 3000)
  val addition = for {
    x <- Gen.choose(0,1000)
    y <- Gen.choose(0,1000)
  } yield (x, y)
  lazy val engine = new NashornScriptEngineFactory().getScriptEngine
  val codegenWrapper = new EscodegenWrapper(engine)
  val compiler = new TalfCompiler

  property("simple_pos_numbers") = Prop.forAll(simpleNumber){(x : Int) =>
    val ast = compiler.compile(x.toString).get
    val js = codegenWrapper.ast2js(ast)
    (engine.eval(js).asInstanceOf[Int] == x)
  }

  property("simple_addition") = Prop.forAll(addition){(it : (Int, Int)) =>
    val (x, y) = it
    val exp = s"$x+$y"
    val js = codegenWrapper.ast2js(compiler.compile(exp).get)
    (engine.eval(js).asInstanceOf[Int] == x + y)
  }

  property("experimentalParser") = Prop.forAll(simpleNumber){(x : Int) =>
    val p = new Statements
    val pR = p.singleInput.parse("x = 3")
    pR match {
      case fastparse.core.Parsed.Failure(_,_,_) => false
      case fastparse.core.Parsed.Success(_,_) => true
      case _ => false
    }
  }
}
