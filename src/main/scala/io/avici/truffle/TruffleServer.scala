package io.avici.truffle

/**
  * Created by Baqiao (Charles) Liu on 1/10/2016.
  */

/**
  * Copyright 2016 Baqiao (Charles) Liu
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *   http://www.apache.org/licenses/LICENSE-2.0
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */


import com.twitter.finagle.param.Stats
import com.twitter.finagle.{Service, Http}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.{Time, Await}

import io.finch._
import io.finch.circe._
import io.circe.generic.auto._

import scala.util.{Failure, Success}


object TruffleServer extends App {

  case class Input(code: String)
  case class Output(ast: String)
  case class Empty()

  val compile: Endpoint[Output] = post("compile" ? body.as[Input]) {i : Input =>
    val compiler = new TruffleCompiler
    val res = compiler.compile(i.code)
    val mes = res match {
      case Success(x) => x.spaces2
      case Failure(_) => "Failed"
    }
    Ok(Output(mes)).withHeader("Access-Control-Allow-Origin", "*")
  }

  val compileOptions: Endpoint[Empty] = options("compile") {
    Ok(Empty())
      .withHeader("Access-Control-Allow-Origin", "*")
      .withHeader("Access-Control-Allow-Methods", "GET, POST")
      .withHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")
  }

  val api: Service[Request, Response] = (
    compile :+: compileOptions
    ).toService

  val server = Http.server
    .serve(s":${sys.env("PORT")}", api)

  Await.ready(server)
}