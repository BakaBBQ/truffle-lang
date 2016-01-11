package io.avici.talf

import io.avici.talf.ast._
import org.scalacheck._

import scala.util.Success
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
object TalfParserSpecification extends Properties("TalfParser"){
  val smallPosInt = Gen.choose(0,10000)
  property("numberParser") = Prop.forAll(smallPosInt){(x : Int) =>
    x < 1000000
  }
}
