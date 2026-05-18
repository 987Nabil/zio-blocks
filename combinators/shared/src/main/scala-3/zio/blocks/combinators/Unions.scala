/*
 * Copyright 2024-2026 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.blocks.combinators

import scala.reflect.TypeTest

/**
 * Union operations: combining values into flat union types and separating them.
 */
object Unions {
  trait Unions[L, R] {
    type Out

    def combine(either: Either[L, R]): Out

    def separate(out: Out): Either[L, R]
  }

  object Unions {
    type WithOut[L, R, O] = Unions[L, R] { type Out = O }

    inline given unions[L, R](using tt: TypeTest[L | R, R]): WithOut[L, R, L | R] =
      new UnionInstance[L, R]
  }

  private[combinators] class UnionInstance[L, R](using tt: TypeTest[L | R, R]) extends Unions[L, R] {
    type Out = L | R

    def combine(either: Either[L, R]): L | R = either match {
      case Left(l)  => l
      case Right(r) => r
    }

    def separate(out: L | R): Either[L, R] = out match {
      case tt(r) => Right(r)
      case _     => Left(out.asInstanceOf[L])
    }
  }

  def combine[L, R](either: Either[L, R])(using u: Unions[L, R]): u.Out = u.combine(either)
}
