/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2015 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import scala.annotation.tailrec

object Test {

  val minSuccess = 100
  val maxDiscardRatio = 5.0

  sealed trait Result[T]
  case class Exhausted[T]() extends Result[T]
  case class Proved[T]() extends Result[T]
  case class Passed[T]() extends Result[T]
  case class Failed[T](ev: T, ev0: Option[T]) extends Result[T]
  case class PropException[T](ev: Option[T], e: Throwable) extends Result[T]

  def count[T](g: Gen[T]): Gen[(Int,Int,Option[T])] = {
    def c(s: Int, d: Int, g: Gen[T]): Gen[(Int,Int,Option[T])] =
      g.withNext flatMap { case (t,ng) =>
        val (ss,dd) = if(t.isDefined) (s+1,d) else (s,d+1)
        ng match {
          case None => Gen.const((ss,dd,t))
          case Some(gg) => Gen.const((ss,dd,t)) setNext c(ss,dd,gg)
        }
      }
    c(0,0,g)
  }

  import Gen.const

  def check2[T](g: Gen[Prop.Result[T]]): Gen[Result[T]] = {

    def ck(s: Int, d: Int, g: Gen[Prop.Result[T]]): Gen[Result[T]] =
      g.withNext flatMap {
        case (Some(Prop.False(ev,ev0)), _) => const(Failed(ev,ev0))
        case (Some(Prop.Throw(ev,ev0)), _) => const(PropException(ev,ev0))
        case (Some(Prop.True()), _) if s >= minSuccess => const(Passed())
        case (_, _) if s+d >= minSuccess && d > s*maxDiscardRatio =>
          const(Exhausted())
        case (_, None) => const(if(s > 0 && d == 0) Proved() else Exhausted())
        case (t, Some(ng)) =>
          Gen.fail.setNext(if(t.isDefined) ck(s+1,d,ng) else ck(s,d+1,ng))
      }

    ck(0,0,g)

  }

/*
  def check2[T](g: Gen[Prop.Result[T]]): Gen[Result[T]] = count(g).withNext flatMap {
    case (None, _) => error("BUG: count() should never fail")
    case (Some(x),ng) => (x,ng) match {
      case ((_,_,Some(Prop.False(ev,ev0))), _) => const(Failed(ev,ev0))
      case ((_,_,Some(Prop.Throw(ev,ev0))), _) => const(PropException(ev,ev0))
      case ((s,_,Some(Prop.True())), _) if s >= minSuccess => const(Passed())
      case ((s,d,_), _) if s+d >= minSuccess && d > s*maxDiscardRatio =>
        const(Exhausted())
      case ((s,d,_), None) if s > 0 && d == 0 => const(Proved())
      case (_, None) => const(Exhausted())
      case (_, Some(ng)) => ng
    }
  }
*/

  def check[T](g: Gen[Prop.Result[T]]): Gen[Result[T]] = g.iterate.map { it =>
    @tailrec def res(s: Int, d: Int): Result[T] = {
      if (s >= minSuccess) Passed()
      else if (s+d >= minSuccess && d > s*maxDiscardRatio) Exhausted()
      else if (it.hasNext) it.next().value match {
        case Some(Prop.True()) => res(s+1,d)
        case Some(Prop.False(ev,ev0)) => Failed(ev,ev0)
        case Some(Prop.Throw(ev,e)) => PropException(ev,e)
        case None => res(s,d+1)
      }
      else if (s > 0 && d == 0) Proved()
      else Exhausted()
    }

    res(0, 0)
  }

}
