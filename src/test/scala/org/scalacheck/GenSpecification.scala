package org.scalacheck

import scala.language.reflectiveCalls

object GenSpecification extends Properties("Gen") {

  import Prop._
  import Gen._

  val gens = oneOf(const(1), oneOf(2,3,4), const(0).repeatedly)

  property("setSize") = forAll(gens) { g =>
    forAll(oneOf(0,1,2,3)) { sz =>
      g.setSize(sz).flatMap(_ => size).map(_ ?== sz)
    }
  }

  property("setSeed") = forAll(gens) { g =>
    forAll(oneOf(0,100,200)) { s =>
      g.setSeed(s).flatMap(_ => seed).map(_ ?== s)
    }
  }

  property("just testing") = for {
    n <- oneOf(1,2,3)
    m <- oneOf(5,6,7)
  } yield boolToResult(n < m)

}
