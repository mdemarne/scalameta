import scala.meta.tql._

import scala.meta.internal.ast._

import org.scalatest.FunSuite

class TQLOriginSuite extends FunSuite {
    val tree =  {
    import scala.meta._
    import scala.meta.dialects.Scala211
    """
      class Test {
        var y = "y"
        case class Me() {
          val x = "x"
        }
      }
    """.parse[Source]
  }


  val collectAllOrigin1 = {
    collect {
      case t: Tree => t
    }.topDown
  }
  val collectAllOrigin2 = {
    collect {
      case b: Term.Name => b
    }.topDown feed { assign =>
      collect {
        case t: Tree => t
      }.topDown
    }
  }

  val findVar1 = {
    collect {
      case x: Defn.Var => x.origin.start
    }.topDown 
  }
  val findVar2 = {
    collect {
      case b: Term.Name => b
    }.topDown feed { assign =>
      (collect {
        case x: Defn.Var => x.origin.start
      }).topDown
    }
  }

    val findVal1 = {
    collect {
      case x: Defn.Val => x.origin.start
    }.topDown 
  }
  val findVal2 = {
    collect {
      case b: Term.Name => b
    }.topDown feed { assign =>
      (collect {
        case x: Defn.Val => x.origin.start
      }).topDown
    }
  }

  val findCase1 = {
    collect {
      case x @ Defn.Class(mods, n: Type.Name, ref, ctor @ Ctor.Primary(_, _, args), bdy) if args.flatten.length == 0 && mods.contains(Mod.Case()) =>
        x.origin.start
    }.topDown
  }

  val findCase2 = {
    collect {
      case b: Term.Name => b
    }.topDown feed { assign =>
      collect {
        case x @ Defn.Class(mods, n: Type.Name, ref, ctor @ Ctor.Primary(_, _, args), bdy) if args.flatten.length == 0 && mods.contains(Mod.Case()) =>
          x.origin.start
      }.topDown
    }
  }

  assert(findVal1(tree).result.head == 76) // OK
  assert(findVal2(tree).result.head == 76) // OK
  assert(findVar1(tree).result.head == 28) // OK
  assert(findVar2(tree).result.head == 28) // NOT OK
  assert(findCase1(tree).result.head == 48) // OK
  assert(findCase2(tree).result.head == 48) // NOT OK

  val result1 = collectAllOrigin1(tree).result.map(_.origin.start)
  val result2 = collectAllOrigin2(tree).result.map(_.origin.start)
  assert(result1 == result2) // NOT OK
}