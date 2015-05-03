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

  def findVar1 = {
    collect {
      case x: Defn.Var => x.origin.start
    }.topDown 
  }
    def findVar2 = {
    collect[Set] {
      case Term.Assign(b: Term.Name, _) => b
    }.topDown feed { assign =>
      (collect {
        case x: Defn.Var => x.origin.start
      }).topDown
    }
  }

  def findCase1 = {
    collect {
      case x @ Defn.Class(mods, n: Type.Name, ref, ctor @ Ctor.Primary(_, _, args), bdy) if args.flatten.length == 0 && mods.contains(Mod.Case()) =>
        x.origin.start
    }.topDown
  }

  def findCase2 = {
    collect[Set] {
      case Term.Assign(b: Term.Name, _) => b
    }.topDown feed { assign =>
      collect {
        case x @ Defn.Class(mods, n: Type.Name, ref, ctor @ Ctor.Primary(_, _, args), bdy) if args.flatten.length == 0 && mods.contains(Mod.Case()) =>
          x.origin.start
      }.topDown
    }
  }

  assert(findVar1(tree).result.head == 28)
  assert(findVar2(tree).result.head == 28)
  assert(findCase1(tree).result.head == 48)
  assert(findCase2(tree).result.head == 48)
}