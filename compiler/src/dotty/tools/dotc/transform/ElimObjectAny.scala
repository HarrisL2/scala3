package dotty.tools
package dotc
package transform

import ast.Trees
import core.Phases.*
import core.Contexts.*
import core.Types.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.core.Symbols.*

class ElimObjectAny extends MiniPhase with InfoTransformer{
  import ast.tpd._
  
  override def phaseName: String = ElimObjectAny.name

  override def description: String = ElimObjectAny.description

  def elimObjectAny(tp: Type)(using Context): Type = 
    new TypeMap {
      def apply(tp: Type): Type = 
        if tp eq defn.ObjectAnyType then
          // println(s"ElimObjectAny: visiting type $tp")
          defn.ObjectType
        else
          mapOver(tp)
    }.apply(tp)

  override def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
    elimObjectAny(tp)

  override def transformTypeTree(tree: TypeTree)(using Context): Tree =
    val newType = elimObjectAny(tree.tpe)
    if (newType ne tree.tpe) then tree.withType(newType)
    else tree
  // def changeObjectAnyToObject(tp: Type)(using Context): Type = {
  //   tp match 
  //       case tr: TypeRef if tr.symbol == defn.ObjectAnySymbol =>
  //         defn.ObjectType
  //       case _ => tp
  // }
}

object ElimObjectAny {
  val name: String = "elimObjectAny"
  val description: String = "Eliminate ObjectAny to Object"
}