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
  
  override def transformApply(tree: Apply)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformSelect(tree: Select)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformIdent(tree: Ident)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformTyped(tree: Typed)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformBlock(tree: Block)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformIf(tree: If)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformTypeApply(tree: TypeApply)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformLiteral(tree: Literal)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformThis(tree: This)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformNew(tree: New)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformSuper(tree: Super)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformMatch(tree: Match)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformTry(tree: Try)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformReturn(tree: Return)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformSeqLiteral(tree: SeqLiteral)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

  override def transformInlined(tree: Inlined)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))
    
  override def transformAssign(tree: Assign)(using Context): Tree =
    tree.withType(elimObjectAny(tree.tpe))

}

object ElimObjectAny {
  val name: String = "elimObjectAny"
  val description: String = "Eliminate ObjectAny to Object"
}