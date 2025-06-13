package dotty.tools
package dotc
package transform

import ast.Trees
import core.Phases.*
import core.DenotTransformers.*
import core.Annotations.*
import core.Denotations.*
import core.SymDenotations.*
import core.Symbols.*
import core.Contexts.*
import core.Types.*
import core.Names.*
import core.Constants.*
import core.Decorators.*
import typer.NoChecking
import ast.{tpd, untpd}
import reporting.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.util.Property

class ErasurePreservation extends MiniPhase with InfoTransformer {

  override def phaseName: String = ErasurePreservation.name

  override def description: String = ErasurePreservation.description

  def toTypeA(tp: Type)(using Context): TypeA = trace(s"toTypeA ${tp}") {tp match
    case tpr: TypeParamRef => TypeA.M(tpr.paramNum)
    case tr: TypeRef =>
      // println(tr.symbol.owner.paramSymss)
      if tr.isRef(defn.IntClass) then TypeA.Int else
      if tr.symbol.isTypeParam then
        val ind = tr.symbol.owner.paramSymss.head.indexWhere(tr.isRef(_))
        if ind != -1 then TypeA.M(ind)
        else TypeA.Ref
      else TypeA.Ref
    case _ => assert(false)
  }


  def toTypeB(tp: Type)(using Context): TypeB = tp match
    case tpr: TypeParamRef => TypeB.M(tpr.paramNum)
    case _ => TypeB.None

  def toReturnTypeB(tp: Type)(using Context): TypeB = tp match
    case tr: TypeRef =>
      // println(tr.symbol.owner.paramSymss)
      if tr.symbol.isTypeParam then
        val ind = tr.symbol.owner.paramSymss.head.indexWhere(tr.isRef(_))
        if ind != -1 then TypeB.M(ind)
        else TypeB.None
      else TypeB.None
    case _ => TypeB.None


  override def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
    tp match
        case pt: PolyType =>
          pt.resType match
            case mt: MethodType =>
              sym.addAnnotation(ErasedInfo(pt.paramInfos.size, mt.paramInfos.map(toTypeB), toTypeB(mt.resType)))
            case other =>
              sym.addAnnotation(ErasedInfo(pt.paramInfos.size, Nil, toTypeB(other.widenExpr)))
        case _ => ()
    tp

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    tree.putAttachment(InvokeReturnType, toReturnTypeB(tree.tpe))
    tree

  override def transformTypeApply(tree: tpd.TypeApply)(using Context): tpd.Tree =
    val args = tree.args.map(_.tpe).map(toTypeA)
    tree.fun.putAttachment(InstructionTypeArguments, args) // Pattern match args based on their types
    tree

}

object ErasurePreservation {
  val name: String = "erasure preservation"
  val description: String = "preserve information in annotations before erasure"
}

enum TypeA:
  case Int
  case M(x: Int)
  case K(x: Int)
  case Ref

enum TypeB:
  case None
  case M(x: Int)
// case class TypeB(tp: Type)

object InstructionTypeArguments extends Property.StickyKey[List[TypeA]]
object InvokeReturnType extends Property.StickyKey[TypeB]

class ErasedInfo(paramCount: Int, paramType: List[TypeB], returnType: TypeB) extends Annotation {
  override def tree(using Context) =
    tpd.New(defn.ErasurePreservationAnnot.typeRef,
            List(tpd.Literal(Constant(toString))))

  override def toString =
    s"$paramCount, $paramType, $returnType"
}
