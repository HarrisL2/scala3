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
import dotty.tools.dotc.cc.pathOwner
import scala.annotation.tailrec

class ErasurePreservation extends MiniPhase with InfoTransformer {

  override def phaseName: String = ErasurePreservation.name

  override def description: String = ErasurePreservation.description

  def toTypeA(tp: Type, sourceSym: Symbol)(using Context): TypeA = trace(s"toTypeA ${tp}") {tp match
    case tpr: TypeParamRef => TypeA.M(tpr.paramNum)
    case tr: TypeRef =>
      // println(tr.symbol.owner.paramSymss)
      if tr.isRef(defn.ByteClass) then TypeA.Byte else
      if tr.isRef(defn.CharClass) then TypeA.Char else
      if tr.isRef(defn.DoubleClass) then TypeA.Double else
      if tr.isRef(defn.FloatClass) then TypeA.Float else
      if tr.isRef(defn.IntClass) then TypeA.Int else
      if tr.isRef(defn.LongClass) then TypeA.Long else
      if tr.isRef(defn.ShortClass) then TypeA.Short else
      if tr.isRef(defn.BooleanClass) then TypeA.Boolean else
      if tr.symbol.isTypeParam then
        val owner = tr.symbol.owner
        if owner.isClass then
          val ind = owner.typeParams.indexOf(tr.symbol)
          val n = debrujin(sourceSym.enclosingClass, owner)
          if ind != -1 then TypeA.K(n, ind) else ???
        else
          val ind = owner.paramSymss.headOption match
            case None => assert(false, i"Got unexpected type ${tp}")
            case Some(value) => value.indexWhere(tr.isRef(_))
          if ind != -1 then TypeA.M(ind) else ???
      else TypeA.Ref
    case _ => assert(false)
  }

  def debrujin(source: Symbol, outer: Symbol)(using Context): Int = trace(i"debrujin: $source, $outer") {
    if (source.enclosingClass == outer) then 0
    else debrujin(source.owner, outer)+1
  }

  def toTypeB(tp: Type, sourceSym: Symbol)(using Context): TypeB = trace(i"toTypeB ${tp}"){ tp match
    case tpr: TypeParamRef => TypeB.M(tpr.paramNum)
    case tr: TypeRef if tr.symbol.isTypeParam =>
      val owner = tr.symbol.owner
      if owner.isClass then
        val ind = owner.typeParams.indexOf(tr.symbol)
        val n = debrujin(sourceSym.enclosingClass, owner)
        if ind != -1 then TypeB.K(n, ind) else TypeB.None
      else
        val ind = owner.paramSymss.headOption match
          case None => assert(false, i"Got unexpected type ${tp}")
          case Some(value) => value.indexWhere(tr.isRef(_))
        if ind != -1 then TypeB.M(ind) else TypeB.None
    case _ => TypeB.None
  }

  def toReturnTypeB(tp: Type, sourceSym: Symbol)(using Context): TypeB = tp match
    case tr: TypeRef if tr.symbol.isTypeParam =>
      val owner = tr.symbol.owner
      if owner.isClass then
        val ind = owner.typeParams.indexOf(tr.symbol)
        val n = debrujin(sourceSym.enclosingClass, owner)
        if ind != -1 then TypeB.K(n, ind) else TypeB.None
      else
        val ind = owner.paramSymss.headOption match
          case None =>  assert(false, i"Got unexpected type ${tp}")
          case Some(value) => value.indexWhere(tr.isRef(_))
        if ind != -1 then TypeB.M(ind) else TypeB.None
    case _ => TypeB.None

  override def transformInfo(tp: Type, sym: Symbol)(using Context): Type = trace(i"transformInfo ${tp}, ${sym}") {
    tp match
        case pt: PolyType =>
          pt.resType match
            case mt: MethodType =>
              // println(i"sym context: $sym, ${sym.enclosingClass}, ${sym.enclosingClass.owner}")
              sym.addAnnotation(ErasedInfo(pt.paramInfos.size, mt.paramInfos.map(p => toTypeB(p, sym)), toTypeB(mt.resType, sym)))
            case other =>
              sym.addAnnotation(ErasedInfo(pt.paramInfos.size, Nil, toTypeB(other.widenExpr, sym)))
        case mt: MethodType =>
          val params = mt.paramInfos.map(p => toTypeB(p, sym))
          val ret = toTypeB(mt.resType, sym)
          if (params.exists(_ != TypeB.None) || ret != TypeB.None) then
            sym.addAnnotation(ErasedInfo(0, params, ret))
        case et: ExprType =>
          val ret = toTypeB(et.widenExpr, sym)
          if (ret != TypeB.None) then
            sym.addAnnotation(ErasedInfo(0, Nil, ret))
          ()
        case other =>
    tp
  }

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    tree.putAttachment(InvokeReturnType, toReturnTypeB(tree.tpe, tree.symbol))
    tree

  override def transformTypeApply(tree: tpd.TypeApply)(using Context): tpd.Tree =
    val args = tree.args.map(_.tpe).map(p => toTypeA(p, tree.symbol))
    tree.fun.putAttachment(InstructionTypeArguments, args) // Pattern match args based on their types
    tree

  // override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree =
  //   println(s"$tree")
  //   tree

}

object ErasurePreservation {
  val name: String = "erasure preservation"
  val description: String = "preserve information in annotations before erasure"
}

enum TypeA:
  case Byte
  case Char
  case Double
  case Float
  case Int
  case Long
  case Short
  case Boolean
  case M(paramNum: Int)
  case K(
    outer: Int,
    paramNum: Int)
  case Ref

enum TypeB:
  case None
  case M(paramNum: Int)
  case K(
    outer: Int,
    paramNum: Int)
// case class TypeB(tp: Type)

object InstructionTypeArguments extends Property.StickyKey[List[TypeA]]
object InvokeReturnType extends Property.StickyKey[TypeB]

class ErasedInfo(val paramCount: Int, val paramType: List[TypeB], val returnType: TypeB) extends Annotation {
  override def tree(using Context) =
    tpd.New(defn.ErasurePreservationAnnot.typeRef,
            List(tpd.Literal(Constant(toString))))

  override def toString =
    s"$paramCount, $paramType, $returnType"
}
