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

class ErasurePreservation extends MiniPhase {

  override def phaseName: String = ErasurePreservation.name

  override def description: String = ErasurePreservation.description

  def getOuterTyParamSyms(sym: Symbol)(using Context): List[Symbol] =
    if !sym.exists then List()
    else getOuterTyParamSyms(sym.owner) ++ (sym.paramSymss.flatten)

  def typeParamToTypeBKM(tr: TypeRef, sourceSym: Symbol)(using Context): TypeB = trace.force(i"${tr}, ${tr.symbol.owner}"){
    val outerTyParams = getOuterTyParamSyms(sourceSym)
    val owner = tr.symbol.owner
    if owner.isClass then
      val ind = (outerTyParams++owner.typeParams).indexOf(tr.symbol)
      val n = debrujin(sourceSym.enclosingClass, owner)
      if ind != -1 then TypeB.K(n, ind) else TypeB.None
    else
      val ind = (outerTyParams++owner.paramSymss.flatten).indexWhere(tr.isRef(_))
      if ind != -1 then TypeB.M(ind) else TypeB.None
  }

  def typeParamToTypeAKM(tr: TypeRef, sourceSym: Symbol)(using Context): TypeA =
    val outerTyParams = getOuterTyParamSyms(sourceSym)
    val owner = tr.symbol.owner
    if owner.isClass then
      val ind = (outerTyParams++owner.typeParams).indexOf(tr.symbol)
      val n = debrujin(sourceSym.enclosingClass, owner)
      if ind != -1 then TypeA.K(n, ind) else ???
    else
      val ind = (outerTyParams++owner.paramSymss.flatten).indexWhere(tr.isRef(_))
      if ind != -1 then TypeA.M(ind) else ???

  def toTypeA(tp: Type, sourceSym: Symbol)(using Context): TypeA = trace(i"toTypeA ${tp}, ${sourceSym}") {tp.widen match
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
        typeParamToTypeAKM(tr, sourceSym)
      else TypeA.Ref
    case _ =>
      TypeA.Ref
  }

  def debrujin(source: Symbol, outer: Symbol)(using Context): Int = trace(i"debrujin: $source, $outer") {
    if (source.enclosingClass == outer) then 0
    else
      // println(s"$source, $outer, ${outer.owner}")
      debrujin(source.owner, outer)+1
  }

  def toTypeB(tp: Type, sourceSym: Symbol, method: Type)(using Context): TypeB = trace.force(i"toTypeB ${tp}, ${sourceSym}"){ tp match
    case tpr: TypeParamRef =>
      val outerTyParams = getOuterTyParamSyms(sourceSym)
      TypeB.M(outerTyParams.length+indexTypeParam(method, tpr))
    case tr: TypeRef if tr.symbol.isTypeParam =>
      typeParamToTypeBKM(tr, sourceSym)
    case _ => TypeB.None
  }

  def toReturnTypeB(tp: Type, sourceSym: Symbol)(using Context): TypeB = tp match
    case tr: TypeRef if tr.symbol.isTypeParam =>
      typeParamToTypeBKM(tr, sourceSym)
    case _ => TypeB.None

  def indexTypeParam(method: Type, tpr: TypeParamRef): Int = method match
    case pt: PolyType =>
      if tpr.binder == pt then
        tpr.paramNum
      else indexTypeParam(pt.resType, tpr) + pt.paramNames.size
    case mt: MethodType => indexTypeParam(mt.resType, tpr)
    case _ => ???

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = trace.force(i"transformDefDef $tree, ${tree.tpe}, ${tree.tpe.widen}"){
    val tup = tree.tpe.widen match
      case pt: PolyType => pt.resType match
        case mt: MethodType =>
          // println(mt.paramInfos)
          // println(mt.paramInfoss)
          // println(tree.tpe.widen)
          val ptParams = pt.paramInfoss.flatten
          Some((ptParams.size, mt.paramInfoss.flatten.map(p => toTypeB(p, ctx.owner, pt)), toTypeB(mt.resType, ctx.owner, pt)))
        case other =>
          Some((pt.paramInfoss.flatten.size, Nil, toTypeB(other.widenExpr, ctx.owner, pt)))
      case mt: MethodType =>
        val params = mt.paramInfos.map(p => toTypeB(p, ctx.owner, mt))
        val ret = toTypeB(mt.resType, ctx.owner, mt)
        if (params.exists(_ != TypeB.None) || ret != TypeB.None) then
          Some((0, params, ret))
        else
          None
      // case tr: TypeRef => Some((0, Nil, toTypeB(tr, ctx.owner)))
      case et: ExprType =>
        ???
        val ret = toTypeB(et.widenExpr, ctx.owner, ???)
        if (ret != TypeB.None) then
          Some((0, Nil, ret))
        else
          None
      case other => None
    if tup.isDefined then
      val outerTyParams = getOuterTyParamSyms(ctx.owner)
      val (paramCount, paramRefs, retType) = tup.get
      tree.putAttachment(MethodParameterReturnType, (paramCount+outerTyParams.length, paramRefs, retType))
    tree
  }

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = trace(i"transfromApply ${tree}") {
    tree.putAttachment(InvokeReturnType, toReturnTypeB(tree.tpe, ctx.owner))
    tree
  }

  override def transformTypeApply(tree: tpd.TypeApply)(using Context): tpd.Tree = trace(i"transfromTypeApply ${tree}") {
    val args = tree.args.map(_.tpe).map(p => toTypeA(p, ctx.owner))
    tree.fun.putAttachment(InstructionTypeArguments, args) // Pattern match args based on their types
    tree
  }

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
object MethodParameterReturnType extends Property.StickyKey[(Int, List[TypeB], TypeB)]

class ErasedInfo(val paramCount: Int, val paramType: List[TypeB], val returnType: TypeB) extends Annotation {
  override def tree(using Context) =
    tpd.New(defn.ErasurePreservationAnnot.typeRef,
            List(tpd.Literal(Constant(toString))))

  override def toString =
    s"$paramCount, $paramType, $returnType"
}
