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

  def toTypeA(tp: Type, outers: List[List[Symbol]])(using Context): TypeA = trace(i"toTypeA ${tp}"){ tp.widen match
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
        def search(tr: TypeRef, depth: Int, outers: List[List[Symbol]]): TypeA =
          outers.headOption match
            case Some(value) => value.indexOf(tr.symbol) match
              case -1 =>
                search(tr, depth+1, outers.tail)
              case ind =>
                if depth != 0 then
                  TypeA.K(depth-1, ind)
                else
                  TypeA.M(ind)
            case None =>
              TypeA.Ref
        search(tr, 0, outers)
      else TypeA.Ref
    case _ =>
      TypeA.Ref
  }

  def indexTypeParam(method: Type, tpr: TypeParamRef): Int = method match
    case pt: PolyType =>
      if tpr.binder == pt then
        tpr.paramNum
      else indexTypeParam(pt.resType, tpr) + pt.paramNames.size
    case mt: MethodType => indexTypeParam(mt.resType, tpr)
    case _ => ???


  def toTypeB(tp: Type, outers: List[List[Symbol]], isConstructor: Boolean = false)(using Context): TypeB = trace(i"toTypeB ${tp}, ${outers}"){ tp match
    case tpr: TypeParamRef =>
      TypeB.M(outers.head.indexWhere(sym => sym.name == tpr.paramName))
    case tr: TypeRef if tr.symbol.isTypeParam =>
      def search(tr: TypeRef, depth: Int, outers: List[List[Symbol]]): TypeB =
        outers.headOption match
          case Some(value) => value.indexOf(tr.symbol) match
            case -1 =>
              search(tr, depth+1, outers.tail)
            case ind =>
              if depth != 0 then
                if isConstructor then
                  TypeB.K(depth, ind)
                else
                  TypeB.K(depth-1, ind)
              else
                TypeB.M(ind)
          case None => TypeB.None
      search(tr, 0, outers)
    case at: AppliedType if at.tycon.isRef(defn.ArrayClass) =>
      TypeB.Array(toTypeB(at.args.head, outers))
    case _ => TypeB.None
  }

  def toReturnTypeB(tp: Type, outers: List[List[Symbol]])(using Context): TypeB = tp match
    case tr: TypeRef if tr.symbol.isTypeParam =>
      def search(tr: TypeRef, depth: Int, outers: List[List[Symbol]]): TypeB =
        outers.headOption match
          case Some(value) => value.indexOf(tr.symbol) match
            case -1 =>
              search(tr, depth+1, outers.tail)
            case ind =>
              if depth != 0 then
                TypeB.K(depth-1, ind)
              else
                TypeB.M(ind)
          case None => TypeB.None
      search(tr, 0, outers)
    case at: AppliedType if at.tycon.isRef(defn.ArrayClass) =>
      TypeB.Array(toTypeB(at.args.head, outers))
    case _ => TypeB.None


  /**
   * Return all outer type parameters that originate from a method
   * until we reach a class
   */
  def getOuterParamss(sym: Symbol, isConstructor: Boolean)(using Context): List[List[Symbol]] = trace(i"getOuterParamss ${sym}") {
    if !sym.exists then List(List())
    else if sym.isClass && isConstructor then
      // println(sym.typeParams)
      val outers = getOuterParamss(sym.owner, false)
      (outers.head ++ sym.typeParams) :: outers.tail
    else if sym.isClass then
      val outers = getOuterParamss(sym.owner, false)
      List() :: (outers.head ++ sym.typeParams) :: outers.tail
    else
      val tyParams = sym.paramSymss.headOption
      val outers = getOuterParamss(sym.owner, false)
      tyParams match
        case Some(tps) =>
          (outers.head ++ sym.paramSymss.headOption.getOrElse(List())) :: outers.tail
        case None => outers
  }

  def methodToInfos(
    params: List[Type],
    resType: Type,
    tyParams: List[Symbol],
    isConstructor: Boolean)(using Context): Tuple3[Int, List[TypeB], TypeB] = trace(i"methodToInfos ${params}, ${resType}")
    {
    var outers = getOuterParamss(ctx.owner, isConstructor)
    if (!isConstructor)
      outers = outers.head ++ tyParams :: outers.tail
    val paramsTypeB: List[TypeB] = params.map(tp => toTypeB(tp, outers, isConstructor))
    val ret: TypeB = toTypeB(resType, outers, isConstructor)
    (outers.head.length, paramsTypeB, ret)
  }


  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = trace(i"transformDefDef $tree, ${tree.tpe}, ${tree.tpe.widen}"){
    val tup: Tuple3[Int, List[TypeB], TypeB] = tree.tpe.widen match
      case pt: PolyType => pt.resType match
        case mt: MethodType =>
          methodToInfos(mt.paramInfos, mt.resType, tree.symbol.paramSymss.head, tree.symbol.isConstructor)
        case other =>
          methodToInfos(Nil, other.widenExpr, tree.symbol.paramSymss.head, tree.symbol.isConstructor)
      case mt: MethodType =>
        methodToInfos(mt.paramInfos, mt.resType, Nil, false)
      case tr: TypeRef =>
        methodToInfos(Nil, tr, Nil, false)
      // case et: ExprType =>
      //   ???
      case other =>
        (0, Nil, TypeB.None)
    val (paramCount, paramRefs, retType) = tup
    if (paramCount != 0 || paramRefs.length > 0 || retType != TypeB.None) then
      tree.putAttachment(MethodParameterReturnType, (paramCount, paramRefs, retType))
    tree
  }

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = trace(i"transfromApply ${tree}") {
    val outers = getOuterParamss(ctx.owner, false)
    toReturnTypeB(tree.tpe, outers) match
      case TypeB.None => 
      case other => tree.putAttachment(InvokeReturnType, other)
    tree
  }

  override def transformIdent(tree: tpd.Ident)(using Context): tpd.Tree = trace(i"transfromIdent ${tree}, ${tree.tpe.widen}") {
    val outers = getOuterParamss(ctx.owner, false)
    toReturnTypeB(tree.tpe.widen, outers) match
      case TypeB.None => 
      case other => tree.putAttachment(InvokeReturnType, other)
    tree
  }
  
  override def transformSelect(tree: tpd.Select)(using Context): tpd.Tree = trace(i"transfromSelect ${tree}, ${tree.tpe.widen}") {
    val outers = getOuterParamss(ctx.owner, false)
    toReturnTypeB(tree.tpe.widen, outers) match
      case TypeB.None => 
      case other => tree.putAttachment(InvokeReturnType, other)
    tree
  }

  override def transformTypeApply(tree: tpd.TypeApply)(using Context): tpd.Tree = trace(i"transfromTypeApply ${tree}") {
    val outers = getOuterParamss(ctx.owner, false)
    val args = tree.args.map(_.tpe).map(p => toTypeA(p, outers))
    tree.fun.putAttachment(InstructionTypeArguments, args)
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
  case Array(tp: TypeB)
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
