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
import ast.{tpd, untpd}
import reporting.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.util.Property
import scala.annotation.tailrec
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.semanticdb.SymbolInformation.Property.VAL.isVal

class ErasurePreservation extends MiniPhase {

  override def phaseName: String = ErasurePreservation.name

  override def description: String = ErasurePreservation.description

  def decomposeType(tp: Type, offset: Int = 0, map: Map[Type, Int] = Map.empty)(using Context): (List[Name], List[Type], Type, Map[Type, Int]) = tp match {
    case pt: PolyType =>
      val newMap = map + (pt -> offset)
      val (restNames, restParams, ret, finalMap) = decomposeType(pt.resType, offset + pt.paramNames.size, newMap)
      (pt.paramNames ++ restNames, restParams, ret, finalMap)
    case mt: MethodType =>
      val (restNames, restParams, ret, finalMap) = decomposeType(mt.resType, offset, map)
      (restNames, mt.paramInfos ++ restParams, ret, finalMap)
    case other =>
      (Nil, Nil, other, map)
  }

  // get all the type parameters from the class and its outer classes
  // copied from AddReifiedTypes, should be the same as it
  def getAllTypeParams(cls: Symbol)(using Context): List[Symbol] = {
      val own = cls.typeParams
      val outers = cls.ownersIterator.drop(1).takeWhile(
          cl => cl.isClass && !cl.is(Flags.Module) && !cl.is(Flags.Package)
      ).flatMap(_.typeParams).toList
      own ++ outers
  }

  def toTypeA(tp: Type, classTypeParams: List[Name], methodTypeParams: List[Name], offsets: Map[Type, Int])(using Context): TypeA = trace(i"toTypeA ${tp}"){ tp.widen match
    case tpr: TypeParamRef => 
      val offset = offsets.getOrElse(tpr.binder, 0)
      TypeA.M(offset + tpr.paramNum)
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
        val mIndex = methodTypeParams.indexOf(tr.name)
        if mIndex != -1 then TypeA.M(mIndex)
        else
          classTypeParams.indexOf(tr.name) match
            case -1 => 
              report.error(s"Could not find type parameter ${tr.name} in ${classTypeParams} and " +
                s"${methodTypeParams} for type ref ${tr}", ctx.source.atSpan(ctx.owner.span))
              TypeA.Ref
            case ind => TypeA.K(0, ind)
      else TypeA.Ref
    case _ =>
      TypeA.Ref
  }

  def toTypeB(tp: Type, classTypeParams: List[Name], methodTypeParams: List[Name], offsets: Map[Type, Int], isConstructor: Boolean = false)(using Context): TypeB = trace(i"toTypeB ${tp}, ${classTypeParams}"){ tp match
    case tpr: TypeParamRef => 
      val offset = offsets.getOrElse(tpr.binder, 0)
      TypeB.M(offset + tpr.paramNum)
    case tr: TypeRef if tr.symbol.isTypeParam =>
      val mIndex = methodTypeParams.indexOf(tr.name)
      if mIndex != -1 then TypeB.M(mIndex)
      else
        classTypeParams.indexOf(tr.name) match
          case -1 => 
            report.error(s"Could not find type parameter ${tr.name} in ${classTypeParams} and " +
              s"${methodTypeParams} for type ref ${tr}", ctx.source.atSpan(ctx.owner.span))
            TypeB.None
          case ind => TypeB.K(0, ind)
    // check if the return type is a primitive
    case tr: TypeRef =>
      if tr.isRef(defn.ByteClass) then TypeB.Byte else
      if tr.isRef(defn.CharClass) then TypeB.Char else
      if tr.isRef(defn.DoubleClass) then TypeB.Double else
      if tr.isRef(defn.FloatClass) then TypeB.Float else
      if tr.isRef(defn.IntClass) then TypeB.Int else
      if tr.isRef(defn.LongClass) then TypeB.Long else
      if tr.isRef(defn.ShortClass) then TypeB.Short else
      if tr.isRef(defn.BooleanClass) then TypeB.Boolean else
      TypeB.None
    case at: AppliedType if at.tycon.isRef(defn.ArrayClass) =>
      TypeB.Array(toTypeB(at.args.head, classTypeParams, methodTypeParams, offsets, isConstructor))
    case _ => TypeB.None
  }

  def toReturnTypeB(tp: Type, classTypeParams: List[Name], methodTypeParams: List[Name], offsets: Map[Type, Int])(using Context): TypeB = tp match
    case tpr: TypeParamRef => 
      val offset = offsets.getOrElse(tpr.binder, 0)
      TypeB.M(offset + tpr.paramNum)
    case tr: TypeRef if tr.symbol.isTypeParam =>
      val mIndex = methodTypeParams.indexOf(tr.name)
      if mIndex != -1 then TypeB.M(mIndex)
      else
        classTypeParams.indexOf(tr.name) match
          case -1 => 
            report.error(s"Could not find type parameter ${tr.name} in ${classTypeParams} and " +
              s"${methodTypeParams} for type ref ${tr}", ctx.source.atSpan(ctx.owner.span))
            TypeB.None
          case ind => TypeB.K(0, ind)
    // check if the return type is a primitive
    case tr: TypeRef =>
      if tr.isRef(defn.ByteClass) then TypeB.Byte else
      if tr.isRef(defn.CharClass) then TypeB.Char else
      if tr.isRef(defn.DoubleClass) then TypeB.Double else
      if tr.isRef(defn.FloatClass) then TypeB.Float else
      if tr.isRef(defn.IntClass) then TypeB.Int else
      if tr.isRef(defn.LongClass) then TypeB.Long else
      if tr.isRef(defn.ShortClass) then TypeB.Short else
      if tr.isRef(defn.BooleanClass) then TypeB.Boolean else
      TypeB.None
    case at: AppliedType if at.tycon.isRef(defn.ArrayClass) =>
      TypeB.Array(toTypeB(at.args.head, classTypeParams, methodTypeParams, offsets))
    case _ => TypeB.None

  def getAllClassParams(sym: Symbol)(using Context): List[Name] = {
    val cls = if sym.isClass then sym else sym.owner.enclosingClass
    if cls.isClass then 
      val allSyms = getAllTypeParams(cls)
      allSyms.map(_.name)
    else Nil
  }

  def getAllMethodParams(using Context): (List[Name], Map[Type, Int]) = {
    val sym = ctx.owner.enclosingMethod
    if sym.exists && !sym.isClass then 
      val (allTypeNames, _, _, map) = decomposeType(sym.info)
      (allTypeNames, map)
    else (Nil, Map.empty)
  }

  // whether the typeB is valid (not None or Array(None))
  def isValid(t: TypeB): Boolean = t match {
    case TypeB.None => false
    case TypeB.Array(tp) => isValid(tp)
    case _ => true
  } 

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree = trace(i"transformDefDef $tree, ${tree.tpe}, ${tree.tpe.widen}"){
    val (allTypeNames, allValueParams, resType, offsets) = decomposeType(tree.tpe.widen)
    // gets all type params of nested classes
    val capturedClassParams = getAllClassParams(tree.symbol.owner.enclosingClass)
    val paramRefs = allValueParams.map(tp => toTypeB(tp, capturedClassParams, allTypeNames, offsets, tree.symbol.isConstructor))
    val retType = toTypeB(resType, capturedClassParams, allTypeNames, offsets, tree.symbol.isConstructor)
    val paramCount = allTypeNames.length
    
    if (paramCount != 0 || paramRefs.exists(isValid) || isValid(retType)) then
      tree.putAttachment(MethodParameterReturnType, (paramCount, paramRefs, retType))
      ErasurePreservation.methodParameterReturnTypeMap.put(tree.symbol, (paramCount, paramRefs, retType))
    tree
  }

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = trace(i"transfromApply ${tree}") {
    val capturedClassParams = getAllClassParams(ctx.owner.enclosingClass)
    val (methodTypeParams, offsets) = getAllMethodParams
    val retTypeB = toReturnTypeB(tree.tpe, capturedClassParams, methodTypeParams, offsets)
    if isValid(retTypeB) then tree.putAttachment(InvokeReturnType, retTypeB)
    tree
  }

  override def transformIdent(tree: tpd.Ident)(using Context): tpd.Tree = trace(i"transfromIdent ${tree}, ${tree.tpe.widen}") {
    val capturedClassParams = getAllClassParams(ctx.owner.enclosingClass)
    val (methodTypeParams, offsets) = getAllMethodParams
    val retTypeB = toReturnTypeB(tree.tpe.widen, capturedClassParams, methodTypeParams, offsets)
    if isValid(retTypeB) then tree.putAttachment(InvokeReturnType, retTypeB)
    tree
  }
  
  override def transformSelect(tree: tpd.Select)(using Context): tpd.Tree = trace(i"transfromSelect ${tree}, ${tree.tpe.widen}") {
    val capturedClassParams = getAllClassParams(ctx.owner.enclosingClass)
    val (methodTypeParams, offsets) = getAllMethodParams
    val retTypeB = toReturnTypeB(tree.tpe.widen, capturedClassParams, methodTypeParams, offsets)
    if isValid(retTypeB) then tree.putAttachment(InvokeReturnType, retTypeB)
    tree
  }

  override def transformTypeApply(tree: tpd.TypeApply)(using Context): tpd.Tree = trace(i"transfromTypeApply ${tree}") {
    // val capturedClassParams = getAllClassParams(ctx.owner.enclosingClass)
    // val (methodTypeParams, offsets) = getAllMethodParams
    // val args = tree.args.map(_.tpe).map(p => toTypeA(p, capturedClassParams, methodTypeParams, offsets))
    // tree.fun.putAttachment(InstructionTypeArguments, args)
    tree
  }

}

object ErasurePreservation {
  val name: String = "erasurePreservation"
  val description: String = "preserve information in annotations before erasure"
  val methodParameterReturnTypeMap = scala.collection.mutable.Map[Symbol, (Int, List[TypeB], TypeB)]()
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
  case Byte
  case Char
  case Double
  case Float
  case Int
  case Long
  case Short
  case Boolean
  case None
  case M(paramNum: Int)
  case K(
    outer: Int,
    paramNum: Int)
  case Array(tp: TypeB)

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
