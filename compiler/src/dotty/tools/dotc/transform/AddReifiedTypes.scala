package dotty.tools
package dotc
package transform

import core.DenotTransformers.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import core.Contexts.*
import core.Types.*
import core.Names.*
import core.Symbols.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Constants.*

class AddReifiedTypes extends MiniPhase with InfoTransformer {
    import ast.tpd.*

    final val DEBUG = false

    val reifiedSyms = scala.collection.mutable.Map[Symbol, Map[Name, Symbol]]()

    def printMap(): Unit = {
        println("Reified Syms Map:")
        for ((k, v) <- reifiedSyms) {
            println(s"  Method: ${k}, Reified Syms: ${v}")
        }
    }
    
    override def phaseName: String = "addReifiedTypes"
    override def description: String = "add reified type values to methods"

    override def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
        if (sym.is(Flags.Method)) then
            val res = addReifiedParams(tp)
            //println(s"Transforming method info for ${sym.name}, from: ${tp.show} to: ${res.show}")
            res
        else tp

    def addReifiedParams(tp: Type)(using Context): Type = tp match {
        case pt: PolyType =>
            val reifiedParamNames = pt.paramNames.map(name => termName(s"reified_$name"))
            val reifiedParamTypes = reifiedParamNames.map(_ => defn.ReifiedValueType)
            pt.resType match {
                case mt: MethodType =>
                    // println(s"Adding reified params to method pt. method type $mt: ${pt.show}")
                    val rest = addReifiedParams(mt.resType)
                    val reifiedList = MethodType(reifiedParamNames, reifiedParamTypes, rest)
                    pt.derivedLambdaType(
                        pt.paramNames,
                        pt.paramInfos,
                        mt.derivedLambdaType(
                            mt.paramNames,
                            mt.paramInfos,
                            reifiedList
                        )
                    )
                case other =>
                    // println(s"Adding reified params to method pt. other $other: ${pt.show}")
                    val rest = addReifiedParams(other)
                    val reifiedList = MethodType(reifiedParamNames, reifiedParamTypes, rest)
                    pt.derivedLambdaType(
                        pt.paramNames,
                        pt.paramInfos,
                        reifiedList
                    )
            }
        case mt: MethodType =>
            // println(s"Adding reified params to method mt. method type $mt: ${tp.show}")
            mt.derivedLambdaType(
                mt.paramNames,
                mt.paramInfos,
                addReifiedParams(mt.resType)
            )
        case _ => tp
    }
    
    override def prepareForDefDef(tree: DefDef)(using Context): Context = 
        val sym = tree.symbol
        // type params from all clauses
        val typeParams = tree.paramss.collect{
            case tparams: List[?] if tparams.nonEmpty && tparams.head.isInstanceOf[TypeDef] =>
                tparams.asInstanceOf[List[TypeDef]]
        }.flatten
        if (typeParams.nonEmpty) {
            if DEBUG then println(s"PrepareForDefDef for tree.show: ${tree.show}" +
              s" tree: ${tree}" +
              s" with type params: ${typeParams.map(_.name)}")
            val typeParamMap = typeParams.map{tparam => 
                val reifiedName = termName(s"reified_${tparam.name}")
                val reifiedSym = newSymbol(sym, reifiedName, Flags.Param, defn.ReifiedValueType)
                (tparam.name: Name) -> reifiedSym
            }.toMap
            reifiedSyms(sym) = typeParamMap
        }
        ctx

    override def transformDefDef(tree: DefDef)(using Context): Tree = {
        val sym = tree.symbol
        // if there are no type parameters, no need to add reified params
        if (!tree.paramss.exists(_.exists(_.isInstanceOf[TypeDef]))) return tree

        var newParamss = List.empty[ParamClause]
        var typeParams = List.empty[TypeDef]
        for (clause <- tree.paramss){
            clause match {
                // def A[U, V](u: U, v: V)
                // case [U, V]
                case tparams: List[?] if tparams.nonEmpty && tparams.head.isInstanceOf[TypeDef] =>
                    newParamss = newParamss :+ clause.asInstanceOf[ParamClause]
                    typeParams = tparams.asInstanceOf[List[TypeDef]]
                case vparams: List[?] =>
                    newParamss = newParamss :+ clause.asInstanceOf[ParamClause]
                    if (typeParams.nonEmpty) {
                        if DEBUG then println(s"TransformDefDef for tree: ${tree.show} Adding reified value at end for method ${sym.name} with type params: ${typeParams.map(_.name)}")
                        newParamss = newParamss :+ createReifiedClause(sym, typeParams)
                        typeParams = Nil
                    }
            }
        }
        if (typeParams.nonEmpty) {
            if DEBUG then println(s"TransformDefDef for tree: ${tree.show} Adding reified value at end for method ${sym.name} with type params: ${typeParams.map(_.name)}")
            newParamss = newParamss :+ createReifiedClause(sym, typeParams)
        }
        cpy.DefDef(tree)(paramss = newParamss)
    }

    def createReifiedClause(sym: Symbol, typeParams: List[TypeDef])(using Context): ParamClause = {
        if (reifiedSyms.contains(sym)){
            if DEBUG then printMap()
            val map = reifiedSyms(sym)
            typeParams.map {
                tparam =>
                    if (map.contains(tparam.name)){
                        ValDef(map(tparam.name).asTerm)
                    } else {
                        report.error(s"createReifiedClause: no reified sym found for type param ${tparam.name} in method ${sym.name}")
                        val reifiedName = termName(s"reified_${tparam.name}")
                        val reifiedSym = newSymbol(sym, reifiedName, Flags.Param, defn.ReifiedValueType)
                        ValDef(reifiedSym.asTerm)
                    }
            }
        } else {
            if DEBUG then println(s"reifiedSyms keys: ${reifiedSyms.keys.map(_.name)}")
            report.error(s"createReifiedClause: no reified syms found for method ${sym.name}")
            Nil
        }
    }

    override def transformApply(tree: Apply)(using Context): Tree = {
        val fun = tree.fun
        val args = tree.args
        val funType = fun.tpe.widen
        val resType = funType match {
            case mt: MethodType => 
                if DEBUG then println(s"TransformApply tree type: ${tree.tpe.show}, method type: ${mt.resType.show}")
                mt.resType
            case _ => tree.tpe
        }
        if (isReifiedParamMethod(resType)){
            val reifiedArgs = collectReifiedArgs(fun)
            val innerApply = cpy.Apply(tree)(fun, args).withType(resType)
            val outerResType = resType match {
                case mt: MethodType => mt.resType
                case _ => resType
            }
            if DEBUG then println(s"Transforming Apply: ${tree.show}, fun type: ${funType.show}, res type: ${resType.show}, with reified args: ${reifiedArgs.map(_.show)}")
            Apply(innerApply, reifiedArgs).withType(outerResType)
        } else tree
    }

    def isReifiedParamMethod(tpe: Type)(using Context): Boolean = tpe match {
        case mt: MethodType =>
            mt.paramNames.nonEmpty && mt.paramNames.head.toString.startsWith("reified_")
        case _ => false
    }

    def collectReifiedArgs(tree: Tree)(using Context): List[Tree] = {
        tree match {
            case ta: TypeApply =>
                ta.args.map(arg => createReifiedValue(arg.tpe))
            case a: Apply =>
                collectReifiedArgs(a.fun)
            case _ => Nil
        }
    }
    
    def createReifiedValue(tpe: Type)(using Context): Tree = {
       tpe.widen match
            case tpr: TypeParamRef => 
                val paramName = tpr.paramName
                var owner = ctx.owner
                var foundSym: Option[Symbol] = None
                while (owner  != NoSymbol && foundSym.isEmpty){
                    if (reifiedSyms.contains(owner)){
                        val map = reifiedSyms(owner)
                        if (map.contains(paramName)){
                            foundSym = Some(map(paramName))
                        }
                    }
                    owner = owner.owner
                }
                foundSym match {
                    case Some(sym) => 
                        ref(sym)
                    case None => 
                        report.error(s"createReifiedValue: no reified sym found for type param ${paramName} in owner ${ctx.owner.name}")
                        Literal(Constant(0.toByte)) // default value to avoid crash
                }
            case tr: TypeRef => {
                if tr.symbol.isTypeParam then
                    val paramName = tr.name
                    val paramOwner = tr.symbol.owner
                    if reifiedSyms.contains(paramOwner) && reifiedSyms(paramOwner).contains(paramName) then
                        val sym = reifiedSyms(paramOwner)(paramName)
                        ref(sym)
                    else {
                        report.error(s"createReifiedValue: no reified sym found for type param ${paramName} in owner ${paramOwner.name}")
                        Literal(Constant(0.toByte)) // default value to avoid crash
                    }
                else
                    val value = 
                        if tr.isRef(defn.ByteClass) then 'B' else
                        if tr.isRef(defn.CharClass) then 'C' else
                        if tr.isRef(defn.DoubleClass) then 'D' else
                        if tr.isRef(defn.FloatClass) then 'F' else
                        if tr.isRef(defn.IntClass) then 'I' else
                        if tr.isRef(defn.LongClass) then 'J' else
                        if tr.isRef(defn.ShortClass) then 'S' else
                        if tr.isRef(defn.BooleanClass) then 'Z' else
                        if tr.isRef(defn.AnyClass) then 'L' else
                            println(s"should not be here! createReifiedValue: unhandled type ref ${tr.show}");
                            report.error(s"createReifiedValue: unhandled type ref ${tr.show}");
                            'L' //TODO
                    Literal(Constant(value.toByte))
            }
    }
}