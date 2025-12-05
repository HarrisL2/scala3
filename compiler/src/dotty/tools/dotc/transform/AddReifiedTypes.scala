package dotty.tools
package dotc
package transform

import core.DenotTransformers.*
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import core.Contexts.Context
import core.Types.*
import core.Names.*
import core.Symbols.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Constants.*

class AddReifiedTypes extends MiniPhase with InfoTransformer {
    import ast.tpd.*
    
    override def phaseName: String = "addReifiedTypes"
    override def description: String = "add reified type values to methods"

    override def transformInfo(tp: Type, sym: Symbol)(using Context): Type = tp match {
        case pt: PolyType =>
            pt.derivedLambdaType(
                pt.paramNames,
                pt.paramInfos,
                addReifiedParams(pt.resType, pt.paramNames)
            )
        case _ => tp
    }

    def addReifiedParams(tp: Type, typeParamNames: List[Name])(using Context): Type = tp match {
        case mt: MethodType =>
            val newParamNames = typeParamNames.map(name => termName(s"reified_$name"))
            val newParamTypes = newParamNames.map(_ => defn.ReifiedValueType)
            mt.derivedLambdaType(
                mt.paramNames ++ newParamNames,
                mt.paramInfos ++ newParamTypes,
                mt.resultType
            )
        case pt: PolyType =>
            pt.derivedLambdaType(
                pt.paramNames,
                pt.paramInfos,
                addReifiedParams(pt.resType, typeParamNames)
            )
        case _ => tp
    }
    
    override def transformDefDef(tree: DefDef)(using Context): Tree = {
        val sym = tree.symbol
        val typeParams = tree.paramss.collectFirst {
            case tparams: List[?] if tparams.nonEmpty && tparams.head.isInstanceOf[TypeDef] =>
                tparams.asInstanceOf[List[TypeDef]]
        }.getOrElse(Nil)

        if (typeParams.nonEmpty) {
            val newParamDefs = typeParams.map { tparam =>
                val paramName = termName(s"reified_${tparam.name}")
                val paramSym = newSymbol(
                    sym,
                    paramName,
                    Flags.Param,
                    defn.ReifiedValueType,
                ).asTerm
                ValDef(paramSym)
            }
            
            var added = false
            val newParamss = tree.paramss.map { clause =>
                val isTypeClause = clause.nonEmpty && clause.head.isInstanceOf[TypeDef]
                if (!isTypeClause && !added) {
                    added = true
                    val newClause: ParamClause = clause.asInstanceOf[List[ValDef]] ++ newParamDefs
                    newClause
                } else {
                    clause
                }
            }
            
            if (added) cpy.DefDef(tree)(paramss = newParamss)
            else tree
        } else tree
    }

    override def transformApply(tree: Apply)(using Context): Tree = {
        val sym = tree.fun.symbol
        if (sym.exists && sym.info.isInstanceOf[PolyType]) {
            val poly = sym.info.asInstanceOf[PolyType]
            val methodType = poly.resultType match {
                case mt: MethodType => mt
                case _ => null
            }
            
            if (methodType != null) {
                val expectedParams = methodType.paramInfos.length
                val currentArgs = tree.args.length
                
                if (currentArgs < expectedParams) {
                    val reifiedArgs = tree.fun match {
                        case TypeApply(_, targs) =>
                            targs.map(targ => createReifiedValue(targ.tpe))
                        case _ =>
                            poly.paramNames.map(_ => createReifiedValue(defn.ReifiedValueType))
                    }
                    return cpy.Apply(tree)(tree.fun, tree.args ++ reifiedArgs)
                }
            }
        }
        tree
    }
    
    def createReifiedValue(tpe: Type)(using Context): Tree = {
        val value = tpe.widen match
            case tpr: TypeParamRef => 'L' //TODO
            case tr: TypeRef =>
                if tr.isRef(defn.ByteClass) then 'B' else
                if tr.isRef(defn.CharClass) then 'C' else
                if tr.isRef(defn.DoubleClass) then 'D' else
                if tr.isRef(defn.FloatClass) then 'F' else
                if tr.isRef(defn.IntClass) then 'I' else
                if tr.isRef(defn.LongClass) then 'J' else
                if tr.isRef(defn.ShortClass) then 'S' else
                if tr.isRef(defn.BooleanClass) then 'Z' else
                if tr.isRef(defn.AnyClass) then 'L' else
                if tr.symbol.isTypeParam then
                    'L' //TODO
                else
                    'L' //TODO
        Literal(Constant(value.toByte))
    }
}