package dotty.tools.dotc.transform

import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.transform.ErasurePreservation.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Phases.*

class MoveReifiedParams extends MiniPhase with InfoTransformer{
    import tpd.*
    override def phaseName: String = "moveReifiedParams"
    override def description: String = "move reified parameters to the end of the parameter list"

    final val DEBUG = false

    override def transformInfo(tp: Type, sym: Symbol)(using Context): Type = tp match {
        case mt: MethodType if sym.is(Flags.Method)=>
            val zipped = mt.paramNames.zip(mt.paramInfos)
            val (reified, normal) = zipped.partition((name, _) => name.toString.startsWith("reified$"))
            val newNames = normal.map(_._1) ++ reified.map(_._1)
            val newInfos = normal.map(_._2) ++ reified.map(_._2)

            if (reified.nonEmpty){
                if DEBUG then println(s"MoveReifiedParams: transformInfo : Reordering method parameters for ${sym} from ${mt.paramNames} to ${newNames}")
                mt.derivedLambdaType(newNames, newInfos, mt.resType)
            } else tp
        case _ => tp
    }

    override def transformDefDef(tree: DefDef)(using Context): Tree = {
        val sym = tree.symbol
        val allParams = tree.paramss.flatten

        val (reifiedParams, normalParams) = allParams.partition(_.name.toString.startsWith("reified$"))

        if (reifiedParams.nonEmpty) {
            val newParams = normalParams ++ reifiedParams
            
            val newTree = cpy.DefDef(tree)(paramss = List(newParams.asInstanceOf[ParamClause]))
            
            // reorder MethodParameterReturnType attachment
            if (tree.hasAttachment(MethodParameterReturnType)) {
                val (count, paramTypes, retType) = tree.attachment(MethodParameterReturnType)
                val zipped = allParams.zip(paramTypes)
                val (reifiedT, normalT) = zipped.partition(_._1.name.toString.startsWith("reified$"))
                val newParamTypes = normalT.map(_._2) ++ reifiedT.map(_._2)
                
                if DEBUG then println(s"MoveReifiedParams: transformDefDef: old attachment: ${tree.attachment(MethodParameterReturnType)}, new attachment: ${(count, newParamTypes, retType)}")
                newTree.putAttachment(MethodParameterReturnType, (count, newParamTypes, retType))
            }
            newTree
        } else tree
    }

    override def transformApply(tree: Apply)(using Context): Tree = 
        val sym = tree.fun.symbol
        if (sym.is(Flags.Method)) {
            // Collect ALL parameter names from the potentially curried/generic type
            val allParamNames = atPhase(erasurePhase) {
                collectParamNames(sym.info)
            }

            val args = tree.args
            // Now we can compare the full flattened argument list with the full parameter name list
            if (args.length == allParamNames.length) {
                val zipped = args.zip(allParamNames)
                val (reifiedArgs, normalArgs) = zipped.partition(_._2.toString.startsWith("reified$"))
                
                if (reifiedArgs.nonEmpty) {
                    val newArgs = normalArgs.map(_._1) ++ reifiedArgs.map(_._1)
                    if DEBUG then println(s"MoveReifiedParams: transformApply: Reordering method arguments for ${sym} from\n" +
                      s"     ${args.map(_.show)}\n" +
                      s"     to ${newArgs.map(_.show)}")
                    cpy.Apply(tree)(tree.fun, newArgs)
                } else tree
            } else tree
        } else tree

    def collectParamNames(tp: Type)(using Context): List[Name] = tp match {
        case pt: PolyType => collectParamNames(pt.resType)
        case mt: MethodType => mt.paramNames ++ collectParamNames(mt.resType)
        case _ => Nil
    }
}