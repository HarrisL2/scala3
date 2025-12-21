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

object AddReifiedTypes {
    val reifiedFieldNamePrefix = "reifiedField$"

    final val DEBUG = false

    val name: String = "addReifiedTypes"
    val description: String = "add reified type values to methods"
}

class AddReifiedTypes extends MiniPhase with InfoTransformer {
    import ast.tpd.*
    import AddReifiedTypes.DEBUG
    import AddReifiedTypes.reifiedFieldNamePrefix

    val reifiedSyms = scala.collection.mutable.Map[Symbol, Map[Name, Symbol]]()
    // ClassSymbol -> (TypeParameter name -> ReifiedField Symbol)
    val classFieldSyms = scala.collection.mutable.Map[Symbol, Map[Name, Symbol]]()

    def printMethodMap(): Unit = {
        println("Reified Syms Map:")
        for ((k, v) <- reifiedSyms) {
            println(s"  Method: ${k}, Reified Syms: ${v}")
        }
    }

    def printClassFieldMap(): Unit = {
        println("Class Field Syms Map:")
        for ((k, v) <- classFieldSyms) {
            println(s"  Class: ${k}, Field Syms: ${v}")
        }
    }
    
    override def phaseName: String = AddReifiedTypes.name
    override def description: String = AddReifiedTypes.description

    def getCallSymbol(tree: Tree)(using Context): Symbol = tree match {
        case a: Apply => getCallSymbol(a.fun)
        case ta: TypeApply => ta.symbol
        case s: Select => s.symbol
        case i: Ident => i.symbol
        case _ => NoSymbol
    }
    override def prepareForTemplate(tree: Template)(using Context): Context = 
        val cls = ctx.owner.asClass
        if (cls.typeParams.nonEmpty){
            if DEBUG then println(s"AddReifiedTypes: PrepareForTemplate for class ${cls.name} with type params: ${cls.typeParams.map(_.name)}")
            val fields = cls.typeParams.map { tparam => 
                val reifiedName = termName(s"${reifiedFieldNamePrefix}${tparam.name}")
                val reifiedSym = newSymbol(cls, reifiedName, Flags.Private | Flags.ParamAccessor, defn.ReifiedValueType).entered
                (tparam.name: Name) -> reifiedSym
            }.toMap
            classFieldSyms(cls) = fields
            printClassFieldMap()
        }
        ctx

    override def transformTemplate(tree: Template)(using Context): Tree = 
        val cls = ctx.owner.asClass
        if (classFieldSyms.contains(cls)){
            val fieldsMap = classFieldSyms(cls)
            
            val constr = tree.constr
            val allParams = constr.paramss.flatten
            val reifiedParamsCount = cls.typeParams.length
            val reifiedParamDefs = allParams.takeRight(reifiedParamsCount)
            
            val newFields = cls.typeParams.zip(reifiedParamDefs).map {
                case (tparam, paramDef) =>
                    val fieldSym = fieldsMap(tparam.name)
                    ValDef(fieldSym.asTerm)
            }
            
            if DEBUG then println(s"TransformTemplate for class ${cls.name}, adding reified fields: ${newFields.map(_.name)}")

            cpy.Template(tree)(body = tree.body ::: newFields)
        } else tree
    
    override def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
        if (sym.is(Flags.Method)) then
            val res = 
                if (sym.isConstructor && sym.owner.isClass && sym.owner.typeParams.nonEmpty) {
                    val cls = sym.owner.asClass
                    val reifiedNames = cls.typeParams.map(tparam => termName(s"${reifiedFieldNamePrefix}${tparam.name}"))
                    val reifiedTypes = reifiedNames.map(_ => defn.ReifiedValueType)
                    if DEBUG then println(s"AddReifiedTypes: TransformInfo for constructor ${sym.name} of class ${cls.name}, adding reified params: ${reifiedNames}")
                    addParamsToMethod(tp, reifiedNames, reifiedTypes)
                } else addReifiedParams(tp)
            //println(s"Transforming method info for ${sym.name}, from: ${tp.show} to: ${res.show}")
            res
        else tp

    def addParamsToMethod(tp: Type, names: List[TermName], types: List[Type])(using Context): Type = {
        tp match {
            case pt: PolyType =>
                pt.derivedLambdaType(
                    pt.paramNames,
                    pt.paramInfos,
                    addParamsToMethod(pt.resType, names, types)
                )
            case mt: MethodType =>
                mt.derivedLambdaType(
                    mt.paramNames,
                    mt.paramInfos,
                    addParamsToMethod(mt.resType, names, types)
            )
            case res =>
                MethodType(names, types, res)
        }
    }

    def addReifiedParams(tp: Type)(using Context): Type = tp match {
        case pt: PolyType =>
            val reifiedParamNames = pt.paramNames.map(name => termName(s"reified$$$name"))
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
            if DEBUG then println(s"AddReifiedTypes: PrepareForDefDef for tree.show: ${tree.show}" +
              s" tree: ${tree}" +
              s" with type params: ${typeParams.map(_.name)}")
            val typeParamMap = typeParams.map{tparam => 
                val reifiedName = termName(s"reified$$${tparam.name}")
                val reifiedSym = newSymbol(sym, reifiedName, Flags.Param, defn.ReifiedValueType)
                (tparam.name: Name) -> reifiedSym
            }.toMap
            reifiedSyms(sym) = typeParamMap
        }
        ctx

    override def transformDefDef(tree: DefDef)(using Context): Tree = {
        val sym = tree.symbol


        //fpr constructors
        if (sym.isConstructor && sym.owner.isClass && classFieldSyms.contains(sym.owner)){
            val cls = sym.owner.asClass
            val newParamSyms = cls.typeParams.map(tparam => 
                newSymbol(sym, termName(s"${reifiedFieldNamePrefix}${tparam.name}"), Flags.Param, defn.ReifiedValueType))
            val newParamDefs = newParamSyms.map(sym => ValDef(sym.asTerm))
            
            val newParamss = tree.paramss :+ newParamDefs
            if DEBUG then println(s"AddReifiedTypes: TransformDefDef for constructor ${sym.name} of class ${cls.name}, adding reified params: ${newParamDefs.map(_.name)}")
            
            return cpy.DefDef(tree)(paramss = newParamss.asInstanceOf[List[ParamClause]])
        }

        //for notmal generic methods
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
                        if DEBUG then println(s"AddReifiedTypes: TransformDefDef for tree: ${tree.show} Adding reified value at end for method ${sym.name} with type params: ${typeParams.map(_.name)}")
                        newParamss = newParamss :+ createReifiedClause(sym, typeParams)
                        typeParams = Nil
                    }
            }
        }
        if (typeParams.nonEmpty) {
            if DEBUG then println(s"AddReifiedTypes: TransformDefDef for tree: ${tree.show} Adding reified value at end for method ${sym.name} with type params: ${typeParams.map(_.name)}")
            newParamss = newParamss :+ createReifiedClause(sym, typeParams)
        }
        cpy.DefDef(tree)(paramss = newParamss)
    }

    def createReifiedClause(sym: Symbol, typeParams: List[TypeDef])(using Context): ParamClause = {
        if (reifiedSyms.contains(sym)){
            if DEBUG then printMethodMap()
            val map = reifiedSyms(sym)
            typeParams.map {
                tparam =>
                    if (map.contains(tparam.name)){
                        ValDef(map(tparam.name).asTerm)
                    } else {
                        report.error(s"AddReifiedTypes: ERROR: createReifiedClause: no reified sym found for type param ${tparam.name} in method ${sym.name}")
                        val reifiedName = termName(s"reified$$${tparam.name}")
                        val reifiedSym = newSymbol(sym, reifiedName, Flags.Param, defn.ReifiedValueType)
                        ValDef(reifiedSym.asTerm)
                    }
            }
        } else {
            if DEBUG then println(s"AddReifiedTypes: reifiedSyms keys: ${reifiedSyms.keys.map(_.name)}")
            report.error(s"AddReifiedTypes: ERROR: createReifiedClause: no reified syms found for method ${sym.name}")
            Nil
        }
    }

    override def transformApply(tree: Apply)(using Context): Tree = {
        val fun = tree.fun
        val args = tree.args
        val sym = getCallSymbol(fun)

        if (sym.isConstructor) {
            println(s"AddReifiedTypes: TransformApply Constructor call: tree: $tree, ${tree.show}")
            val isReified = tree.tpe.widen match {
                case mt: MethodType => 
                    // println(s"TransformApply Constructor call: method type: ${mt.paramNames}")
                    mt.paramNames.nonEmpty && mt.paramNames.head.toString.startsWith(reifiedFieldNamePrefix)
                case _ => false
            }
            if (isReified) {
                if DEBUG then println(s"AddReifiedTypes: TransformApply Constructor: need to add reified args to constructor call: ${tree.show}")
                var current = fun
                while (current.isInstanceOf[Apply]) current = current.asInstanceOf[Apply].fun
                
                val reifiedArgs = collectReifiedArgs(current)
                if (reifiedArgs.nonEmpty) {
                    if DEBUG then println(s"AddReifiedTypes: TransformApply Constructor: appending args ${reifiedArgs.map(_.show)}")
                    val reifiedParamNames = reifiedArgs.map(_ => termName("reified"))
                    val reifiedParamTypes = reifiedArgs.map(_ => defn.ReifiedValueType)

                    val reifiedMethodType = MethodType(reifiedParamNames, reifiedParamTypes, tree.tpe)
                    val inner = tree.withType(reifiedMethodType)
                    Apply(inner, reifiedArgs).withType(tree.tpe)
                } else tree
            } else tree
        } else {
            val funType = fun.tpe.widen
            val resType = funType match {
                case mt: MethodType => 
                    if DEBUG then println(s"AddReifiedTypes: TransformApply tree type: ${tree.tpe.show}, method type: ${mt.resType.show}")
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
                if DEBUG then println(s"AddReifiedTypes: Transforming Apply: ${tree.show}, fun type: ${funType.show}, res type: ${resType.show}, with reified args: ${reifiedArgs.map(_.show)}")
                Apply(innerApply, reifiedArgs).withType(outerResType)
            } else tree
        }
    }

    def isReifiedParamMethod(tpe: Type)(using Context): Boolean = tpe match {
        case mt: MethodType =>
            mt.paramNames.nonEmpty && mt.paramNames.head.toString.startsWith("reified$")
        case _ => false
    }

    def collectReifiedArgs(tree: Tree)(using Context): List[Tree] = {
        tree match {
            case ta: TypeApply =>
                ta.args.map(arg => createReifiedValue(arg.tpe))
            case s: Select if s.symbol.isConstructor =>
                s.qualifier match {
                    case n: New =>
                        n.tpe match {
                            case at: AppliedType => at.args.map(createReifiedValue)
                            case _ => Nil
                        }
                    case _ => Nil
                }
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
                        println(s"AddReifiedTypes: should not be here! ERROR: createReifiedValue: no reified sym found for type param ${paramName} in owner ${ctx.owner.name}");
                        report.error(s"AddReifiedTypes: ERROR: createReifiedValue: no reified sym found for type param ${paramName} in owner ${ctx.owner.name}")
                        Literal(Constant(0.toByte)).withType(defn.ReifiedValueType)
                }
            case tr: TypeRef => {
                if tr.symbol.isTypeParam then
                    val paramName = tr.name
                    val paramOwner = tr.symbol.owner
                    // class type param:
                    if (paramOwner.isClass){
                        if (classFieldSyms.contains(paramOwner)) {
                            val fieldSym = classFieldSyms(paramOwner)(paramName)
                            This(paramOwner.asClass).select(fieldSym)
                        } else {
                            println(s"AddReifiedTypes: should not be here! ERROR: createReifiedValue: no reified field found for class type param ${paramName} in class ${paramOwner.name}");
                            report.error(s"AddReifiedTypes: ERROR: createReifiedValue: no reified field found for class type param ${paramName} in class ${paramOwner.name}")
                            Literal(Constant(0.toByte)).withType(defn.ReifiedValueType)
                        }
                    }
                    // method type param:
                    else
                    if reifiedSyms.contains(paramOwner) && reifiedSyms(paramOwner).contains(paramName) then
                        val sym = reifiedSyms(paramOwner)(paramName)
                        ref(sym)
                    else {
                        println(s"AddReifiedTypes: should not be here! ERROR: createReifiedValue: no reified sym found for type param ${paramName} in owner ${paramOwner.name}");
                        report.error(s"AddReifiedTypes: ERROR: createReifiedValue: no reified sym found for type param ${paramName} in owner ${paramOwner.name}")
                        Literal(Constant(0.toByte)).withType(defn.ReifiedValueType)
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
                            // println(s"should not be here! createReifiedValue: unhandled type ref ${tr.show}");
                            // report.error(s"ERROR: createReifiedValue: unhandled type ref ${tr.show}");
                            'L' //TODO
                    Literal(Constant(value.toByte)).withType(defn.ReifiedValueType)
            }
    }
}