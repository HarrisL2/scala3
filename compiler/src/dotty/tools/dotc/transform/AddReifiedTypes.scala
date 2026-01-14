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
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Scopes.*

object AddReifiedTypes {
    val reifiedFieldNamePrefix = "reifiedField$"
    val reifiedLocalNamePrefix = "reifiedLocal$"

    final val DEBUG = false

    val name: String = "addReifiedTypes"
    val description: String = "add reified type values to methods"
}

class AddReifiedTypes extends MiniPhase with InfoTransformer {
    import ast.tpd.*
    import AddReifiedTypes.DEBUG
    import AddReifiedTypes.reifiedFieldNamePrefix
    import AddReifiedTypes.reifiedLocalNamePrefix

    // MethodSymbol -> (TypeParameter name -> ReifiedParam Symbol)
    // map to the symbol of the added reified parameter
    val reifiedSyms = scala.collection.mutable.Map[Symbol, Map[Name, Symbol]]()
    // ClassSymbol -> (TypeParameter name -> ReifiedField Symbol)
    // map to the symbol of the added reified field
    val classFieldSyms = scala.collection.mutable.Map[Symbol, Map[Name, Symbol]]()
    // ClassSymbol -> List of all captured type parameters from outer classes + own parameters
    val classCapturedTypeParams = scala.collection.mutable.Map[Symbol, List[Symbol]]()
    // map old method symbol to new method symbols
    val symbolMap = scala.collection.mutable.Map[Symbol, Symbol]()

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

    def printSymbolMap(): Unit = {
        println("Symbol Map:")
        for ((k, v) <- symbolMap) {
            println(s"  Old Symbol: ${k}, New Symbol: ${v}")
        }
    }
    
    override def phaseName: String = AddReifiedTypes.name
    override def description: String = AddReifiedTypes.description

    override def changesMembers: Boolean = true

    def getCallSymbol(tree: Tree)(using Context): Symbol = tree match {
        case a: Apply => getCallSymbol(a.fun)
        case ta: TypeApply => ta.symbol
        case s: Select => s.symbol
        case i: Ident => i.symbol
        case _ => NoSymbol
    }

    // get all the type parameters from the class and its outer classes
    def getAllTypeParams(cls: Symbol)(using Context): List[Symbol] = {
        val own = cls.typeParams
        val outers = cls.ownersIterator.drop(1).takeWhile(
            cl => cl.isClass && !cl.is(Flags.Module) && !cl.is(Flags.Package)
        ).flatMap(_.typeParams).toList
        own ++ outers
    }

    override def prepareForTemplate(tree: Template)(using Context): Context = 
        val cls = ctx.owner.asClass
        val allTypeParams = getAllTypeParams(cls)
        println(s"AddReifiedTypes: prepareForTemplate Class ${cls.name} all type params (including captured): ${allTypeParams.map(_.name)}")
        if (allTypeParams.nonEmpty){
            // store the captured type params for transformTemplate
            classCapturedTypeParams(cls) = allTypeParams
            println(s"AddReifiedTypes: PrepareForTemplate for class ${cls.name}, " +
              s"captured type params: ${allTypeParams.map(_.name)} to classCapturedTypeParams map")
            val fields = allTypeParams.map { tparam => 
                val reifiedFieldName = termName(s"${reifiedFieldNamePrefix}${cls.fullName.toString.replace('.','$')}$$${tparam.name}")
                // ParamAccessor flag to generate assign in Constructors phase
                val reifiedSym = newSymbol(cls, reifiedFieldName, Flags.ParamAccessor, defn.ReifiedValueType).entered
                (tparam.name: Name) -> reifiedSym
            }.toMap
            // store the field symbol map for transformTemplate
            classFieldSyms(cls) = fields
            println(s"AddReifiedTypes: PrepareForTemplate for class ${cls.name}, " +
              s"adding reified fields: ${fields.values.map(_.name)} to classFieldSyms map")
        }
        ctx

    override def transformTemplate(tree: Template)(using Context): Tree = 
        val cls = ctx.owner.asClass
        if (classFieldSyms.contains(cls)){
            val fieldsMap = classFieldSyms(cls)
            // all parameters of the constructor, including non-type parameter params
            val allParams =  tree.constr.paramss.flatten
            // all type parameters, including captured type params from outer classes
            val allTypeParams = classCapturedTypeParams(cls)
            val reifiedParamsCount = allTypeParams.length
            val reifiedParamDefs = allParams.takeRight(reifiedParamsCount)
            println(s"AddReifiedTypes: TransformTemplate for class ${cls.name}, " +
              s"all type params: ${allTypeParams.map(_.name)}, " +
              s"all params: ${allParams.map(_.name)}")
            // create the new field storages
            val newFields = allTypeParams.zip(reifiedParamDefs).map {
                case (tparam, paramDef) =>
                    val fieldSym = fieldsMap(tparam.name)
                    ValDef(fieldSym.asTerm, EmptyTree)
            }
            
            if DEBUG then println(s"TransformTemplate for class ${cls.name}, adding reified fields: ${newFields.map(_.name)}")

            cpy.Template(tree)(body = tree.body ::: newFields)
        } else tree
    
    def ignoreClassForReification(cls: Symbol)(using Context): Boolean = 
        cls.is(Flags.JavaDefined) || cls.is(Flags.Scala2x)

    override def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
        if (sym.isClass) then
            transformClassInfo(tp, sym)
        // if is constructor & has type parameters
        else if (sym.isConstructor && sym.owner.isClass && getAllTypeParams(sym.owner).nonEmpty) then
            val cls = sym.owner.asClass
            if ignoreClassForReification(cls) then tp
            else
                val allTypeParams = getAllTypeParams(cls)
                val reifiedNames = allTypeParams.map(tparam => termName(s"${reifiedFieldNamePrefix}${cls.fullName.toString.replace('.','$')}$$${tparam.name}"))
                val reifiedTypes = reifiedNames.map(_ => defn.ReifiedValueType)
                if DEBUG then println(s"AddReifiedTypes: TransformInfo for constructor ${sym.name} of class ${cls.name}, adding reified params: ${reifiedNames}")
                addParamsToConstructor(tp, reifiedNames, reifiedTypes)
        else tp
    
    def transformClassInfo(tp: Type, cls: Symbol)(using Context): Type = 
        if ignoreClassForReification(cls) then tp
        else
            tp match {
                case ci: ClassInfo =>
                    var newDecls = ci.decls
                    var changed = false

                    def process(s: Symbol): Unit = {
                        if (needReifiedParams(s)) {
                            val newInfo = addParamsToMethod(s.info, s)
                            if (!changed) then
                                newDecls = newDecls.cloneScope
                                changed = true
                            val newSym = newSymbol(cls, s.name, s.flags, newInfo, s.privateWithin, s.coord)
                            val scope = newDecls.asInstanceOf[MutableScope]
                            scope.unlink(s)
                            scope.enter(newSym)
                            // store the pair (s -> newSym) to symbolMap for transformDefDef
                            symbolMap(s) = newSym
                            if DEBUG then println(s"AddReifiedTypes: Replaced symbol ${s.name} with new symbol ${newSym}")
                        }
                    }
                    // process for al decls
                    ci.decls.iterator.foreach(process)
                    if changed then 
                        ci.derivedClassInfo(decls = newDecls)
                    else ci
                case _ => tp         
        }

    def needReifiedParams(sym: Symbol)(using Context): Boolean = 
        sym.is(Flags.Method) && sym.name != nme.asInstanceOf_ && !sym.isConstructor && 
        (sym.info match {
            case pt: PolyType => true
            case _ => false
        })

    def addParamsToConstructor(tp: Type, names: List[TermName], types: List[Type])(using Context): Type = {
        tp match {
            case pt: PolyType =>
                pt.derivedLambdaType(
                    pt.paramNames,
                    pt.paramInfos,
                    addParamsToConstructor(pt.resType, names, types)
                )
            case mt: MethodType =>
                mt.derivedLambdaType(
                    mt.paramNames,
                    mt.paramInfos,
                    addParamsToConstructor(mt.resType, names, types)
            )
            case res =>
                MethodType(names, types, res)
        }
    }

    def addParamsToMethod(tp: Type, methodSym: Symbol)(using Context): Type = tp match {
        case pt: PolyType =>
            val reifiedParamNames = pt.paramNames.map(name => 
                termName(s"reified$$${methodSym.fullName.toString.replace('.','$')}$$$name"))
            val reifiedParamTypes = reifiedParamNames.map(_ => defn.ReifiedValueType)
            pt.resType match {
                case mt: MethodType =>
                    // println(s"Adding reified params to method pt. method type $mt: ${pt.show}")
                    val rest = addParamsToMethod(mt.resType, methodSym)
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
                    val rest = addParamsToMethod(other, methodSym)
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
                addParamsToMethod(mt.resType, methodSym)
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
        // println(s"AddReifiedTypes: prepareForDefDef Method ${sym.name} type params: ${typeParams.map(_.name)}")
        if (typeParams.nonEmpty) {
            if DEBUG then println(s"AddReifiedTypes: PrepareForDefDef for tree.show: ${tree.show}" +
              s" tree: ${tree}" +
              s" with type params: ${typeParams.map(_.name)}")
            val typeParamMap = typeParams.map{tparam => 
                val reifiedName = termName(s"reified$$${sym.fullName.toString.replace('.','$')}$$${tparam.name}")
                val reifiedSym = newSymbol(sym, reifiedName, Flags.Param, defn.ReifiedValueType)
                (tparam.name: Name) -> reifiedSym
            }.toMap
            // add the (type param -> reified param symbol) map to reifiedSyms for transformDefDef
            reifiedSyms(sym) = typeParamMap
        }
        ctx

    override def transformDefDef(tree: DefDef)(using Context): Tree = {
        val sym = tree.symbol

        // if (sym.name.toString == "A") {
        //      println(s"DEBUG: transformDefDef A (id=${sym.id}). info(current)=${sym.info.show}")
        //      try {
        //         println(s"DEBUG: transformDefDef A. info(next)=${sym.info(using ctx.withPhase(ctx.phase.next)).show}")
        //      } catch {
        //          case e: Throwable => println(s"DEBUG: Failed to print info(next): $e")
        //      }
        // }

        if (symbolMap.contains(sym)) {
            val newMethodSym = symbolMap(sym)
            val newParamssBuffer = scala.collection.mutable.ListBuffer[ParamClause]()
            var paramMap = Map[Symbol, Symbol]()
            val reifiedParamMap = scala.collection.mutable.Map[Name, Symbol]()
            // map the old symbol to the type of the new symbol
            var typeParamMap = Map[Symbol, Type]()
            // take care of multiple parameter list
            var pendingReified: List[ValDef] = Nil

            for (clause <- tree.paramss){
                clause match {
                    case tparams: List[?] if tparams.nonEmpty && tparams.head.isInstanceOf[TypeDef] =>
                        if (pendingReified.nonEmpty) {
                            newParamssBuffer += pendingReified
                            pendingReified = Nil
                        }
                        val newDefs = tparams.asInstanceOf[List[TypeDef]].map { tdef =>
                            val oldSym = tdef.symbol
                            val newSym = newSymbol(newMethodSym, tdef.name, oldSym.flags, oldSym.info, oldSym.privateWithin, oldSym.coord)
                            paramMap += (oldSym -> newSym)
                            typeParamMap += (oldSym -> newSym.typeRef)
                            TypeDef(newSym.asType)
                        }
                        newParamssBuffer += newDefs
                        val tdefs = clause.asInstanceOf[List[TypeDef]]
                        val extraParamDefs = tdefs.map { tparam =>
                            val reifiedName = termName(s"reified$$${newMethodSym.fullName.toString.replace('.','$')}$$${tparam.name}")
                            val reifiedSym = newSymbol(newMethodSym, reifiedName, Flags.Param, defn.ReifiedValueType)
                            ValDef(reifiedSym.asTerm)
                        }

                        tdefs.zip(extraParamDefs).foreach { case (tdef, vdef) =>
                            reifiedParamMap += (tdef.name -> vdef.symbol)
                        }
                        pendingReified = extraParamDefs

                    case vparams: List[?] =>
                        val newDefs = vparams.asInstanceOf[List[ValDef]].map { vdef =>
                            val oldSym = vdef.symbol
                            val newInfo = oldSym.info.subst(typeParamMap.keys.toList, typeParamMap.values.toList)
                            val newSym = newSymbol(newMethodSym, vdef.name, oldSym.flags, newInfo, oldSym.privateWithin, oldSym.coord)
                            paramMap += (oldSym -> newSym)
                            ValDef(newSym.asTerm)
                        }
                        newParamssBuffer += newDefs

                        if (pendingReified.nonEmpty) {
                            newParamssBuffer += pendingReified
                            pendingReified = Nil
                        }

                }
            }
            if (pendingReified.nonEmpty) {
                newParamssBuffer += pendingReified
            }
            reifiedSyms(newMethodSym) = reifiedParamMap.toMap

            val newParamss = newParamssBuffer.toList
            newMethodSym.setParamssFromDefs(newParamss)

            val newBody = tree.rhs.subst(paramMap.keys.toList, paramMap.values.toList)
            return DefDef(newMethodSym.asTerm, newBody)
        }
        

        //for constructors
        if (sym.isConstructor && sym.owner.isClass && classFieldSyms.contains(sym.owner)) then
            val cls = sym.owner.asClass
            val allTypeParams = getAllTypeParams(cls)
            val newParamSyms = allTypeParams.map(tparam => 
                newSymbol(sym, termName(s"${reifiedFieldNamePrefix}${cls.fullName.toString.replace('.','$')}$$${tparam.name}"), Flags.Param, defn.ReifiedValueType))
            val newParamDefs = newParamSyms.map(sym => ValDef(sym.asTerm))
            
            val newParamss = tree.paramss :+ newParamDefs

            sym.setParamssFromDefs(newParamss.asInstanceOf[List[ParamClause]])

            if DEBUG then println(s"AddReifiedTypes: TransformDefDef for constructor ${sym.name} of class ${cls.name}, adding reified params: ${newParamDefs.map(_.name)}")
            // no need to add assign here, Constructors phase generate them
            cpy.DefDef(tree)(paramss = newParamss.asInstanceOf[List[ParamClause]])
        else 
            tree
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
                        val reifiedName = termName(s"reified$$${sym.fullName.toString.replace('.','$')}$$${tparam.name}")
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

    def extractOuter(tree: Tree)(using Context): Tree = {
        // println(s"extract outer: tree: ${tree} tree.show " + tree.show)
        tree match {
            case Apply(fn, _) => extractOuter(fn)
            case TypeApply(fn, _) => extractOuter(fn)
            case Select(qual, _) => 
                if (qual.isInstanceOf[New]) extractOuter(qual)
                else qual
            case New(tpt) => extractOuter(tpt)
            case AppliedTypeTree(tpt, _) => extractOuter(tpt)
            case t: TypeTree => 
                def extractOuterFromType(tpe: Type)(using Context): Tree = {
                    tpe match {
                        case TypeRef(prefix, _) =>
                            prefix match {
                                case tr: TermRef => ref(tr)
                                case th: ThisType => This(th.cls)
                                case _ => EmptyTree
                            }
                        case AppliedType(tycon, _) => extractOuterFromType(tycon)
                        case _ => EmptyTree
                    }
                }
                extractOuterFromType(t.tpe)
            case _ => EmptyTree
        }
    }

    override def transformApply(tree: Apply)(using Context): Tree = {
        val sym = getCallSymbol(tree)

        if ((symbolMap.contains(sym) || reifiedSyms.contains(sym)) && !sym.isConstructor) {
            println(s"AddReifiedTypes: TransformApply for call to method ${sym.name} in tree: ${tree.show}")
            val newSym = symbolMap.getOrElse(sym, sym)

            val newFun = if (symbolMap.contains(sym)) {
                def patchFun(t: Tree): Tree = t match {
                    case s: Select => s.qualifier.select(newSym)
                    case i: Ident => ref(newSym)
                    case ta: TypeApply => TypeApply(patchFun(ta.fun), ta.args)
                    case a: Apply => Apply(patchFun(a.fun), a.args)
                    case _ => t
                }
                patchFun(tree.fun)
            } else tree.fun

            val newApply = Apply(newFun, tree.args)

            if (tree.fun.isInstanceOf[TypeApply]) then
                val targs = tree.fun.asInstanceOf[TypeApply].args
                val reifiedArgs = targs.map(arg => createReifiedValue(arg.tpe))
                if (reifiedArgs.nonEmpty)
                    println(s"AddReifiedTypes: TransformApply: appending reified args ${reifiedArgs.map(_.show)} to call " +
                      s"to method ${sym.name} with fun: ${tree.fun.show}")
                    return Apply(newApply, reifiedArgs)
            else 
                println(s"AddReifiedTypes: TransformApply: no type args found in call to method ${sym.name}")
                return newApply
        }
        if (sym.isConstructor) {
            val fun = tree.fun
            // println(s"AddReifiedTypes: TransformApply Constructor call: tree: $tree, ${tree.show}")
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
                
                val cls = sym.owner.asClass
                val allTypeParams = getAllTypeParams(cls)
                
                // args for type params of this class
                val ownReifiedArgs = collectReifiedArgs(current)
                // type params of outer (not own) 
                val capturedTypeParams = allTypeParams.drop(cls.typeParams.length)
                // optimization: only computer outer once, use it to extract all captured reified args & pass to constructor
                var outerValDef: Tree = EmptyTree
                var outerSym: Symbol = NoSymbol
                // args for captured
                val capturedReifiedArgs = 
                    if (capturedTypeParams.nonEmpty)
                        val outer = extractOuter(tree)
                        println(s"AddReifiedTypes: TransformApply Constructor: class ${cls.name}, outerclass: ${cls.owner.name}, " +
                          s"captured type params: ${capturedTypeParams.map(_.name)} got outer: ${outer.show}")
                        if (outer != EmptyTree) then
                            val outerName = termName(s"${reifiedFieldNamePrefix}outer")
                            outerSym = newSymbol(ctx.owner, outerName, Flags.Synthetic, outer.tpe.widen)
                            outerValDef = ValDef(outerSym.asTerm, outer)
                            val outerReifiedArgs = 
                                capturedTypeParams.map{ tparam =>
                                    val outerClass = cls.owner.asClass
                                    if (classFieldSyms.contains(outerClass) && classFieldSyms(outerClass).contains(tparam.name))
                                        val fieldSym = classFieldSyms(outerClass)(tparam.name)
                                        ref(outerSym).select(fieldSym)
                                    else
                                        // outer has no field for this type param
                                        report.error(s"AddReifiedTypes: ERROR: TransformApply Constructor: no reified field found for captured type param ${tparam.name} in outer class ${outerClass.name} from outer arg: ${outer.show}");
                                        Literal(Constant(0.toByte)).withType(defn.ReifiedValueType)
                                }
                            println("outer reified args: " + outerReifiedArgs.map(_.show))
                            outerReifiedArgs
                        else
                            report.error(s"AddReifiedTypes: ERROR: TransformApply Constructor: could not extract outer for constructor call: ${tree.show}");
                            Nil
                    else Nil

                val reifiedArgs = ownReifiedArgs ++ capturedReifiedArgs

                if (reifiedArgs.nonEmpty) {
                    if DEBUG then println(s"AddReifiedTypes: TransformApply Constructor: appending args ${reifiedArgs.map(_.show)}")
                    val cls = sym.owner.asClass
                    val tparams = allTypeParams

                    // locals to hold reified values
                    val (valDefs, refs) = tparams.zip(reifiedArgs).map {
                        case (tparam, arg) =>
                            val localName = termName(s"${reifiedLocalNamePrefix}${cls.fullName.toString.replace('.','$')}$$${tparam.name}")
                            val valSym = newSymbol(ctx.owner, localName, Flags.Synthetic, defn.ReifiedValueType)
                            val valDef = ValDef(valSym.asTerm, arg)
                            (valDef, ref(valSym))
                    }.unzip

                    val resType = tree.tpe match {
                        case mt: MethodType => mt.resType
                        case _ => tree.tpe
                    }
                    
                    val apply = Apply(tree, refs).withType(resType) //TODO: can avoid evluating outers again

                    val allValDefs = 
                        if (outerValDef != EmptyTree) outerValDef :: valDefs
                        else valDefs
                    Block(allValDefs, apply)
                } else tree
            } else tree
        } else {
            val fun = tree.fun
            val args = tree.args
            val funType = fun.tpe.widen
            val resType = funType match {
                case mt: MethodType => 
                    if DEBUG then println(s"AddReifiedTypes: TransformApply tree type: ${tree.tpe.show}, method type: ${mt.resType.show}")
                    mt.resType
                case _ => tree.tpe
            }
            if (isReifiedParamMethod(resType)){
                report.error(s"AddReifiedTypes: ERROR: TransformApply: method ${sym.name} not found in symbolMap but has reified param in result type: ${resType.show}")
                tree
            } else tree
        }
    }

    def isReifiedParamMethod(tpe: Type)(using Context): Boolean = tpe match {
        case mt: MethodType =>
            mt.paramNames.nonEmpty && mt.paramNames.head.toString.startsWith("reified$")
        case _ => false
    }

    def collectReifiedArgs(tree: Tree)(using Context): List[Tree] = {
        var targs = List.empty[Tree]
        def traverse(t: Tree): Unit = t match {
            case Apply(fn, _) => traverse(fn)
            case TypeApply(fn, args) => 
                targs = args ::: targs
                traverse(fn)
            case Select(qual, _) => 
                if (qual.isInstanceOf[New]) traverse(qual)
                else traverse(qual)
            case New(tpt) => traverse(tpt)
            case AppliedTypeTree(fn, args) =>
                targs = args ::: targs
                traverse(fn)
            case _ =>
        }
        traverse(tree)
        targs.map( arg => createReifiedValue(arg.tpe))
    }

    // create reified value for a class type parameter
    def createReifiedValueFromClass(tpe: Type, currentOwner: Symbol)(using Context): Tree = {
        val typeParamSym = tpe.typeSymbol
        val paramName = typeParamSym.name
        val paramOwner = typeParamSym.owner
        var current = currentOwner
        var found: Option[Tree] = None

        // go up the owner chain to find the reified symbol
        while (current != NoSymbol && found.isEmpty){
            if (current.isClass && classFieldSyms.contains(current)){
                val fieldMap = classFieldSyms(current)
                if (fieldMap.contains(paramName))
                    val fieldSym = fieldMap(paramName)
                    found = Some(This(current.asClass).select(fieldSym))
            }
            current = current.owner
        }
        found match {
            case None => 
                report.error(s"AddReifiedTypes: ERROR: createReifiedValueFromClass: no reified field found for class type param ${paramName} in owner ${currentOwner.name}")
                Literal(Constant(0.toByte)).withType(defn.ReifiedValueType)
            case Some(value) => value
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
                       createReifiedValueFromClass(tr, ctx.owner)
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