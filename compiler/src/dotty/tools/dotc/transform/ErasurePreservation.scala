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

class ErasurePreservation extends MiniPhase {

  override def phaseName: String = ErasurePreservation.name

  override def description: String = ErasurePreservation.description

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree =
    val sym = tree.symbol
    sym.info match
        case pt: PolyType =>
          def toTypeB(tp: Type): TypeB = tp match
            case tpr: TypeParamRef => TypeB.M(tpr.paramNum)
            case _ => TypeB.None
          pt.resType match
            case mt: MethodType =>
              sym.addAnnotation(ErasedInfo(pt.paramInfos.size, mt.paramInfos.map(toTypeB), toTypeB(mt.resType)))
            case other =>
              sym.addAnnotation(ErasedInfo(pt.paramInfos.size, Nil, toTypeB(other.widenExpr)))
        case _ => ()
    tree
}

object ErasurePreservation {
  val name: String = "erasure preservation"
  val description: String = "preserve information in annotations before erasure"
}

enum TypeB:
  case None
  case M(x: Int)
// case class TypeB(tp: Type)

class ErasedInfo(paramCount: Int, paramType: List[TypeB], returnType: TypeB) extends Annotation {
  override def tree(using Context) =
    tpd.New(defn.SourceFileAnnot.typeRef,
            List(tpd.Literal(Constant(toString))))

  override def toString =
    s"$paramCount, $paramType, $returnType"
}
