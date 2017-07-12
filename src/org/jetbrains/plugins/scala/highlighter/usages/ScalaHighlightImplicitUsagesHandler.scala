package org.jetbrains.plugins.scala.highlighter.usages

import java.util

import com.intellij.codeInsight.highlighting.HighlightUsagesHandlerBase
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.util.Consumer
import org.jetbrains.plugins.scala.codeInspection.collections.MethodRepr
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression.ExpressionTypeResult
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScMethodCall, ScParenthesisedExpr}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.types.result.Success

import scala.annotation.tailrec
import scala.collection.JavaConverters._

class ScalaHighlightImplicitUsagesHandler(editor: Editor, file: PsiFile, target: ScalaPsiElement)
    extends HighlightUsagesHandlerBase[ScalaPsiElement](editor, file) {

  private def isImplicitConversionOf(target: ScalaPsiElement, e: ScExpression): Boolean =
    e.getTypeAfterImplicitConversion() match {
      case ExpressionTypeResult(Success(_, _), _, Some(implicitFunction)) =>
        implicitFunction match {
          case `target`        => true
          case fun: ScFunction => fun.getSyntheticNavigationElement.contains(target)
          case _               => false
        }
      case _ => false
    }

  private def isImplicitParameterOf(target: ScalaPsiElement, e: ScExpression): Boolean = {
    val pars = e.findImplicitParameters.getOrElse(Seq.empty)
    pars.exists(_.element == target)
  }

  @tailrec
  private def refNameId(expr: PsiElement): PsiElement = expr match {
    case mc: ScMethodCall if mc.getFirstChild.isInstanceOf[ScParenthesisedExpr] => mc.getFirstChild
    case MethodRepr(_: ScMethodCall, Some(base), None, _)                       => refNameId(base)
    case MethodRepr(_, _, Some(ref), _)                                         => ref.nameId
    case _                                                                      => expr
  }

  override def getTargets: util.List[ScalaPsiElement] = util.Collections.singletonList(target)

  override def selectTargets(targets: util.List[ScalaPsiElement],
                             selectionConsumer: Consumer[util.List[ScalaPsiElement]]): Unit =
    selectionConsumer.consume(targets)

  override def computeUsages(targets: util.List[ScalaPsiElement]): Unit = {
    val usages = targets.asScala.flatMap { target =>
      file
        .depthFirst()
        .collect {
          case e: ScExpression if isImplicitParameterOf(target, e) || isImplicitConversionOf(target, e) =>
            val nameRange = refNameId(e).getTextRange
            TextRange.create(nameRange.getStartOffset, e.getTextOffset + e.getTextLength)
        }
    }
    myReadUsages.addAll(usages.asJava)
  }

  override def highlightReferences: Boolean = true
}
