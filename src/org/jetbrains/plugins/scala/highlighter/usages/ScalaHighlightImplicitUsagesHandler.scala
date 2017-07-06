package org.jetbrains.plugins.scala.highlighter.usages

import java.util

import com.intellij.codeInsight.highlighting.HighlightUsagesHandlerBase
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.util.Consumer
import org.jetbrains.plugins.scala.codeInspection.collections.MethodRepr
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression.ExpressionTypeResult
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScMethodCall, ScParenthesisedExpr}
import org.jetbrains.plugins.scala.lang.psi.types.result.Success
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern

import scala.annotation.tailrec
import scala.collection.JavaConverters._

class ScalaHighlightImplicitUsagesHandler(editor: Editor, file: PsiFile, target: ScReferencePattern)
    extends HighlightUsagesHandlerBase[PsiElement](editor, file) {

  private def isImplicitlyConversionOf(target: ScReferencePattern, e: ScExpression): Boolean =
    e.getTypeAfterImplicitConversion() match {
      case ExpressionTypeResult(Success(_, _), _, Some(implicitFunction)) => implicitFunction == target
      case _                                                              => false
    }

  private def isImplicitParameterOf(target: ScReferencePattern, e: ScExpression): Boolean = {
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

  override def getTargets: util.List[PsiElement] = util.Collections.singletonList(target)

  override def selectTargets(targets: util.List[PsiElement], selectionConsumer: Consumer[util.List[PsiElement]]): Unit =
    selectionConsumer.consume(targets)

  override def computeUsages(targets: util.List[PsiElement]): Unit = {
    val usages = file.depthFirst().filter {
      case e: ScExpression => isImplicitParameterOf(target, e) || isImplicitlyConversionOf(target, e)
      case _               => false
    }
    myReadUsages.addAll(usages.map(refNameId(_).getTextRange).toSeq.asJavaCollection)
  }

  override def highlightReferences: Boolean = true
}
