package org.jetbrains.plugins.scala.codeInspection.syntacticClarification

import com.intellij.codeInspection.{LocalQuickFixOnPsiElement, ProblemsHolder}
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.codeInspection.AbstractInspection
import org.jetbrains.plugins.scala.lang.transformation.AbstractTransformer

/**
  * @author Pavel Fatin
  */
class TransformerBasedInspection(name: String, solution: String, transformer: AbstractTransformer) extends AbstractInspection(name) {

  override def actionFor(implicit holder: ProblemsHolder): PartialFunction[PsiElement, Any] = new PartialFunction[PsiElement, Any] {
    def isDefinedAt(e: PsiElement): Boolean = transformer.transformation(e.getProject).isDefinedAt(e)

    def apply(e: PsiElement): Unit = {
      holder.registerProblem(e, name, new LocalQuickFixOnPsiElement(e) {
        def invoke(project: Project, psiFile: PsiFile, psiElement: PsiElement, psiElement1: PsiElement) {
          transformer.transformation(e.getProject).apply(e)
        }

        def getText: String = solution

        def getFamilyName: String = getText
      })
    }
  }
}
