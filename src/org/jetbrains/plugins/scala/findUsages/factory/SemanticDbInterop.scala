package org.jetbrains.plugins.scala.findUsages.factory

import java.lang.reflect.Method
import java.net.URL

import com.intellij.openapi.module.ModuleManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.CompilerModuleExtension
import com.intellij.psi.{PsiElement, PsiManager}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createExpressionFromText
import org.jetbrains.plugins.scala.lang.refactoring.rename.RenameSuperMembersUtil.allSuperMembers

import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader

sealed trait SugarType {}
object ImplicitConversion extends SugarType {}
object ImplicitParameter  extends SugarType {}
object TypeParameter      extends SugarType {}

// Hopefully some day semantic.Database#sugars will provide this information
object SugarType {
  def apply(sugar: String): SugarType =
    if (sugar.startsWith("("))
      ImplicitParameter
    else if (sugar.startsWith("["))
      TypeParameter
    else
      ImplicitConversion
}

case class SemanticDbInterop(project: Project) {
  private def usageToElement(filePath: String, offset: Int, value: String): PsiElement = {
    val virtualFile = project.getBaseDir.findFileByRelativePath(filePath)
    val file        = inReadAction(PsiManager.getInstance(project).findFile(virtualFile))
    val properOffset = SugarType(value) match {
      case ImplicitConversion => offset
      case _                  => offset - 1
    }
    val elem         = file.findElementAt(properOffset)
    val parentMethod = inReadAction(ScalaPsiUtil.getParentOfType(elem, classOf[ScMethodCall], classOf[ScGenericCall]))
    parentMethod match {
      case null => elem
      case mc   => mc
    }
  }

  private val classpath: Array[String] = ModuleManager
    .getInstance(project)
    .getModules
    .map(module => new URL(CompilerModuleExtension.getInstance(module).getCompilerOutputUrl).getPath)

  private val sourcepath: String = project.getBasePath

  def findImplicitUsages(target: ScNamedElement): Seq[PsiElement] = {
    def elemMatches(elem: PsiElement): Boolean =
      elem match {
        case ne: ScNamedElement        => elemMatches(ne) || allSuperMembers(ne, withSelfType = false).exists(elemMatches(_))
        case re: ScReferenceExpression => inReadAction(re.resolve.getNavigationElement) == target
        case mc: ScMethodCall          => elemMatches(mc.getInvokedExpr) || mc.argumentExpressions.exists(elemMatches)
        case gc: ScGenericCall         => elemMatches(gc.referencedExpr)
        case _                         => false
      }

    SemanticDbInterop.findUsages
      .invoke(
        SemanticDbInterop.instance,
        inReadAction(target.getName),
        classpath,
        sourcepath
      )
      .asInstanceOf[Array[Array[AnyRef]]]
      .flatMap {
        case Array(path: String, offset: Integer, sugar: String) =>
          val usageElement = usageToElement(path, offset, sugar)
          val matches = SugarType(sugar) match {
            case TypeParameter => false
            case _ =>
              val expr = inReadAction(createExpressionFromText(sugar.replace(".this.", "."), usageElement))
              expr match {
                case par: ScParenthesisedExpr => par.expr.exists(elemMatches(_))
                case tpl: ScTuple             => tpl.exprs.exists(elemMatches(_))
                case _                        => elemMatches(expr)
              }
          }
          if (matches) Some(usageElement)
          else None
      }
  }
}

object SemanticDbInterop {
  private[this] val pluginCP = new URL(
    getClass
      .getResource(".")
      .toString
      .replaceAll("^jar:", "")
      .replaceAll("!/.+$", "")
      .replaceAll(getClass.getPackage.getName.replace(".", "/") + "/$", "")
      .replaceAll("/[^/]*$", "")
  )

  private[this] val clazz: Class[_] = new URLClassLoader(
    Seq(
      new URL(s"$pluginCP/scalameta120.jar"),
      new URL(s"$pluginCP/scala-library.jar"),
      new URL(s"$pluginCP/scala-plugin.jar"),
      new URL(s"$pluginCP/isolated/scalapb-runtime.jar"),
      new URL(s"$pluginCP/isolated/protobuf-java.jar"),
      new URL(s"$pluginCP/isolated/lenses.jar")
    ),
    null
  ).loadClass(SemanticDbUsagesProvider.getClass.getName)

  private val instance: AnyRef = clazz.getField("MODULE$").get(null)

  private val findUsages: Method = clazz.getDeclaredMethod(
    "findUsages",
    classOf[String],
    classOf[Array[String]],
    classOf[String]
  )
}
