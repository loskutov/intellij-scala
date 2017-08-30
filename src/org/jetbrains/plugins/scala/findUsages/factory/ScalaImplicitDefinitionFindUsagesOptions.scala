package org.jetbrains.plugins.scala.findUsages.factory

import com.intellij.find.findUsages.JavaFindUsagesOptions
import com.intellij.openapi.project.Project

/**
  * @author Ignat Loskutov
  */
class ScalaImplicitDefinitionFindUsagesOptions(project: Project) extends JavaFindUsagesOptions(project) {
  isSearchForTextOccurrences = false

  override def equals(o: Any): Boolean = o match {
    case other: ScalaImplicitDefinitionFindUsagesOptions => super.equals(o)
    case _                                               => false
  }

}
