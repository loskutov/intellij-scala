package org.jetbrains.plugins.scala.findUsages.factory

import com.intellij.find.findUsages.JavaFindUsagesOptions
import com.intellij.openapi.project.Project

/**
  * @author Ignat Loskutov
  */
class ScalaImplicitDefinitionFindUsagesOptions(project: Project) extends JavaFindUsagesOptions(project) {
  isSearchForTextOccurrences = false

  var useSemanticDb = false

  override def equals(o: Any): Boolean = {
    o match {
      case other: ScalaImplicitDefinitionFindUsagesOptions =>
        super.equals(o) &&
          other.useSemanticDb == useSemanticDb
      case _ => false
    }
  }

  override def hashCode(): Int = {
    var res = super.hashCode()
    res = 2 * res + (if (useSemanticDb) 1 else 0)
    res
  }
}
