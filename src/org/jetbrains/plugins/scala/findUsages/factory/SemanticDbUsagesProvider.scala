package org.jetbrains.plugins.scala.findUsages.factory

import scala.meta.inputs.Input.{File, LabeledString}
import scala.meta.inputs.{Input, Position}
import scala.meta.io.{AbsolutePath, Classpath, Sourcepath}
import scala.meta.semantic.Database

/** An object providing API which relies on semanticdb to get implicit usages.
 * Supposed to be loaded with an isolated classloader, thus its API only operates with Java objects.
 */
object SemanticDbUsagesProvider {

  def occurrences(db: Database, needle: String): Array[(Position, String)] = {
    db.sugars.toArray.collect {
      case (pos: Position, sugar: String) if sugar.contains(needle) => (pos, sugar)
    }
  }

  private def getPath(input: Input): Option[String] = input match {
    case File(path, _)          => Some(path.toString)
    case LabeledString(path, _) => Some(path)
    case _                      => None // TODO: some cases might be missing
  }

  def findUsages(target: String, classpath: Array[String], sourcepath: String): Array[Array[AnyRef]] = {
    val database = Database.load(
      Classpath(classpath.map(AbsolutePath.apply)),
      Sourcepath(sourcepath)
    )
    for {
      (pos, sugar) <- occurrences(database, target)
      path         <- getPath(pos.input)
    } yield Array(path.asInstanceOf[AnyRef], Int.box(pos.start.offset), sugar)
  }

}
