package org.jetbrains.plugins.scala.annotator

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter

class ImplicitParameterResolutionTest extends ScalaLightCodeInsightFixtureTestAdapter {
  def testCorrectImplicits(): Unit = {
    checkTextHasNoErrors(
      """
        |object A{
        |  implicit val implicitInt = 42
        |  val v1: Int = implicitly
        |  val v2 = implicitly[Int]
        |}
      """.stripMargin)
  }
}
