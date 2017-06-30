package org.jetbrains.plugins.scala.extensions.implementation.iterator

import com.intellij.psi.PsiElement

/**
 * Pavel.Fatin, 11.05.2010
 */

class ParentsIterator(element: PsiElement, strict: Boolean = true) extends Iterator[PsiElement] {
  private var current = if (strict && element != null) element.getParent else element

  def hasNext: Boolean = current != null

  def next(): PsiElement = {
    val result = current
    current = current.getParent
    result
  }
}