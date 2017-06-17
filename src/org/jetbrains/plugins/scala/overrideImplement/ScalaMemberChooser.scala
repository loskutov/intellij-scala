package org.jetbrains.plugins.scala
package overrideImplement

import java.awt.FlowLayout
import javax.swing.event.{HyperlinkEvent, HyperlinkListener, TreeSelectionEvent, TreeSelectionListener}
import javax.swing.{JComponent, JPanel}

import com.intellij.ide.util.MemberChooser
import com.intellij.psi._
import com.intellij.ui.{HyperlinkLabel, NonFocusableCheckBox}
import com.intellij.util.ui.ThreeStateCheckBox
import com.intellij.util.ui.ThreeStateCheckBox.State
import org.jetbrains.plugins.scala.lang.formatting.settings.{ScalaCodeStyleSettings, TypeAnnotationPolicy, TypeAnnotationRequirement}
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScTemplateDefinition}
import org.jetbrains.plugins.scala.settings.ScalaApplicationSettings
import org.jetbrains.plugins.scala.util.TypeAnnotationUtil._

import scala.collection.mutable

/**
 * Nikolay.Tropin
 * 2014-03-25
 */
class ScalaMemberChooser[T <: ClassMember : scala.reflect.ClassTag](elements: Array[T],
                         allowEmptySelection: Boolean,
                         allowMultiSelection: Boolean,
                         needAddOverrideChb: Boolean,
                         needSpecifyTypeChb: Boolean,
                         needCopyScalaDocChb: Boolean,
                         targetClass: ScTemplateDefinition)
        extends {
          val addOverrideModifierChb = new NonFocusableCheckBox(ScalaBundle.message("add.override.modifier"))
          val copyScalaDocChb = new NonFocusableCheckBox(ScalaBundle.message("copy.scaladoc"))
          val typePanel = new JPanel()

          private val otherComponents = Array[JComponent](copyScalaDocChb, addOverrideModifierChb, typePanel)
          private val sortedElements = ScalaMemberChooser.sorted(elements, targetClass)

        } with MemberChooser[T](sortedElements.toArray[T], allowEmptySelection, allowMultiSelection, targetClass.getProject, null, otherComponents) {

  val mySpecifyTypeChb = new ThreeStateCheckBox(ScalaBundle.message("specify.return.type.explicitly"))

  setUpSettingsPanel()
  setUpTypePanel()
  trackSelection()

  override def doOKAction(): Unit = {
    ScalaApplicationSettings.getInstance.ADD_OVERRIDE_TO_IMPLEMENTED = addOverrideModifierChb.isSelected
    ScalaApplicationSettings.getInstance.COPY_SCALADOC = copyScalaDocChb.isSelected

    ScalaApplicationSettings.getInstance().SPECIFY_RETURN_TYPE_EXPLICITLY = {
      mySpecifyTypeChb.getState match {
        case State.SELECTED => ScalaApplicationSettings.ReturnTypeLevel.ADD
        case State.NOT_SELECTED => ScalaApplicationSettings.ReturnTypeLevel.REMOVE
        case State.DONT_CARE => ScalaApplicationSettings.ReturnTypeLevel.BY_CODE_STYLE
      }
    }

    super.doOKAction()
  }

  private def setUpSettingsPanel(): Unit ={
    typePanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0))

    addOverrideModifierChb.setSelected(ScalaApplicationSettings.getInstance().ADD_OVERRIDE_TO_IMPLEMENTED)
    addOverrideModifierChb.setVisible(needAddOverrideChb)

    copyScalaDocChb.setSelected(ScalaApplicationSettings.getInstance().COPY_SCALADOC)
    copyScalaDocChb.setVisible(needCopyScalaDocChb)
  }

  private def setUpTypePanel(): JPanel ={
    updateSpecifyTypeChb()
    typePanel.add(mySpecifyTypeChb)

    val myLinkContainer = new JPanel
    myLinkContainer.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0))
    typePanel.add(myLinkContainer)

    myLinkContainer.add(setUpHyperLink())
    typePanel.setVisible(needSpecifyTypeChb)
    typePanel
  }

  private def setUpHyperLink(): HyperlinkLabel = {
    val link = createTypeAnnotationsHLink(targetClass.getProject, ScalaBundle.message("default.ta.settings"))

    link.addHyperlinkListener(new HyperlinkListener {
      override def hyperlinkUpdate(e: HyperlinkEvent): Unit = extensions.invokeLater(updateSpecifyTypeChb())
    })

    link
  }

  private def updateSpecifyTypeChb(): Unit = mySpecifyTypeChb.setState(computeCheckBoxState)

  private def trackSelection(): Unit = {
    val selectionListener = new TreeSelectionListener {
      override def valueChanged(e: TreeSelectionEvent): Unit = {
        updateSpecifyTypeChb()
      }
    }

    // Reorder listeners. We need to know information about current selected element,
    // but, if we add new listener it will be called before MemberChooser listener,
    // and mySelectedElements in our listener will have previous information
    val listeners = myTree.getTreeSelectionListeners
    listeners.foreach(myTree.removeTreeSelectionListener)
    myTree.addTreeSelectionListener(selectionListener)
    listeners.foreach(myTree.addTreeSelectionListener)
  }

  private def computeCheckBoxState: ThreeStateCheckBox.State = {
    import scala.collection.JavaConversions._

    val distintValues = mySelectedElements.iterator
      .collect{case member: ClassMember => member}
      .map(member => typeAnnotationNeeded(member.getElement))
      .toList.distinct

    distintValues.length match {
      case 1 => if (distintValues.head) State.SELECTED else State.NOT_SELECTED
      case _ => State.DONT_CARE
    }
  }

  private def typeAnnotationNeeded(element: PsiElement): Boolean = {
    val isOverriding = !element.isInstanceOf[ScTypedDeclaration]
    val simple = isSimple(element)

    implicit val pair = (isOverriding, simple)

    def default = isTypeAnnotationNeeded(
      TypeAnnotationRequirement.Preferred.ordinal(),
      TypeAnnotationPolicy.Regular.ordinal,
      TypeAnnotationPolicy.Optional.ordinal
    )

    implicit val settings = ScalaCodeStyleSettings.getInstance(element.getProject)

    element match {
      case _: ScPatternDefinition | _: ScVariableDefinition |
           _: ScVariableDeclaration | _: ScValueDeclaration =>
        isTypeAnnotationNeededProperty(element.asInstanceOf[ScMember])
      case _: ScFunctionDeclaration | _: ScFunctionDefinition =>
        isTypeAnnotationNeededMethod(element.asInstanceOf[ScMember])
      case modifierListOwner: PsiModifierListOwner =>
        val visibility = Visibility(modifierListOwner)

        modifierListOwner match {
          case _: PsiMethod =>
            isTypeAnnotationNeededMethod(visibility)
          case _: PsiField =>
            isTypeAnnotationNeededProperty(visibility)
          case _ => default
        }
      case _ => default
    }
  }
}

object ScalaMemberChooser {
  def sorted[T <: ClassMember](members: Seq[T], targetClass: ScTemplateDefinition): Seq[T] = {
    val groupedMembers = members.groupBy(cm => cm.getElement.getContainingClass)
    val sortedClasses = mutable.LinkedHashSet[PsiClass]()
    if (targetClass != null) {
      val supers = targetClass.supers
      sortedClasses ++= supers
    }
    val ordering = new Ordering[PsiClass] {
      override def compare(c1: PsiClass, c2: PsiClass): Int = {
        val less = c1.isInheritor(c2, /*checkDeep =*/ true)
        val more = c2.isInheritor(c1, /*checkDeep =*/ true)
        if (less && more) 0 //it is possible to have cyclic inheritance for generic traits in scala
        else if (less) -1
        else 1
      }
    }
    sortedClasses ++= groupedMembers.keys.toSeq.sorted(ordering)

    sortedClasses.flatMap(c => groupedMembers.getOrElse(c, Seq.empty)).toSeq
  }
}
