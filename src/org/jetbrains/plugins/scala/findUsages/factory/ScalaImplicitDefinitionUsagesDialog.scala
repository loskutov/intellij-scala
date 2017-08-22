package org.jetbrains.plugins.scala.findUsages.factory

import java.awt.event.{ItemEvent, ItemListener}
import javax.swing._

import com.intellij.find.findUsages._
import com.intellij.openapi.module.{ModuleManager, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.OrderRootType
import com.intellij.openapi.ui.ex.MultiLineLabel
import com.intellij.ui.IdeBorderFactory
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScMember
import org.jetbrains.plugins.scala.project.{ModuleExt, ProjectExt}

/**
 * @author Ignat Loskutov
 */
class ScalaImplicitDefinitionUsagesDialog(element: ScReferencePattern,
                                          project: Project,
                                          findUsagesOptions: FindUsagesOptions,
                                          toShowInNewTab: Boolean,
                                          mustOpenInNewTab: Boolean,
                                          isSingleFile: Boolean,
                                          handler: FindUsagesHandler)
    extends JavaFindUsagesDialog[ScalaImplicitDefinitionFindUsagesOptions](
      element,
      project,
      findUsagesOptions,
      toShowInNewTab,
      mustOpenInNewTab,
      isSingleFile,
      handler
    ) {

  protected override def createFindWhatPanel: JPanel =
    if (!isLocalOrPrivate(getParentOfType(element, classOf[ScMember]))) {
      val pluginName  = "scalahost" // to be renamed to `semanticdb`
      val modules     = ModuleManager.getInstance(project).getModules
      val virtFileUrl = element.getNavigationElement.getContainingFile.getVirtualFile.getPresentableUrl
      val targetModules = (if (virtFileUrl.startsWith("jar://")) {
                             val maybeLib = project.libraries.find(
                               _.getUrls(OrderRootType.SOURCES).exists(url => virtFileUrl.startsWith(url))
                             )
                             maybeLib.map(lib => modules.filter(_.libraries.contains(lib)).toSeq)
                           } else {
                             import scala.collection.JavaConverters._
                             val maybeModule = Option(ScalaPsiUtil.getModule(element))
                             maybeModule.map(m => m +: ModuleUtilCore.getAllDependentModules(m).asScala)
                           }).get.toSet
      val modulesWithPlugin    = targetModules.filter(_.scalaCompilerSettings.plugins.exists(_.contains(pluginName)))
      val modulesWithoutPlugin = targetModules &~ modulesWithPlugin
      val whatPanel            = new JPanel()
      if (modulesWithPlugin.nonEmpty) {
        val options = findUsagesOptions.asInstanceOf[ScalaImplicitDefinitionFindUsagesOptions]
        if (modulesWithoutPlugin.nonEmpty) {
          options.useSemanticDb = false
          val desc =
            s"""You are willing to search for uses of a non-local implicit definition.
               |In order to find its implicit usages, up-to-date semantic information can be used.
               |The $pluginName compiler plugin is, in turn, required to collect this information.
               |It's absent in the following modules: ${modulesWithoutPlugin.mkString(", ")}
               |It can make the search more fast and reliable in some cases.
               |Be careful: this feature is in a "highly-experimental" state!""".stripMargin
          whatPanel.setBorder(IdeBorderFactory.createTitledBorder("semanticdb"))
          whatPanel.setLayout(new BoxLayout(whatPanel, BoxLayout.Y_AXIS))
          val label = new MultiLineLabel(desc)
          val searchWithSemanticdbButton = new JRadioButton(
            modulesWithPlugin.map(_.getName).mkString("Search in the following modules: ", ", ", "")
          )
          val fallbackButton = new JRadioButton("Fallback to the stable implementation", true)
          searchWithSemanticdbButton.addItemListener(new ItemListener {
            override def itemStateChanged(e: ItemEvent): Unit = e.getStateChange match {
              case ItemEvent.SELECTED   => options.useSemanticDb = true
              case ItemEvent.DESELECTED => options.useSemanticDb = false
            }
          })

          val rbg = new ButtonGroup()
          rbg.add(searchWithSemanticdbButton)
          rbg.add(fallbackButton)
          whatPanel.add(label)
          whatPanel.add(searchWithSemanticdbButton)
          whatPanel.add(fallbackButton)
        } else {
          options.useSemanticDb = true
        }
        whatPanel
      } else {
        val label = new MultiLineLabel(
          """You are willing to search for uses of a non-local implicit definition.
            |It may take a long time on large projects, so please choose a narrow scope below.""".stripMargin
        )
        whatPanel.add(label)
        whatPanel
      }
    } else super.createFindWhatPanel()

}
