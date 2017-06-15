package relationalClustering.clustering.evaluation.semisupervised

import relationalClustering.clustering.evaluation.supervised.LabelsContainer
import relationalClustering.representation.domain.{Domain, KnowledgeBase}

import scala.util._

/**
  * Created by seb on 14.09.16.
  */
class SampleConstraints(protected val labelsContainer: LabelsContainer,
                        protected val domain: Domain,
                        protected val knowledgeBase: KnowledgeBase,
                        protected val treeDepth: Int) {

  val groupedByLabels = domain.getElements.map(e => (e, labelsContainer.getLabel(e))).groupBy(_._2).toList.map(item => item._2.map(_._1)).map(x => x.toList)


  def sample_must_link(numConstraints: Int, container: ConstraintsContainer) = {
    val perLabel = (numConstraints.toDouble / groupedByLabels.size).toInt
    val roll = new Random()

    groupedByLabels.foreach(sameLabelSet => {
      (1 to perLabel).foreach(turn => {
        container.addMustLink(sameLabelSet(roll.nextInt(sameLabelSet.length)), sameLabelSet(roll.nextInt(sameLabelSet.length)))
      })
    })
  }

  def sample_cannot_link(numConstraints: Int, container: ConstraintsContainer) = {
    val maxLabelInd = groupedByLabels.size

    (1 to numConstraints).foreach(turn => {

      // choose labels
      val label1 = Random.nextInt(maxLabelInd)
      val label2 = Random.nextInt(maxLabelInd) match {
        case x if x == label1 => (x + 1) % maxLabelInd
        case x if x != label1 => x
      }

      container.addCannotLink(groupedByLabels(label1)(Random.nextInt(groupedByLabels(label1).length)), groupedByLabels(label2)(Random.nextInt(groupedByLabels(label2).length)))
    })
  }

  def sample(numConstraints: Int) = {
    val constraints = new ConstraintsContainer("", domain.getName, knowledgeBase, treeDepth)

    sample_must_link(numConstraints, constraints)
    sample_cannot_link(numConstraints, constraints)

    constraints

  }

}
