package relationalClustering.parameterLearning

import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.AbstractBagCombine
import relationalClustering.clustering.evaluation.LabelsContainer
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.similarity.SimilarityNeighbourhoodTrees

/**
  * Created by seb on 16.01.17.
  */
class LearnWeightsLPSupervised(protected val labels: LabelsContainer,
                               protected val knowledgeBase: KnowledgeBase,
                               protected val domain: String,
                               protected val treeDepth: Int,
                               protected val bagComparison: AbstractBagComparison,
                               protected val bagCombination: AbstractBagCombine,
                               protected val aggregates: List[AbstractAggregator]) {

  val firstCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(1.0, 0.0, 0.0, 0.0, 0.0), bagComparison, bagCombination, aggregates)
  val secondCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 1.0, 0.0, 0.0, 0.0), bagComparison, bagCombination, aggregates)
  val thirdCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 0.0, 1.0, 0.0, 0.0), bagComparison, bagCombination, aggregates)
  val fourthCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 0.0, 0.0, 1.0, 0.0), bagComparison, bagCombination, aggregates)
  val fifthCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 0.0, 0.0, 0.0, 1.0), bagComparison, bagCombination, aggregates)

  val objectsByClasses: Map[String, List[String]] = labels.getAllPairs.toList.groupBy(_._2).map(item => (item._1, item._2.map(_._1))).filterNot(_._1 == "xxxxxx")


  // necessary to get the normalization constants
  firstCompSim.getObjectSimilarity(List(domain), labels.getAllPairs.toList.map(item => (item._1, domain)))
  secondCompSim.getObjectSimilarity(List(domain), labels.getAllPairs.toList.map(item => (item._1, domain)))
  thirdCompSim.getObjectSimilarity(List(domain), labels.getAllPairs.toList.map(item => (item._1, domain)))
  fourthCompSim.getObjectSimilarity(List(domain), labels.getAllPairs.toList.map(item => (item._1, domain)))
  fifthCompSim.getObjectSimilarity(List(domain), labels.getAllPairs.toList.map(item => (item._1, domain)))

  def learn() = {
    //sum(same class)(similarities of elements) - sum(pairs of classes different from each other)(similarities of elements)
  }

}
