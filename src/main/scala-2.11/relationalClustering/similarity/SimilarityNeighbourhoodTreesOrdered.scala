package relationalClustering.similarity

import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.UnionCombination
import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.domain.KnowledgeBase

/**
  * Created by seb on 27.01.17.
  */
class SimilarityNeighbourhoodTreesOrdered(override protected val knowledgeBase: KnowledgeBase,
                                          override protected val depth: Int,
                                          override protected val weights: List[Double],
                                          override protected val bagCompare: AbstractBagComparison,
                                          protected val vertexCombination: String,
                                          override protected val aggregators: List[AbstractAggregator],
                                          override protected val useLocalRepo: Boolean = false) extends SimilarityNeighbourhoodTrees(knowledgeBase, depth, weights, bagCompare, new UnionCombination, aggregators, useLocalRepo) {

  def this(KB: KnowledgeBase, d: Int, ws: List[Double], bagComp: AbstractBagComparison, bagComb: String, aggs: List[AbstractAggregator], cache: Map[(String, String), NeighbourhoodGraph]) {
    this(KB, d, ws, bagComp, bagComb, aggs, false)
    setNeighbourhoodGraphs(cache)
  }

  def combinedIndividuals(individuals: List[Double]): Double = {
    vertexCombination match {
      case "avg" => individuals.sum / individuals.length
      case "min" => individuals.min
      case "max" => individuals.max
    }
  }

  override protected def hyperedgeAttributeSimilarity(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]): Double = {
    val individualMeasures = ngs1.zip(ngs2).map(ntrees => attributeSimilarity(ntrees._1, ntrees._2))
    combinedIndividuals(individualMeasures)
  }

  override protected def hyperEdgeAttributeNeighbourhoodSimilarity(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]): Double = {
    val individualMeasures = ngs1.zip(ngs2).map(ntrees => attributeNeighbourhoodSimilarity(ntrees._1, ntrees._2))
    combinedIndividuals(individualMeasures)
  }

  override protected def hyperEdgeConnections(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]): Double = {
    val individualMeasures = ngs1.zip(ngs2).map(ntrees => elementConnections(ntrees._1, ntrees._2))
    combinedIndividuals(individualMeasures)
  }

  override protected def hyperEdgeVertexDistribution(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]): Double = {
    val individualMeasures = ngs1.zip(ngs2).map(ntrees => vertexIdentityDistribution(ntrees._1, ntrees._2))
    combinedIndividuals(individualMeasures)
  }

  override protected def hyperEdgeEdgeDistribution(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]): Double = {
    val individualMeasures = ngs1.zip(ngs2).map(ntrees => edgeDistributionsSimilarity(ntrees._1, ntrees._2))
    combinedIndividuals(individualMeasures)
  }
}
