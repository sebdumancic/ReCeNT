package relationalClustering.similarity

import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.UnionCombination
import relationalClustering.neighbourhood.NeighbourhoodTree
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

  def this(KB: KnowledgeBase, d: Int, ws: List[Double], bagComp: AbstractBagComparison, bagComb: String, aggs: List[AbstractAggregator], cache: Map[(String, String), NeighbourhoodTree]) {
    this(KB, d, ws, bagComp, bagComb, aggs, false)
    setNeighbourhoodGraphs(cache)
  }

  override def getFilename(domains: List[String]): String = {
    s"${domains.mkString("")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_localRepo${useLocal}_orderPreserved.txt"
  }

  override def getVertexNormsFilename(domains: List[String]): String = {
    s"${domains.mkString("")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_localRepo${useLocal}_orderPreserved.vnorms"
  }

  override def getEdgeNormsFilename(domains: List[String]): String = {
    s"${domains.mkString("")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_localRepo${useLocal}_orderPreserved.hnorms"
  }

  override def getFilenameHyperEdges(domains: List[String]): String = {
    s"${domains.mkString("")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_combination${bagCombine.getName}_localRepo${useLocal}_orderPreserved.txt"
  }


  def combinedIndividuals(individuals: List[Double]): Double = {
    vertexCombination match {
      case "avg" => individuals.sum / individuals.length
      case "min" => individuals.min
      case "max" => individuals.max
    }
  }

  override protected def hyperedgeAttributeSimilarity(ngs1: List[NeighbourhoodTree], ngs2: List[NeighbourhoodTree]): Double = {
    val individualMeasures = ngs1.zip(ngs2).map(ntrees => attributeSimilarity(ntrees._1, ntrees._2))
    combinedIndividuals(individualMeasures)
  }

  override protected def hyperEdgeAttributeNeighbourhoodSimilarity(ngs1: List[NeighbourhoodTree], ngs2: List[NeighbourhoodTree]): Double = {
    val individualMeasures = ngs1.zip(ngs2).map(ntrees => attributeNeighbourhoodSimilarity(ntrees._1, ntrees._2))
    combinedIndividuals(individualMeasures)
  }

  override protected def hyperEdgeConnections(ngs1: List[NeighbourhoodTree], ngs2: List[NeighbourhoodTree]): Double = {
    val individualMeasures = ngs1.zip(ngs2).map(ntrees => elementConnections(ntrees._1, ntrees._2))
    combinedIndividuals(individualMeasures)
  }

  override protected def hyperEdgeVertexDistribution(ngs1: List[NeighbourhoodTree], ngs2: List[NeighbourhoodTree]): Double = {
    val individualMeasures = ngs1.zip(ngs2).map(ntrees => vertexIdentityDistribution(ntrees._1, ntrees._2))
    combinedIndividuals(individualMeasures)
  }

  override protected def hyperEdgeEdgeDistribution(ngs1: List[NeighbourhoodTree], ngs2: List[NeighbourhoodTree]): Double = {
    val individualMeasures = ngs1.zip(ngs2).map(ntrees => edgeDistributionsSimilarity(ntrees._1, ntrees._2))
    combinedIndividuals(individualMeasures)
  }
}
