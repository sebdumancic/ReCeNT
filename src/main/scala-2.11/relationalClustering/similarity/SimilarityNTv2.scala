package relationalClustering.similarity

import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.AbstractBagCombine
import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.KnowledgeBase

/**
  * Created by seb on 08.02.16.
  */
class SimilarityNTv2(override protected val knowledgeBase: KnowledgeBase,
                     override protected val depth: Int,
                     override protected val weights: List[Double],
                     override protected val bagCompare: AbstractBagComparison,
                     override protected val bagCombine: AbstractBagCombine,
                     override protected val useLocalRepo: Boolean = false) extends SimilarityNeighbourhoodTrees(knowledgeBase, depth, weights, bagCompare, bagCombine, useLocalRepo) {

  override def getFilename(domains: List[String]) = {
    s"${domains.mkString(",")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_useLocalRepo${useLocal}_compatible.txt"
  }

  override def getFilenameHyperEdges(domains: List[String]) = {
    s"${domains.mkString("_")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_localRepo${useLocal}_compatible.txt"
  }

  override protected def attributeNeighbourhoodSimilarity(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph): Double = {
    val firstGraph = ng1.getAttributeValueDistribution(0)
    val secondGraph = ng2.getAttributeValueDistribution(0)

    val firstAttributes = firstGraph.foldLeft(List[(String,String)]())( (acc, dom) => {
      acc ::: dom._2
    })
    val secondAttributes = secondGraph.foldLeft(List[(String,String)]())( (acc, dom) => {
      acc ::: dom._2
    })

    bagCompare.compareBags(firstAttributes.map( x => s"${x._1}${x._2}"), secondAttributes.map(x => s"${x._1}${x._2}"))
  }

}
