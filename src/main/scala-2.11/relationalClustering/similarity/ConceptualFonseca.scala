package relationalClustering.similarity

import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.domain.KnowledgeBase

/**
  * Created by seb on 05.04.16.
  */
class ConceptualFonseca(override protected val knowledgeBase: KnowledgeBase,
                        override protected val depth: Int,
                        override protected val bagCompare: AbstractBagComparison) extends SimilarityNeighbourhoodTrees(knowledgeBase, depth, List(1.0,0.0,0.0,0.0,0.0), bagCompare, null, true) {

  override def getFilename(domains: List[String]) = {
    s"${domains.mkString(",")}_depth${depth}_conceptualclusteringfonseca.txt"
  }

  override protected def attributeSimilarity(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val attrs1 = ng1.getClauses(getDepth)
    val attrs2 = ng2.getClauses(getDepth)

    attrs1.intersect(attrs2).size.toDouble/attrs1.union(attrs2).size.toDouble
  }

}
