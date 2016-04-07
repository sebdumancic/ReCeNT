package relationalClustering.similarity

import relationalClustering.bagComparison.ChiSquaredDistance
import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.domain.KnowledgeBase

/**
  * Created by seb on 05.04.16.
  */
class ConceptualFonseca(override protected val knowledgeBase: KnowledgeBase,
                        protected val maxClauseLength: Int) extends SimilarityNeighbourhoodTrees(knowledgeBase, 0, List(1.0,0.0,0.0,0.0,0.0), new ChiSquaredDistance, null, true) {

  protected def getClauseLength = {
    maxClauseLength
  }

  override def getFilename(domains: List[String]) = {
    s"${domains.mkString(",")}_clause${getClauseLength}_conceptualclusteringfonseca.txt"
  }

  override protected def attributeSimilarity(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val attrs1 = ng1.getClauses(getClauseLength)
    val attrs2 = ng2.getClauses(getClauseLength)

    attrs1.intersect(attrs2).size.toDouble/math.max(attrs1.union(attrs2).size.toDouble, 1.0)
  }

}
