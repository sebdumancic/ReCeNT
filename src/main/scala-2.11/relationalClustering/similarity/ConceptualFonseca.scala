package relationalClustering.similarity

import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bags.bagComparison.ChiSquaredDistance
import relationalClustering.neighbourhood.NeighbourhoodTree
import relationalClustering.representation.domain.KnowledgeBase

/**
  * Created by seb on 05.04.16.
  */
class ConceptualFonseca(override protected val knowledgeBase: KnowledgeBase,
                        protected val maxClauseLength: Int) extends SimilarityNeighbourhoodTrees(knowledgeBase, 0, List(1.0,0.0,0.0,0.0,0.0), new ChiSquaredDistance, null, List[AbstractAggregator](), true) {

  protected def getClauseLength: Int = {
    maxClauseLength
  }

  override def getFilename(domains: List[String]): String = {
    s"${domains.mkString(",")}_clause${getClauseLength}_conceptualclusteringfonseca.txt"
  }

  override protected def attributeSimilarity(ng1: NeighbourhoodTree, ng2: NeighbourhoodTree): Double = {
    val attrs1 = ng1.getClauses(getClauseLength)
    val attrs2 = ng2.getClauses(getClauseLength)

    attrs1.intersect(attrs2).size.toDouble/math.max(attrs1.union(attrs2).size.toDouble, 1.0)
  }

}
