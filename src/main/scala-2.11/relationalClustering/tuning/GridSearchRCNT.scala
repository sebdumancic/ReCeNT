package relationalClustering.tuning

import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.UnionCombination
import relationalClustering.clustering.AbstractSKLearnCluster
import relationalClustering.clustering.evaluation.AbstractEvaluatorModel
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.similarity.SimilarityNeighbourhoodTrees

/**
  * Created by seb on 16.02.16.
  */
class GridSearchRCNT(override protected val stepSize: Double,
                     override protected val depth: Int,
                     override protected val query: List[String],
                     override protected val k: Int,
                     override protected val knowledgeBase: KnowledgeBase,
                     override protected val bagCompare: AbstractBagComparison,
                     override protected val clustering: AbstractSKLearnCluster,
                     override protected val evaluateCluster: AbstractEvaluatorModel,
                     override protected val rootFolder: String = "./tmp") extends GridSearchFixedK(stepSize, depth, query, k, knowledgeBase, bagCompare, clustering, evaluateCluster, rootFolder){

  /** Returns the similarity measure to optimize */
  def getSimilarityMeasure(parameters: List[Double]) = {
    new SimilarityNeighbourhoodTrees(getKB, getDepth, parameters, bagCompare, new UnionCombination(), false)
  }

}
