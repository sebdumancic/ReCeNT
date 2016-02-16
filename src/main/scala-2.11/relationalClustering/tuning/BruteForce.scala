package relationalClustering.tuning

import relationalClustering.clustering.AbstractCluster
import relationalClustering.clustering.evaluation.AbstractEvaluatorModel
import relationalClustering.similarity.AbstractSimilarityMeasure

/**
  * Created by seb on 15.02.16.
  */
class BruteForce(protected val stepSize: Double,
                 protected val clusteringAlgorithm: AbstractCluster,
                 protected val similarity: AbstractSimilarityMeasure,
                 protected val evaluationMethod: AbstractEvaluatorModel) {

  def getParameters() = {
    val parameterDomains = (1 to 5).map(x => scala.collection.immutable.NumericRange(0.0,1.0, stepSize).toList )

  }

}
