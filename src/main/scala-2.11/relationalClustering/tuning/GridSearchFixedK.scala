package relationalClustering.tuning

import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.clustering.AbstractSKLearnCluster
import relationalClustering.clustering.evaluation.AbstractEvaluatorModel
import relationalClustering.representation.KnowledgeBase
import relationalClustering.similarity.AbstractSimilarityMeasure

/** Implements brute force search over the best parameters for similarity measure
  * Created by seb on 15.02.16.
  */
abstract class GridSearchFixedK(protected val stepSize: Double,
                                protected val depth: Int,
                                protected val query: List[String],
                                protected val k: Int,
                                protected val knowledgeBase: KnowledgeBase,
                                protected val bagCompare: AbstractBagComparison,
                                protected val clustering: AbstractSKLearnCluster,
                                protected val evaluateCluster: AbstractEvaluatorModel,
                                protected val rootFolder: String = "./tmp") {

  /** Returns the root folder*/
  protected def getRoot = {
    rootFolder
  }

  /** Returns the specified depth */
  protected def getDepth = {
    depth
  }

  /** Returns the list of queries */
  protected def getQueries = {
    query
  }

  /** Returns the number of clusters */
  protected def getK = {
    k
  }

  /** Returns the Knowledge base */
  protected def getKB = {
    knowledgeBase
  }

  /** Generates of all the parameter combination for the similarity measure
    *
    * @return set of parameter combinations: [[List]] [ [[Double]] ]
    * */
  def getParameters = {
    val parameterDomains = (1 to 5).map(x => (0.0 to 1.0 by stepSize).toList).map( x => x.map(y => List[Double](y)))

    parameterDomains.reduceLeft( (x,y) => {
      for{xs <-x; ys <-y} yield xs ++ ys
    }).filter( _.sum == 1.0).toSet
  }

  /** Same as [[getParameters]] but filters out parameters with zero values*/
  def getParametersNoZeros = {
    getParameters.filterNot( _.contains(0.0))
  }

  /** Returns the similarity measure to optimize */
  def getSimilarityMeasure(parameters: List[Double]): AbstractSimilarityMeasure

  /** Evaluates the provided parameters
    *
    * @param pars a set of parameters
    * @return [[Double]]
    * */
  def evaluateParameters(pars: List[Double]): Double = {
    val sim = getSimilarityMeasure(pars)

    val filename = sim.getObjectSimilaritySave(getQueries, getRoot)
    val cluster = clustering.clusterFromFile(filename._1, getK)

    evaluateCluster.validate(cluster, filename._2.map( _._1), filename._1)
  }

  /** Performs the grid search and returns the best clustering, together with the corresponding parameters
    *
    * @return (parameters, clustering)
    * */
  def findBestParameters = {
    val results = collection.mutable.Map[List[Double], Double]()

    getParameters.foreach( parSet => {
      results(parSet) = evaluateParameters(parSet)
    })

    results.maxBy(_._2)
  }



}
