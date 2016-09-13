package relationalClustering.parameterLearning

import breeze.linalg.DenseVector
import breeze.optimize.{DiffFunction, LBFGS}
import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.AbstractBagCombine
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.similarity.SimilarityNeighbourhoodTrees

/**
  * Created by seb on 13.09.16.
  */
class LearnWeights(protected val constraints: ConstraintsContainer,
                   protected val knowledgeBase: KnowledgeBase,
                   protected val treeDepth: Int,
                   protected val bagComparison: AbstractBagComparison,
                   protected val bagCombination: AbstractBagCombine,
                   protected val aggregates: List[AbstractAggregator]) {

  val firstCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(1.0, 0.0, 0.0, 0.0, 0.0), bagComparison, bagCombination, aggregates)
  val secondCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 1.0, 0.0, 0.0, 0.0), bagComparison, bagCombination, aggregates)
  val thirdCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 0.0, 1.0, 0.0, 0.0), bagComparison, bagCombination, aggregates)
  val fourthCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 0.0, 0.0, 1.0, 0.0), bagComparison, bagCombination, aggregates)
  val fifthCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 0.0, 0.0, 0.0, 1.0), bagComparison, bagCombination, aggregates)
  val orderedForGrad = List(firstCompSim, secondCompSim, thirdCompSim, fourthCompSim, fifthCompSim)

  val correction = constraints.getMustLink.size.toDouble / constraints.getCannotLink.size.toDouble

  def getDiffFunction = {

    new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val tmpSimM = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, x.toScalaVector.toList.take(5), bagComparison, bagCombination, aggregates)

        val myVal = {
          val eval = -constraints.getMustLink.map(t => tmpSimM.pairObjectSimilarity(t._1, t._2)).sum + correction * constraints.getCannotLink.map(t => tmpSimM.pairObjectSimilarity(t._1, t._2)).sum
          println(s" $x -> cl : ${correction * constraints.getCannotLink.map(t => tmpSimM.pairObjectSimilarity(t._1, t._2)).sum} ; ml : ${constraints.getMustLink.map(t => tmpSimM.pairObjectSimilarity(t._1, t._2))}")
          eval
        }

        val myGrad = {
          val gradElems = orderedForGrad.map(sm => {
            -constraints.getMustLink.map(t => sm.pairObjectSimilarity(t._1, t._2)).sum + correction * constraints.getCannotLink.map(t => sm.pairObjectSimilarity(t._1, t._2)).sum
          })
          DenseVector.tabulate(5) { i => gradElems(i) }
        }

        (myVal, myGrad)
      }
    }
  }

  def learn() = {
    val f = getDiffFunction
    val lbfgs = new LBFGS[DenseVector[Double]](maxIter = 1000)

    val res = lbfgs.minimizeAndReturnState(DiffFunction.withL2Regularization(f, 0.5), DenseVector[Double](0.0, 0.0, 0.0, 0.0, 0.0))
    println(res.x, res.convergenceInfo)

    res.x.toScalaVector().toList
  }


}
