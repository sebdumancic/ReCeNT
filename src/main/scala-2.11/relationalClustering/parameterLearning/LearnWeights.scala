package relationalClustering.parameterLearning

import breeze.optimize.linear._
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


  def learn() = {
    val mustLinkSimilarities = constraints.getMustLink.toList.map( ct => orderedForGrad.map( f => f.pairObjectSimilarity(ct._1, ct._2)))
    val cannotLinkSimilarities = constraints.getCannotLink.toList.map( ct => orderedForGrad.map( f => f.pairObjectSimilarity(ct._1, ct._2)))

    val w1_const = mustLinkSimilarities.map(_.head).sum - cannotLinkSimilarities.map(_.head).sum
    val w2_const = mustLinkSimilarities.map(_(1)).sum - cannotLinkSimilarities.map(_(1)).sum
    val w3_const = mustLinkSimilarities.map(_(2)).sum - cannotLinkSimilarities.map(_(2)).sum
    val w4_const = mustLinkSimilarities.map(_(3)).sum - cannotLinkSimilarities.map(_(3)).sum
    val w5_const = mustLinkSimilarities.map(_(4)).sum - cannotLinkSimilarities.map(_(4)).sum

    val lp = new LinearProgram()
    import lp._

    val w1 = Real()
    val w2 = Real()
    val w3 = Real()
    val w4 = Real()
    val w5 = Real()

    val lpp = ( (w1 * w1_const + w2 * w2_const + w3 * w3_const + w4 * w4_const + w5 * w5_const)
                subjectTo( w1 + w2 + w3 + w4 + w5 =:= 1.0)
      )

    val result = maximize(lpp)

    result.result.toScalaVector().toList
  }


}
