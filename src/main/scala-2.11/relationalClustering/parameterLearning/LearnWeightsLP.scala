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
class LearnWeightsLP(protected val constraints: ConstraintsContainer,
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
  val allObjectsInConstraintsWithDomains = (constraints.getMustLink ++ constraints.getCannotLink).foldLeft(List[(String, String)]())((acc, cons) => {
    acc ++ List((cons._1.getRoot.getEntity, cons._1.getRoot.getDomain), (cons._2.getRoot.getEntity, cons._2.getRoot.getDomain))
  })
  val correction = constraints.getMustLink.size.toDouble / constraints.getCannotLink.size.toDouble

  // necessary to get the normalization constants
  firstCompSim.getObjectSimilarity(allObjectsInConstraintsWithDomains.map(_._2), allObjectsInConstraintsWithDomains)
  secondCompSim.getObjectSimilarity(allObjectsInConstraintsWithDomains.map(_._2), allObjectsInConstraintsWithDomains)
  thirdCompSim.getObjectSimilarity(allObjectsInConstraintsWithDomains.map(_._2), allObjectsInConstraintsWithDomains)
  fourthCompSim.getObjectSimilarity(allObjectsInConstraintsWithDomains.map(_._2), allObjectsInConstraintsWithDomains)
  fifthCompSim.getObjectSimilarity(allObjectsInConstraintsWithDomains.map(_._2), allObjectsInConstraintsWithDomains)


  def learn() = {

    val w1_const = constraints.getMustLink.foldLeft(0.0)((acc, c) => acc + firstCompSim.pairObjectSimilarity(c._1, c._2)) - constraints.getCannotLink.foldLeft(0.0)((acc, c) => acc + firstCompSim.pairObjectSimilarity(c._1, c._2))
    val w2_const = constraints.getMustLink.foldLeft(0.0)((acc, c) => acc + secondCompSim.pairObjectSimilarity(c._1, c._2)) - constraints.getCannotLink.foldLeft(0.0)((acc, c) => acc + secondCompSim.pairObjectSimilarity(c._1, c._2))
    val w3_const = constraints.getMustLink.foldLeft(0.0)((acc, c) => acc + thirdCompSim.pairObjectSimilarity(c._1, c._2)) - constraints.getCannotLink.foldLeft(0.0)((acc, c) => acc + thirdCompSim.pairObjectSimilarity(c._1, c._2))
    val w4_const = constraints.getMustLink.foldLeft(0.0)((acc, c) => acc + fourthCompSim.pairObjectSimilarity(c._1, c._2)) - constraints.getCannotLink.foldLeft(0.0)((acc, c) => acc + fourthCompSim.pairObjectSimilarity(c._1, c._2))
    val w5_const = constraints.getMustLink.foldLeft(0.0)((acc, c) => acc + fifthCompSim.pairObjectSimilarity(c._1, c._2)) - constraints.getCannotLink.foldLeft(0.0)((acc, c) => acc + fifthCompSim.pairObjectSimilarity(c._1, c._2))

    val lp = new LinearProgram()
    import lp._

    val w1 = Real()
    val w2 = Real()
    val w3 = Real()
    val w4 = Real()
    val w5 = Real()

    val lpp = ((w1 * w1_const + w2 * w2_const + w3 * w3_const + w4 * w4_const + w5 * w5_const)
      //subjectTo( (w1 + w2 + w3 + w4 + w5) =:= 1.0)
      subjectTo (w1 <= 1.0)
      subjectTo (w1 >= 0.0)
      subjectTo (w2 <= 1.0)
      subjectTo (w2 >= 0.0)
      subjectTo (w3 <= 1.0)
      subjectTo (w3 >= 0.0)
      subjectTo (w4 <= 1.0)
      subjectTo (w4 >= 0.0)
      subjectTo (w5 <= 1.0)
      subjectTo (w5 >= 0.0)
      )

    val result = maximize(lpp)

    val parameterValues = result.result.toScalaVector().toList
    println(s"FOUND parameters: $parameterValues")

    parameterValues.map(e => e / parameterValues.sum).max == 0.0 match {
      case true => List(0.2, 0.2, 0.2, 0.2, 0.2)
      case false => parameterValues.map(e => e / parameterValues.sum).map(e => BigDecimal(e).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
    }
  }


}
