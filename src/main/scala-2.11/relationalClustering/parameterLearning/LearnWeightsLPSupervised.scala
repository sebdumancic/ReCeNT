package relationalClustering.parameterLearning

import breeze.optimize.linear.LinearProgram
import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bags.bagCombination.AbstractBagCombine
import relationalClustering.bags.bagComparison.AbstractBagComparison
import relationalClustering.clustering.evaluation.supervised.LabelsContainer
import relationalClustering.neighbourhood.{NeighbourhoodTree, NodeRepository}
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.similarity.SimilarityNeighbourhoodTrees

/**
  * Created by seb on 16.01.17.
  */
class LearnWeightsLPSupervised(protected val labels: LabelsContainer,
                               protected val knowledgeBase: KnowledgeBase,
                               protected val domain: String,
                               protected val treeDepth: Int,
                               protected val bagComparison: AbstractBagComparison,
                               protected val bagCombination: AbstractBagCombine,
                               protected val aggregates: List[AbstractAggregator]) {

  val firstCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(1.0, 0.0, 0.0, 0.0, 0.0), bagComparison, bagCombination, aggregates)
  val secondCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 1.0, 0.0, 0.0, 0.0), bagComparison, bagCombination, aggregates)
  val thirdCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 0.0, 1.0, 0.0, 0.0), bagComparison, bagCombination, aggregates)
  val fourthCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 0.0, 0.0, 1.0, 0.0), bagComparison, bagCombination, aggregates)
  val fifthCompSim = new SimilarityNeighbourhoodTrees(knowledgeBase, treeDepth, List(0.0, 0.0, 0.0, 0.0, 1.0), bagComparison, bagCombination, aggregates)

  val nodeRepo = new NodeRepository(knowledgeBase)
  val neighbourhoodTrees: Map[String, NeighbourhoodTree] = labels.getAllPairs.map(item => (item._1, new NeighbourhoodTree(item._1, domain, treeDepth, knowledgeBase, nodeRepo)))

  val objectsByClasses: Map[String, List[String]] = labels.getAllPairs.toList.groupBy(_._2).map(item => (item._1, item._2.map(_._1))).filterNot(_._1 == "xxxxxx")


  // necessary to get the normalization constants
  firstCompSim.getObjectSimilarity(List(domain), labels.getAllPairs.toList.map(item => (item._1, domain)))
  secondCompSim.getObjectSimilarity(List(domain), labels.getAllPairs.toList.map(item => (item._1, domain)))
  thirdCompSim.getObjectSimilarity(List(domain), labels.getAllPairs.toList.map(item => (item._1, domain)))
  fourthCompSim.getObjectSimilarity(List(domain), labels.getAllPairs.toList.map(item => (item._1, domain)))
  fifthCompSim.getObjectSimilarity(List(domain), labels.getAllPairs.toList.map(item => (item._1, domain)))

  def learn() = {
    implicit class Crossable[X](xs: Traversable[X]) {
      def cross[Y](ys: Traversable[Y]) = for {x <- xs; y <- ys} yield (x, y)
    }

    //sum(same class)(similarities of elements) - sum(pairs of classes different from each other)(similarities of elements)for each component
    val w1_comp_sameclass = objectsByClasses.keySet.foldLeft(0.0)((acc, cl) => {
      acc + (1.0 / (objectsByClasses(cl).length * objectsByClasses(cl).length) / 2) * objectsByClasses(cl).combinations(2).foldLeft(0.0)((acc_i, pair) => acc_i + firstCompSim.pairObjectSimilarity(neighbourhoodTrees(pair.head), neighbourhoodTrees(pair(1))))
    })
    val w2_comp_sameclass = objectsByClasses.keySet.foldLeft(0.0)((acc, cl) => {
      acc + (1.0 / (objectsByClasses(cl).length * objectsByClasses(cl).length) / 2) * objectsByClasses(cl).combinations(2).foldLeft(0.0)((acc_i, pair) => acc_i + secondCompSim.pairObjectSimilarity(neighbourhoodTrees(pair.head), neighbourhoodTrees(pair(1))))
    })
    val w3_comp_sameclass = objectsByClasses.keySet.foldLeft(0.0)((acc, cl) => {
      acc + (1.0 / (objectsByClasses(cl).length * objectsByClasses(cl).length) / 2) * objectsByClasses(cl).combinations(2).foldLeft(0.0)((acc_i, pair) => acc_i + thirdCompSim.pairObjectSimilarity(neighbourhoodTrees(pair.head), neighbourhoodTrees(pair(1))))
    })
    val w4_comp_sameclass = objectsByClasses.keySet.foldLeft(0.0)((acc, cl) => {
      acc + (1.0 / (objectsByClasses(cl).length * objectsByClasses(cl).length) / 2) * objectsByClasses(cl).combinations(2).foldLeft(0.0)((acc_i, pair) => acc_i + fourthCompSim.pairObjectSimilarity(neighbourhoodTrees(pair.head), neighbourhoodTrees(pair(1))))
    })
    val w5_comp_sameclass = objectsByClasses.keySet.foldLeft(0.0)((acc, cl) => {
      acc + (1.0 / (objectsByClasses(cl).length * objectsByClasses(cl).length) / 2) * objectsByClasses(cl).combinations(2).foldLeft(0.0)((acc_i, pair) => acc_i + fifthCompSim.pairObjectSimilarity(neighbourhoodTrees(pair.head), neighbourhoodTrees(pair(1))))
    })

    val w1_comp_diffclass = objectsByClasses.keys.toList.combinations(2).foldLeft(0.0)((acc, clComb) => {
      acc + (1.0 / ((objectsByClasses(clComb.head).length * objectsByClasses(clComb(1)).length) / 2)) * (objectsByClasses(clComb.head) cross objectsByClasses(clComb(1))).foldLeft(0.0)((acc_i, p) => acc_i + firstCompSim.pairObjectSimilarity(neighbourhoodTrees(p._1), neighbourhoodTrees(p._2)))
    })
    val w2_comp_diffclass = objectsByClasses.keys.toList.combinations(2).foldLeft(0.0)((acc, clComb) => {
      acc + (1.0 / ((objectsByClasses(clComb.head).length * objectsByClasses(clComb(1)).length) / 2)) * (objectsByClasses(clComb.head) cross objectsByClasses(clComb(1))).foldLeft(0.0)((acc_i, p) => acc_i + secondCompSim.pairObjectSimilarity(neighbourhoodTrees(p._1), neighbourhoodTrees(p._2)))
    })
    val w3_comp_diffclass = objectsByClasses.keys.toList.combinations(2).foldLeft(0.0)((acc, clComb) => {
      acc + (1.0 / ((objectsByClasses(clComb.head).length * objectsByClasses(clComb(1)).length) / 2)) * (objectsByClasses(clComb.head) cross objectsByClasses(clComb(1))).foldLeft(0.0)((acc_i, p) => acc_i + thirdCompSim.pairObjectSimilarity(neighbourhoodTrees(p._1), neighbourhoodTrees(p._2)))
    })
    val w4_comp_diffclass = objectsByClasses.keys.toList.combinations(2).foldLeft(0.0)((acc, clComb) => {
      acc + (1.0 / ((objectsByClasses(clComb.head).length * objectsByClasses(clComb(1)).length) / 2)) * (objectsByClasses(clComb.head) cross objectsByClasses(clComb(1))).foldLeft(0.0)((acc_i, p) => acc_i + fourthCompSim.pairObjectSimilarity(neighbourhoodTrees(p._1), neighbourhoodTrees(p._2)))
    })
    val w5_comp_diffclass = objectsByClasses.keys.toList.combinations(2).foldLeft(0.0)((acc, clComb) => {
      acc + (1.0 / ((objectsByClasses(clComb.head).length * objectsByClasses(clComb(1)).length) / 2)) * (objectsByClasses(clComb.head) cross objectsByClasses(clComb(1))).foldLeft(0.0)((acc_i, p) => acc_i + fifthCompSim.pairObjectSimilarity(neighbourhoodTrees(p._1), neighbourhoodTrees(p._2)))
    })

    val correction = labels.getDistinctLabels.length.toDouble / (labels.getDistinctLabels.length.toDouble * labels.getDistinctLabels.length.toDouble / 2)

    val lp = new LinearProgram()
    import lp._

    val w1 = Real()
    val w2 = Real()
    val w3 = Real()
    val w4 = Real()
    val w5 = Real()

    println(s"Correction: $correction")
    println(s"Coefficients individually: ($w1_comp_sameclass $w1_comp_diffclass), ($w2_comp_sameclass $w2_comp_diffclass), ($w3_comp_sameclass $w3_comp_diffclass), ($w4_comp_sameclass $w4_comp_diffclass), ($w5_comp_sameclass $w5_comp_diffclass) ")
    println(s"Coefficients: ${w1_comp_sameclass - correction * w1_comp_diffclass}, ${w2_comp_sameclass - correction * w2_comp_diffclass}, ${w3_comp_sameclass - correction * w3_comp_diffclass}, ${w4_comp_sameclass - correction * w4_comp_diffclass}, ${w5_comp_sameclass - correction * w5_comp_diffclass}")


    val lpp = ((w1 * (w1_comp_sameclass - correction * w1_comp_diffclass) + w2 * (w2_comp_sameclass - correction * w2_comp_diffclass) + w3 * (w3_comp_sameclass - correction * w3_comp_diffclass) + w4 * (w4_comp_sameclass - correction * w4_comp_diffclass) + w5 * (w5_comp_sameclass - correction * w5_comp_diffclass))
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
      case false => parameterValues.map(e => e / parameterValues.sum)
    }
  }

}
