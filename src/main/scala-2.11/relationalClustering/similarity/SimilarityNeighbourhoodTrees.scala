package relationalClustering.similarity

import breeze.linalg.DenseMatrix
import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bags.bagCombination.AbstractBagCombine
import relationalClustering.bags.bagComparison.AbstractBagComparison
import relationalClustering.neighbourhood.NeighbourhoodTree
import relationalClustering.representation.domain.{KnowledgeBase, NumericDomain}
import relationalClustering.utils.Settings

/**
  * Implements the similarity measure using neighbourhood trees
  *
  * @constructor constructs a measure object with provided knowledge base, depth, weights and bag comparison method
  * @param knowledgeBase knowledge base: [[KnowledgeBase]]
  * @param depth depth of the neighbourhood graphs: [[Int]]
  * @param weights a list of weights (attributes, attribute neighbourhood, connections, vertex neighbourhoods, edge distribution), must sum to 1.0: [[List]]
  * @param bagCompare bag comparison class: [[AbstractBagComparison]]
  * Created by seb on 03.02.16.
  */
class SimilarityNeighbourhoodTrees(override protected val knowledgeBase: KnowledgeBase,
                                   override protected val depth: Int,
                                   protected val weights: List[Double],
                                   protected val bagCompare: AbstractBagComparison,
                                   protected val bagCombine: AbstractBagCombine,
                                   protected val aggregators: List[AbstractAggregator],
                                   override protected val useLocalRepo: Boolean = false) extends AbstractSimilarityNTrees(knowledgeBase, depth, useLocalRepo) {

  require(weights.length == 5, s" You should provide 5 weights not ${weights.length} ($weights)")

  require(BigDecimal(weights.sum).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble == 1.0, s"Weights should sum to one, not ${BigDecimal(weights.sum).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble}")

  /** An auxiliary constructor that takes an existing neighbourhood graph cache */
  def this(KB: KnowledgeBase, d: Int, ws: List[Double], bagComp: AbstractBagComparison, bagComb: AbstractBagCombine, aggs: List[AbstractAggregator], cache: Map[(String, String), NeighbourhoodTree]) {
    this(KB, d, ws, bagComp, bagComb, aggs, false)
    setNeighbourhoodGraphs(cache)
  }

  override def getParameters: List[Double] = {
    weights
  }

  /** Returns a new object together with normalization constants and neighbourhood trees*/
  def copy: SimilarityNeighbourhoodTrees = {
    val newObj = new SimilarityNeighbourhoodTrees(knowledgeBase, depth, weights, bagCompare, bagCombine, aggregators)
    newObj.setObjectNorms(getObjectNorm.map(x => x))
    newObj.setHyperedgeNorms(getHyperEdgeNorm.map( x => x))
    newObj.assignNTs(getNeighbourhoodGraphCache)
    newObj
  }



  /***********************
    *
    *    OBJECT SIMILARITY
    * * * * * * */

  /** Method implementing an interface for accessing similarity of object(s) from specified domain(s)
    *
    * @param domains list of domains to cluster objects from: [[List]]
    * @return (ordering of objects, similarity matrix for corresponding element)
    * */
  def getObjectSimilarity(domains: List[String], objectsToUse: List[(String, String)] = null): (List[String], DenseMatrix[Double]) = {
    val objects = if (objectsToUse == null) {
      getObjectsFromDomains(domains)
    }
    else {
      objectsToUse
    }

    val functionsWithNorm = List(false, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(NeighbourhoodTree, NeighbourhoodTree) => Double](attributeSimilarity, attributeNeighbourhoodSimilarity, elementConnections, vertexIdentityDistribution, edgeDistributionsSimilarity)
    )

    val returnMat = weights.zipWithIndex.filter( _._1 > 0.0).foldLeft(DenseMatrix.zeros[Double](objects.length, objects.length))( (acc, w) => {
      acc + (DenseMatrix.tabulate(objects.length, objects.length){(x,y) => w._1} :* accumulateIntoMatrix(objects, functionsWithNorm(w._2)._2, functionsWithNorm(w._2)._1, w._2))
    })

    (objects.map(_._1), returnMat)
  }

  /** Calculates similarity between two individual neighbourhood trees (normalizing constants have to be calculated before!!!)
    *
    * @param nt1 the first neighbourhood tree
    * @param nt2 the second neighbourhood tree
    * @return similarity
    * */
  def pairObjectSimilarity(nt1: NeighbourhoodTree, nt2: NeighbourhoodTree): Double = {
    if (objectsNormConstants.isEmpty) {
      objectsNormConstants = readNormsFromFile(getVertexNormsFilename(List(nt1.getRoot.getDomain)))
    }
    require(objectsNormConstants.nonEmpty, s"SimilarityNeighbourhoodTrees::pairObjectSimilarity : no normalization constants provided!")

    require(objectsNormConstants.nonEmpty, s"SimilarityNeighbourhoodTrees::pairObjectSimilarity : no normalization constants provided!")

    val functionsWithNorm = List(false, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(NeighbourhoodTree, NeighbourhoodTree) => Double](attributeSimilarity, attributeNeighbourhoodSimilarity, elementConnections, vertexIdentityDistribution, edgeDistributionsSimilarity)
    )

    weights.zipWithIndex.filter( _._1 > 0.0).foldLeft(0.0)( (acc, w) => {
      val norm = if (objectsNormConstants(w._2) == 0.0) {
        1.0
      }
      else {
        objectsNormConstants(w._2)
      }

      val calc = w._1 * (if (functionsWithNorm(w._2)._1) {
        1.0 - (functionsWithNorm(w._2)._2(nt1, nt2) / norm)
      } else {
        functionsWithNorm(w._2)._2(nt1, nt2) / norm
      })

      if (calc < 0) {
        println(s"Similarity <0 for ${nt1.getRoot.getEntity} and ${nt2.getRoot.getEntity} with ${w._2} (${w._1}): $calc")
      }
      acc + calc
    })
  }

  /** Compute the attribute-value pair similarity between two root elements
    *
    * @param ng1 neighbourhood graph of the first element: [[NeighbourhoodTree]]
    * @param ng2 neighbourhood graph of the second element: [[NeighbourhoodTree]]
    * @return Double
    * */
  protected def attributeSimilarity(ng1: NeighbourhoodTree, ng2: NeighbourhoodTree): Double = {
    val attrs1 = ng1.getRootAttributes
    val attrs2 = ng2.getRootAttributes

    val numAttrs1 = ng1.getRootNumericAttributes.toMap
    val numAttrs2 = ng2.getRootNumericAttributes.toMap

    // discrete attributes                   // numerical attributes
    attrs1.intersect(attrs2).size.toDouble + (numAttrs1.keySet ++ numAttrs2.keySet).toList.foldLeft(0.0)((acc, key) => {
      acc + (numAttrs1.contains(key) && numAttrs2.contains(key) match {
        case false => 0.0
        case true =>
          val domain = getKB.getPredicate(key).getArgumentRoles.zip(getKB.getPredicate(key).getDomainObjects).filter(_._1 == Settings.ARG_TYPE_NUMBER)
          require(domain.length == 1, s"SimilarityNeighbourhoodTrees::attributeSimilarity : predicate $key has more than one number domain!")
          1.0 - (math.abs(numAttrs1(key) - numAttrs2(key)) / domain.head._2.asInstanceOf[NumericDomain].getRange)
      })
    })
  }

  /** Computes the attribute neighbourhood similarity of two neighbourhood graphs, per level and vertex type
    *
    * @param ng1 the first elements's neighbourhood graph: [[NeighbourhoodTree]]
    * @param ng2 the second elements's neighbourhood graph: [[NeighbourhoodTree]]
    * @return [[Double]]
    * */
  protected def attributeNeighbourhoodSimilarity(ng1: NeighbourhoodTree, ng2: NeighbourhoodTree): Double = {

    val discreteValue = (1 to getDepth).foldLeft(0.0)((acc, depth) => {
      acc + (ng1.getVertexTypesAtLevel(depth) ++ ng2.getVertexTypesAtLevel(depth)).foldLeft(0.0)((acc_i, vtype) => {
        acc_i + (ng1.getDiscreteAttributeNames(depth, vtype) ++ ng2.getDiscreteAttributeNames(depth, vtype)).foldLeft(0.0)((acc_ii, attr) => {
          acc_ii + math.abs(bagCompare.compareBags(ng1.getBDiscrete(depth, vtype, attr), ng2.getBDiscrete(depth, vtype, attr)))
        })
      })
    })

    val annotationValue = (1 to getDepth).foldLeft(0.0)((acc, depth) => {
      acc + (ng1.getVertexTypesAtLevel(depth) ++ ng2.getVertexTypesAtLevel(depth)).foldLeft(0.0)((acc_i, vtype) => {
        acc_i + math.abs(bagCompare.compareBags(ng1.getBAnnotations(depth, vtype), ng2.getBAnnotations(depth, vtype)))
      })
    })

    val numericValue = if (!getKB.hasNumericAttributes) {
      0.0
    } else {
      aggregators.foldLeft(0.0)((acc, agg) => {
        acc + (1 to getDepth).foldLeft(0.0)((acc_i, depth) => {
          acc_i + (ng1.getVertexTypesAtLevel(depth) ++ ng2.getVertexTypesAtLevel(depth)).foldLeft(0.0)((acc_ii, vtype) => {
            acc_ii + (ng1.getContinuousAttributeNames(depth, vtype) ++ ng2.getContinuousAttributeNames(depth, vtype)).foldLeft(0.0)((acc_iii, attr) => {
              val denominator = getKB.getPredicate(attr).getArgumentRoles.zip(getKB.getPredicate(attr).getDomainObjects).filter(_._1 == Settings.ARG_TYPE_NUMBER)
              require(denominator.length == 1, s"SimilarityNeighbourhoodTrees::attributeSimilarity : predicate $attr has more than one number domain!")
              val ng1Agg = ng1.getBContinuous(depth, vtype, attr, agg)
              val ng2Agg = ng2.getBContinuous(depth, vtype, attr, agg)
              val value = if (ng1Agg.isDefined && ng2Agg.isDefined) {
                math.abs(ng1Agg.get - ng2Agg.get) / denominator.head._2.asInstanceOf[NumericDomain].getRange
              }
              else {
                1.0
              }
              if (bagCompare.needsToBeInverted) {
                //needs to be a distance value
                acc_iii + value
              }
              else {
                // value represents a similarity
                acc_iii + (1.0 - value)
              }
            })
          })
        })
      })
    }


    discreteValue + annotationValue + numericValue
  }

  /** Calculates the number of connections between two root elements
    *
    * @param ng1 the first element's [[NeighbourhoodTree]]
    * @param ng2 the second element's [[NeighbourhoodTree]]
    * @return [[Double]]
    * */
  protected def elementConnections(ng1: NeighbourhoodTree, ng2: NeighbourhoodTree): Double = {
    ng2.getV(1, ng1.getRootDomain).getOrElse(ng1.getRoot.getEntity, 0).toDouble
  }

  /** Computes the vertex identity distributions similarity for two neighbourhood graphs, per level and vertex type
    *
    * @param ng1 the first element's [[NeighbourhoodTree]]
    * @param ng2 the second element's [[NeighbourhoodTree]]
    * @return [[Double]]
    * */
  protected def vertexIdentityDistribution(ng1: NeighbourhoodTree, ng2: NeighbourhoodTree): Double = {
    (1 to getDepth).foldLeft(0.0)((acc, depth) => {
      acc + (ng1.getVertexTypesAtLevel(depth) ++ ng2.getVertexTypesAtLevel(depth)).foldLeft(0.0)((acc_i, vtype) => {
        acc_i + math.abs(bagCompare.compareBags(ng1.getV(depth, vtype), ng2.getV(depth, vtype)))
      })
    })
  }

  /** Calculates the edge distribution similarity between two neighbourhood graphs, per level
    *
    * @param ng1 the first element's [[NeighbourhoodTree]]
    * @param ng2 the second element's [[NeighbourhoodTree]]
    * @return [[Double]]
    * */
  protected def edgeDistributionsSimilarity(ng1: NeighbourhoodTree, ng2: NeighbourhoodTree): Double = {
    (0 until getDepth).foldLeft(0.0)((acc, depth) => {
      acc + math.abs(bagCompare.compareBags(ng1.getE(depth), ng2.getE(depth)))
    })
  }

  /** Accumulates the similarity measure of provided elements in a matrix
    *
    * @param elements list of element-domain tuples: [[List]]
    * @param simFunc similarity functions that takes two neighbourhood trees as input and return a Double
    * @param shouldBeInverted flag indicating should the computed matrix be inverted in order to be a similarity measure: [[Boolean]]
    * */
  protected def accumulateIntoMatrix(elements: List[(String, String)], simFunc: (NeighbourhoodTree, NeighbourhoodTree) => Double, shouldBeInverted: Boolean, constInd: Int): DenseMatrix[Double] = {
    val similarityMatrix = DenseMatrix.zeros[Double](elements.length, elements.length)

    for(ind1 <- elements.indices; ind2 <- (ind1 + 1) until elements.length) {
      val simValue = simFunc(getNeighbourhoodGraph(elements(ind1)._1, elements(ind1)._2), getNeighbourhoodGraph(elements(ind2)._1, elements(ind2)._2))

      similarityMatrix(ind1, ind2) += simValue
      similarityMatrix(ind2, ind1) += simValue
    }

    if (shouldBeInverted) {
      normalizeAndInvert(similarityMatrix, constInd, "v")
    } else {
      normalizeMatrix(similarityMatrix, constInd, "v")
    }
  }




  /*********************************
    *
    *   HYPEREDGE SIMILARITY
    *
    */

  /** Method implementing an interface to assess the similarity of edges connecting objects from specified domains
    *
    * @param domains list of domains that hyperedges connect: [[List]]
    * @return (ordering of hyperEdges, similarity matrix)
    * */
  def getHyperEdgeSimilarity(domains: List[String]): (List[List[String]], DenseMatrix[Double]) = {
    val hyperEdges = getHyperEdges(domains)

    val functionsWithNorm = List(false, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(List[NeighbourhoodTree], List[NeighbourhoodTree]) => Double](hyperedgeAttributeSimilarity, hyperEdgeAttributeNeighbourhoodSimilarity, hyperEdgeConnections, hyperEdgeVertexDistribution, hyperEdgeEdgeDistribution)
    )

    val returnMat = weights.zipWithIndex.filter( _._1 > 0.0).foldLeft(DenseMatrix.zeros[Double](hyperEdges.length, hyperEdges.length))( (acc, weight) => {
      acc + (accumulateIntoMatrixHyperEdge(hyperEdges, domains, functionsWithNorm(weight._2)._2, functionsWithNorm(weight._2)._1, weight._2) :* DenseMatrix.tabulate(hyperEdges.length, hyperEdges.length){ (x,y) => weight._1})
    })

    (hyperEdges, returnMat)
  }


  /** Calculates similarity between two individual hyperedges (normalizing constants have to be calculated before!!!)
    *
    * @param nt1 an ordered set of neighbourhood trees
    * @param nt2 an ordered set of neighbourhood trees
    *
    * */
  def getPairHyperEdgeSimilarity(nt1: List[NeighbourhoodTree], nt2: List[NeighbourhoodTree]): Double = {
    val functionsWithNorm = List(false, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(List[NeighbourhoodTree], List[NeighbourhoodTree]) => Double](hyperedgeAttributeSimilarity, hyperEdgeAttributeNeighbourhoodSimilarity, hyperEdgeConnections, hyperEdgeVertexDistribution, hyperEdgeEdgeDistribution)
    )

    if (hyperEdgeNormConstants.isEmpty) {
      hyperEdgeNormConstants = readNormsFromFile(getEdgeNormsFilename(nt1.map(_.getRoot.getDomain)))
    }

    weights.zipWithIndex.filter( _._1 > 0.0).foldLeft(0.0)( (acc, w) => {
      val norm = if (hyperEdgeNormConstants(w._2) == 0.0) {
        1.0
      } else {
        hyperEdgeNormConstants(w._2)
      }

      acc + w._1 * (if (functionsWithNorm(w._2)._1) {
        1.0 - (functionsWithNorm(w._2)._2(nt1, nt2) / norm)
      } else {
        functionsWithNorm(w._2)._2(nt1, nt2) / norm
      })
    })

  }

  /** computes the similarity between hyperEdge in vertices according to their attributes
    *
    * @param ngs1 the first hyperedge (ordered set of vertices)
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperedgeAttributeSimilarity(ngs1: List[NeighbourhoodTree], ngs2: List[NeighbourhoodTree]): Double = {
    val discreteAttributeSet = ngs1.flatMap(nt => nt.getDiscreteAttributeNames(0, nt.getRootDomain)) ++ ngs2.flatMap(nt => nt.getDiscreteAttributeNames(0, nt.getRootDomain))
    val discreteValue = discreteAttributeSet.foldLeft(0.0)((acc, attr) => {
      val he1 = ngs1.map(nt => nt.getBDiscrete(0, nt.getRootDomain, attr)).reduceLeft(bagCombine.combineBags)
      val he2 = ngs2.map(nt => nt.getBDiscrete(0, nt.getRootDomain, attr)).reduceLeft(bagCombine.combineBags)
      acc + math.abs(bagCompare.compareBags(he1, he2))
    })

    val h1 = ngs1.flatMap(nt => nt.getRootAttributes.toList)
    val h2 = ngs2.flatMap(nt => nt.getRootAttributes.toList)
    val discVal = h1.intersect(h2).size.toDouble

    val continuousAttributeSet = ngs1.flatMap(nt => nt.getContinuousAttributeNames(0, nt.getRootDomain)) ++ ngs2.flatMap(nt => nt.getContinuousAttributeNames(0, nt.getRootDomain))
    val numericValue = continuousAttributeSet.foldLeft(0.0)((acc, attr) => {
      val he1 = ngs1.map(nt => nt.getBContinuous(0, nt.getRootDomain, attr)).filter(_.isDefined).map(_.get).reduce(_ ++ _)
      val he2 = ngs2.map(nt => nt.getBContinuous(0, nt.getRootDomain, attr)).filter(_.isDefined).map(_.get).reduce(_ ++ _)

      acc + aggregators.foldLeft(0.0)((acc_i, agg) => {
        val denominator = getKB.getPredicate(attr).getArgumentRoles.zip(getKB.getPredicate(attr).getDomainObjects).filter(_._1 == Settings.ARG_TYPE_NUMBER)
        require(denominator.length == 1, s"SimilarityNeighbourhoodTrees::attributeSimilarity : predicate $attr has more than one number domain!")

        val value = if (he1.nonEmpty && he2.nonEmpty) {
          math.abs(agg.aggregate(he1) - agg.aggregate(he2)) / denominator.head._2.asInstanceOf[NumericDomain].getRange
        }
        else {
          1.0
        }

        if (bagCompare.needsToBeInverted) {
          //needs to be a distance value
          acc_i + value
        }
        else {
          // value represents a similarity
          acc_i + (1.0 - value)
        }
      })
    })

    discVal + numericValue
  }

  /** Computes the similarity between hyperEdges according to the attribute neighbourhoods of the vertices it connects
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeAttributeNeighbourhoodSimilarity(ngs1: List[NeighbourhoodTree], ngs2: List[NeighbourhoodTree]): Double = {
    val discreteValue = (1 to getDepth).foldLeft(0.0)((acc, depth) => {
      val domains = ngs1.flatMap(nt => nt.getVertexTypesAtLevel(depth)) ++ ngs2.flatMap(nt => nt.getVertexTypesAtLevel(depth))

      acc + domains.foldLeft(0.0)((acc_i, vtype) => {
        val attributes = ngs1.flatMap(nt => nt.getDiscreteAttributeNames(depth, vtype)) ++ ngs2.flatMap(nt => nt.getDiscreteAttributeNames(depth, vtype))

        acc_i + attributes.foldLeft(0.0)((acc_ii, attr) => {
          val he1 = ngs1.map(nt => nt.getBDiscrete(0, nt.getRootDomain, attr)).reduceLeft(bagCombine.combineBags)
          val he2 = ngs2.map(nt => nt.getBDiscrete(0, nt.getRootDomain, attr)).reduceLeft(bagCombine.combineBags)
          acc_ii + math.abs(bagCompare.compareBags(he1, he2))
        })
      })
    })

    val annotationValue = (1 to getDepth).foldLeft(0.0)((acc, depth) => {
      val domains = ngs1.flatMap(nt => nt.getVertexTypesAtLevel(depth)) ++ ngs2.flatMap(nt => nt.getVertexTypesAtLevel(depth))
      acc + domains.foldLeft(0.0)((acc_i, vtype) => {
        val he1 = ngs1.map(nt => nt.getBAnnotations(0, nt.getRootDomain)).reduceLeft(bagCombine.combineBags)
        val he2 = ngs2.map(nt => nt.getBAnnotations(0, nt.getRootDomain)).reduceLeft(bagCombine.combineBags)
        acc_i + math.abs(bagCompare.compareBags(he1, he2))
      })
    })

    val numericValue = if (!getKB.hasNumericAttributes) {
      0.0
    } else {
      (1 to getDepth).foldLeft(0.0)((acc, depth) => {
        val domains = ngs1.flatMap(nt => nt.getVertexTypesAtLevel(depth)) ++ ngs2.flatMap(nt => nt.getVertexTypesAtLevel(depth))

        acc + domains.foldLeft(0.0)((acc_i, vtype) => {
          val attributes = ngs1.flatMap(nt => nt.getContinuousAttributeNames(depth, vtype)) ++ ngs2.flatMap(nt => nt.getContinuousAttributeNames(depth, vtype))
          acc_i + attributes.foldLeft(0.0)((acc_ii, attr) => {
            val he1 = ngs1.map(nt => nt.getBContinuous(0, nt.getRootDomain, attr)).filter(_.isDefined).map(_.get).reduceLeft(_ ++ _)
            val he2 = ngs2.map(nt => nt.getBContinuous(0, nt.getRootDomain, attr)).filter(_.isDefined).map(_.get).reduceLeft(_ ++ _)

            acc_ii + aggregators.foldLeft(0.0)((acc_iii, agg) => {
              val denominator = getKB.getPredicate(attr).getArgumentRoles.zip(getKB.getPredicate(attr).getDomainObjects).filter(_._1 == Settings.ARG_TYPE_NUMBER)
              require(denominator.length == 1, s"SimilarityNeighbourhoodTrees::attributeSimilarity : predicate $attr has more than one number domain!")

              val value = if (he1.nonEmpty && he2.nonEmpty) {
                math.abs(agg.aggregate(he1) - agg.aggregate(he2)) / denominator.head._2.asInstanceOf[NumericDomain].getRange
              }
              else {
                1.0
              }

              if (bagCompare.needsToBeInverted) {
                //needs to be a distance value
                acc_iii + value
              }
              else {
                // value represents a similarity
                acc_iii + (1.0 - value)
              }
            })
          })
        })
      })
    }

    discreteValue + annotationValue + numericValue
  }

  /** Calculates the number of connections between vertices in two hyperedges
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeConnections(ngs1: List[NeighbourhoodTree], ngs2: List[NeighbourhoodTree]): Double = {
    ngs1.foldLeft(0.0)((acc, nt) => {
      val combined = ngs2.map(_.getV(1, nt.getRootDomain)).reduceLeft(bagCombine.combineBags)
      acc + combined.getOrElse(nt.getRoot.getEntity, 0).toDouble
    })
  }

  /** Calculates the similarity in vertex neighbourhoods of the vertices in hyperedges
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeVertexDistribution(ngs1: List[NeighbourhoodTree], ngs2: List[NeighbourhoodTree]): Double = {
    (1 to getDepth).foldLeft(0.0)((acc, depth) => {
      val domains = ngs1.flatMap(nt => nt.getVertexTypesAtLevel(depth)) ++ ngs2.flatMap(nt => nt.getVertexTypesAtLevel(depth))
      acc + domains.foldLeft(0.0)((acc_i, vtype) => {
        val he1 = ngs1.map(_.getV(depth, vtype)).reduceLeft(bagCombine.combineBags)
        val he2 = ngs2.map(_.getV(depth, vtype)).reduceLeft(bagCombine.combineBags)
        acc_i + math.abs(bagCompare.compareBags(he1, he2))
      })
    })
  }

  /** Calculates the edge distribution similarity between vertices in a hyperedge
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeEdgeDistribution(ngs1: List[NeighbourhoodTree], ngs2: List[NeighbourhoodTree]): Double = {
    (0 until getDepth).foldLeft(0.0)((acc, depth) => {
      val he1 = ngs1.map(_.getE(depth)).reduceLeft(bagCombine.combineBags)
      val he2 = ngs2.map(_.getE(depth)).reduceLeft(bagCombine.combineBags)
      acc + math.abs(bagCompare.compareBags(he1, he2))
    })
  }

  /** Accumulates the similarity measure of provided elements in a matrix
    *
    * @param elements a list of hyperedges
    * @param domains domains of elements in a hyperedge
    * @param simFunction similarity measure
    * */
  protected def accumulateIntoMatrixHyperEdge(elements: List[List[String]], domains: List[String], simFunction: (List[NeighbourhoodTree], List[NeighbourhoodTree]) => Double, shouldBeInverted: Boolean, constId: Int): DenseMatrix[Double] = {
    val similarityMatrix = DenseMatrix.zeros[Double](elements.length, elements.length)

    for(ind1 <- elements.indices; ind2 <- (ind1 + 1) until elements.length) {
      val simValue = simFunction(elements(ind1).zipWithIndex.map(x => getNeighbourhoodGraph(x._1, domains(x._2))),
                                 elements(ind2).zipWithIndex.map( x => getNeighbourhoodGraph(x._1, domains(x._2))))

      similarityMatrix(ind1, ind2) += simValue
      similarityMatrix(ind2, ind1) += simValue
    }

    if (shouldBeInverted) {
      normalizeAndInvert(similarityMatrix, constId, "h")
    } else {
      normalizeMatrix(similarityMatrix, constId, "h")
    }
  }

  /** FILE NAME GETTERS FOR SAVING SIMILARITY MATRICES */

  /** Uniquely identifies the filename to save similarity matrix (once calculated it can be reused)
    *
    * @param domains list of domains of interest
    **/
  override def getFilename(domains: List[String]): String = {
    s"${domains.mkString("")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_localRepo$useLocal.txt"
  }

  override def getVertexNormsFilename(domains: List[String]): String = {
    s"${domains.mkString("")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_localRepo$useLocal.vnorms"
  }

  override def getEdgeNormsFilename(domains: List[String]): String = {
    s"${domains.mkString("")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_localRepo$useLocal.hnorms"
  }

  override def getFilenameHyperEdges(domains: List[String]): String = {
    s"${domains.mkString("")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_combination${bagCombine.getName}_localRepo$useLocal.txt"
  }
}
