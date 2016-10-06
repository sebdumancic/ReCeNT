package relationalClustering.similarity

import breeze.linalg.DenseMatrix
import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.AbstractBagCombine
import relationalClustering.neighbourhood.NeighbourhoodGraph
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
  def this(KB: KnowledgeBase, d: Int, ws: List[Double], bagComp: AbstractBagComparison, bagComb: AbstractBagCombine, aggs: List[AbstractAggregator], cache: Map[(String,String), NeighbourhoodGraph]) {
    this(KB, d, ws, bagComp, bagComb, aggs, false)
    setNeighbourhoodGraphs(cache)
  }

  /** Returns a new object together with normalization constants and neighbourhood trees*/
  def copy = {
    val newObj = new SimilarityNeighbourhoodTrees(knowledgeBase, depth, weights, bagCompare, bagCombine, aggregators)
    newObj.setObjectNorms(getObjectNorm.map(x => x))
    newObj.setHyperedgeNorms(getHyperEdgeNorm.map( x => x))
    newObj.assignNTs(getNeighbourhoodGraphCache)
    newObj
  }

  /** Uniquely identifies the filename to save similarity matrix (once calculated it can be reused)
    *
    * @param domains list of domains of interest
    * */
  def getFilename(domains: List[String]) = {
    s"${domains.mkString(",")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_localRepo$useLocal.txt"
  }

  def getFilenameHyperEdges(domains: List[String]) = {
    s"${domains.mkString("")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_combination${bagCombine.getName}_localRepo$useLocal.txt"
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
  def getObjectSimilarity(domains: List[String], objectsToUse: List[(String, String)] = null) = {
    val objects = objectsToUse == null match {
      case true => getObjectsFromDomains(domains)
      case false => objectsToUse
    }

    val functionsWithNorm = List(false, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(NeighbourhoodGraph, NeighbourhoodGraph) => Double](attributeSimilarity, attributeNeighbourhoodSimilarity, elementConnections, vertexIdentityDistribution, edgeDistributionsSimilarity)
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
  def pairObjectSimilarity(nt1: NeighbourhoodGraph, nt2: NeighbourhoodGraph) = {
    require(objectsNormConstants.nonEmpty, s"SimilarityNeighbourhoodTrees::pairObjectSimilarity : no normalization constants provided!")

    val functionsWithNorm = List(false, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(NeighbourhoodGraph, NeighbourhoodGraph) => Double](attributeSimilarity, attributeNeighbourhoodSimilarity, elementConnections, vertexIdentityDistribution, edgeDistributionsSimilarity)
    )

    weights.zipWithIndex.filter( _._1 > 0.0).foldLeft(0.0)( (acc, w) => {
      val norm = objectsNormConstants(w._2) == 0.0 match {
            case true => 1.0
            case false => objectsNormConstants(w._2)
          }

      val calc = w._1 * (functionsWithNorm(w._2)._1 match {
        case true => 1.0 - (functionsWithNorm(w._2)._2(nt1, nt2)/norm)
        case false => functionsWithNorm(w._2)._2(nt1, nt2)/norm
      })
      if (calc < 0) {
        println(s"Similarity <0 for ${nt1.getRoot.getEntity} and ${nt2.getRoot.getEntity} with ${w._2} (${w._1}): $calc")
      }
      acc + calc
    })
  }

  /** Compute the attribute-value pair similarity between two root elements
    *
    * @param ng1 neighbourhood graph of the first element: [[NeighbourhoodGraph]]
    * @param ng2 neighbourhood graph of the second element: [[NeighbourhoodGraph]]
    * @return Double
    * */
  protected def attributeSimilarity(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
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
    * @param ng1 the first elements's neighbourhood graph: [[NeighbourhoodGraph]]
    * @param ng2 the second elements's neighbourhood graph: [[NeighbourhoodGraph]]
    * @return [[Double]]
    * */
  protected def attributeNeighbourhoodSimilarity(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val firstAttrs = ng1.getAttributeValueDistribution
    val secondAttrs = ng2.getAttributeValueDistribution

    // aggregate numeric attributes
    val numericalValue = getKB.hasNumericAttributes match {
      case false => 0.0
      case true =>
        aggregators.foldLeft(0.0)((acc, agg) => {
          val numFirstAttrs = ng1.aggregateNumericAttributes(agg)
          val numSecondAttrs = ng2.aggregateNumericAttributes(agg)
          val aggDepth = numFirstAttrs.isEmpty && numSecondAttrs.isEmpty match {
            case true => -1
            case false =>
              val firstKeys = if (numFirstAttrs.keys.isEmpty) -1 else numFirstAttrs.keys.max
              val secondKeys = if (numSecondAttrs.keys.isEmpty) -1 else numSecondAttrs.keys.max
              math.max(firstKeys, secondKeys)
          }
          acc + (0 to aggDepth).foldLeft(0.0)((acc_i, depth) => {
            acc_i + (numFirstAttrs.getOrElse(depth, Map[String, List[(String, Double)]]()).keySet ++ numSecondAttrs.getOrElse(depth, Map[String, List[(String, Double)]]()).keySet).foldLeft(0.0)((acc_ii, vType) => {
              val aggs1 = numFirstAttrs.getOrElse(depth, Map[String, List[(String, Double)]]()).getOrElse(vType, List[(String, Double)]()).toMap
              val aggs2 = numSecondAttrs.getOrElse(depth, Map[String, List[(String, Double)]]()).getOrElse(vType, List[(String, Double)]()).toMap

              acc_ii + (aggs1.keySet ++ aggs2.keySet).foldLeft(0.0)((acc_iii, pred) => {
                //value will be a distance
                val value = aggs1.contains(pred) && aggs2.contains(pred) match {
                  case false => 1.0
                  case true =>
                    val domain = getKB.getPredicate(pred).getArgumentRoles.zip(getKB.getPredicate(pred).getDomainObjects).filter(_._1 == Settings.ARG_TYPE_NUMBER)
                    require(domain.length == 1, s"SimilarityNeighbourhoodTrees::attributeSimilarity : predicate $pred has more than one number domain!")
                    math.abs(aggs1(pred) - aggs2(pred)) / domain.head._2.asInstanceOf[NumericDomain].getRange
                }
                acc_iii + (bagCompare.needsToBeInverted match {
                  case true =>
                    // needs to be a distance value
                    value
                  case false =>
                    // value represents a similarity
                    1.0 - value
                })
              })
            })
          })
        })
    }

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      acc + firstAttrs(depth).keySet.union(secondAttrs(depth).keySet).foldLeft(0.0)( (acc_i, vType) => {
        acc_i + math.abs(bagCompare.compareBags(firstAttrs(depth).getOrElse(vType, List[(String,String)]()),
                                                secondAttrs(depth).getOrElse(vType, List[(String,String)]())))
      })
    }) + numericalValue
  }

  /** Calculates the number of connections between two root elements
    *
    * @param ng1 the first element's [[NeighbourhoodGraph]]
    * @param ng2 the second element's [[NeighbourhoodGraph]]
    * @return [[Double]]
    * */
  protected def elementConnections(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val depthOneVertices = ng2.collectVertexIdentity()(0).getOrElse(ng1.getRootDomain, List[String]())

    depthOneVertices.count( _ == ng1.getRoot.getEntity ).toDouble
  }

  /** Computes the vertex identity distributions similarity for two neighbourhood graphs, per level and vertex type
    *
    * @param ng1 the first element's [[NeighbourhoodGraph]]
    * @param ng2 the second element's [[NeighbourhoodGraph]]
    * @return [[Double]]
    * */
  protected def vertexIdentityDistribution(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val neighbourhood1 = ng1.collectVertexIdentity()
    val neighbourhood2 = ng2.collectVertexIdentity()

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      acc + neighbourhood1(depth).keySet.union(neighbourhood2(depth).keySet).foldLeft(0.0)( (acc_i, vType) => {
        acc_i + math.abs(bagCompare.compareBags(neighbourhood1(depth).getOrElse(vType, List[String]()), neighbourhood2(depth).getOrElse(vType, List[String]())))
      })
    })
  }

  /** Calculates the edge distribution similarity between two neighbourhood graphs, per level
    *
    * @param ng1 the first element's [[NeighbourhoodGraph]]
    * @param ng2 the second element's [[NeighbourhoodGraph]]
    * @return [[Double]]
    * */
  protected def edgeDistributionsSimilarity(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val distr1 = ng1.getEdgeDistribution
    val distr2 = ng2.getEdgeDistribution

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      acc + math.abs(bagCompare.compareBags(distr1(depth), distr2(depth)))
    })
  }

  /** Accumulates the similarity measure of provided elements in a matrix
    *
    * @param elements list of element-domain tuples: [[List]]
    * @param simFunc similarity functions that takes two neighbourhood trees as input and return a Double
    * @param shouldBeInverted flag indicating should the computed matrix be inverted in order to be a similarity measure: [[Boolean]]
    * */
  protected def accumulateIntoMatrix(elements: List[(String,String)], simFunc: (NeighbourhoodGraph, NeighbourhoodGraph) => Double, shouldBeInverted: Boolean, constInd: Int) = {
    val similarityMatrix = DenseMatrix.zeros[Double](elements.length, elements.length)

    for(ind1 <- elements.indices; ind2 <- (ind1 + 1) until elements.length) {
      val simValue = simFunc(getNeighbourhoodGraph(elements(ind1)._1, elements(ind1)._2), getNeighbourhoodGraph(elements(ind2)._1, elements(ind2)._2))

      similarityMatrix(ind1, ind2) += simValue
      similarityMatrix(ind2, ind1) += simValue
    }

    shouldBeInverted match {
      case true => normalizeAndInvert(similarityMatrix, constInd, "v")
      case false => normalizeMatrix(similarityMatrix, constInd, "v")
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
  def getHyperEdgeSimilarity(domains: List[String]) = {
    val hyperEdges = getHyperEdges(domains)

    val functionsWithNorm = List(false, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(List[NeighbourhoodGraph], List[NeighbourhoodGraph]) => Double](hyperedgeAttributeSimilarity, hyperEdgeAttributeNeighbourhoodSimilarity, hyperEdgeConnections, hyperEdgeVertexDistribution, hyperEdgeEdgeDistribution)
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
  def getPairHyperEdgeSimilarity(nt1: List[NeighbourhoodGraph], nt2: List[NeighbourhoodGraph]) = {
    val functionsWithNorm = List(false, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(List[NeighbourhoodGraph], List[NeighbourhoodGraph]) => Double](hyperedgeAttributeSimilarity, hyperEdgeAttributeNeighbourhoodSimilarity, hyperEdgeConnections, hyperEdgeVertexDistribution, hyperEdgeEdgeDistribution)
    )

    weights.zipWithIndex.filter( _._1 > 0.0).foldLeft(0.0)( (acc, w) => {
      val norm = hyperEdgeNormConstants(w._2) == 0.0 match {
        case true => 1.0
        case false => hyperEdgeNormConstants(w._2)
      }
      acc + w._1 * (functionsWithNorm(w._2)._1 match {
        case true => 1.0 - (functionsWithNorm(w._2)._2(nt1, nt2)/norm)
        case false => functionsWithNorm(w._2)._2(nt1, nt2)/norm
      })
    })

  }

  /** computes the similarity between hyperEdge in vertices according to their attributes
    *
    * @param ngs1 the first hyperedge (ordered set of vertices)
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperedgeAttributeSimilarity(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]) = {
    val ngOneCombined = ngs1.map(_.getRootAttributes).foldLeft(Set[(String,String)]())((acc, attrSet) => {
      bagCombine.combineBags(acc.toList, attrSet.toList).toSet
    })
    val ngTwoCombined = ngs2.map(_.getRootAttributes).foldLeft(Set[(String,String)]())( (acc, attrSet) => {
      bagCombine.combineBags(acc.toList, attrSet.toList).toSet
    })

    ngOneCombined.intersect(ngTwoCombined).size.toDouble
  }

  /** Computes the similarity between hyperEdges accoring to the attribute neighbourhoods of the vertices it connects
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeAttributeNeighbourhoodSimilarity(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]) = {
    val firstNeighbourhoods = ngs1.map(_.getAttributeValueDistribution)
    val secondNeighbourhoods = ngs2.map(_.getAttributeValueDistribution)

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      val firstDepthFilteredAttrs = firstNeighbourhoods.map( _(depth))
      val secondDepthFilteredAttrs = secondNeighbourhoods.map(_(depth))
      val keys = firstDepthFilteredAttrs.map( _.keySet.toList).foldLeft(Set[String]())( (acc, dom) => bagCombine.combineBags(acc.toList, dom).toSet).union(
        secondDepthFilteredAttrs.map( _.keySet.toList).foldLeft(Set[String]())( (acc, dom) => bagCombine.combineBags(acc.toList, dom).toSet)
      )
      acc + keys.foldLeft(0.0)( (acc_i, vType) => {
        val firstBag = firstDepthFilteredAttrs.map( _.getOrElse(vType, List[(String,String)]())).foldLeft(List[(String,String)]())((acc, attrSet) => {
          bagCombine.combineBags(acc, attrSet)
        })
        val secondBag = secondDepthFilteredAttrs.map(_.getOrElse(vType, List[(String,String)]())).foldLeft(List[(String,String)]())( (acc, attrSet) => {
          bagCombine.combineBags(acc, attrSet)
        })
        acc_i + math.abs(bagCompare.compareBags(firstBag.map(e => s"${e._1}${e._2}"), secondBag.map(e => s"${e._1}${e._2}")))
      })
    })
  }

  /** Calculates the number of connections between vertices in two hyperedges
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeConnections(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]) = {
    val depthOneVertices = ngs2.map( _.collectVertexIdentity()(0)).map( x => x.foldLeft(List[String]())( (acc, dom) => {
      acc ++ dom._2
    })).reduce(_ ++ _)

    ngs1.map( _.getRoot.getEntity).foldLeft(0.0)( (acc, elem) => {
      acc + depthOneVertices.count( _ == elem).toDouble
    })
  }

  /** Calculates the similarity in vertex neighbourhoods of the vertices in hyperedges
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeVertexDistribution(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]) = {
    val firstVertices = ngs1.map( _.collectVertexIdentity())
    val secondVertices = ngs2.map(_.collectVertexIdentity())

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      val firstDepthFiltered = firstVertices.map( _(depth))
      val secondDepthFiltered = secondVertices.map(_(depth))
      val iterables = firstDepthFiltered.map(_.keySet.toList).foldLeft(Set[String]())( (acc, dom) => bagCombine.combineBags(acc.toList, dom).toSet).union(
        secondDepthFiltered.map(_.keySet.toList).foldLeft(Set[String]())( (acc, dom) => bagCombine.combineBags(acc.toList, dom).toSet)
      )
      acc + iterables.foldLeft(0.0)( (acc_i, vType) => {
        val firstBag = firstDepthFiltered.map(_.getOrElse(vType, List[String]())).foldLeft(List[String]())( (acc, vSet) => bagCombine.combineBags(acc, vSet))
        val secondBag = secondDepthFiltered.map(_.getOrElse(vType, List[String]())).foldLeft(List[String]())( (acc, vSet) => bagCombine.combineBags(acc, vSet))
        acc_i + math.abs(bagCompare.compareBags(firstBag, secondBag))
      })
    })
  }

  /** Calculates the edge distribution similarity between vertices in a hyperedge
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeEdgeDistribution(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]) = {
    val firstDistribution = ngs1.map(_.getEdgeDistribution)
    val secondDistribution = ngs2.map(_.getEdgeDistribution)

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      val firstDepthFiltered = firstDistribution.map( _(depth))
      val secondDepthfiltered = secondDistribution.map(_(depth))

      acc + math.abs(bagCompare.compareBags(firstDepthFiltered.foldLeft(List[String]())( (acc, eSet) => bagCombine.combineBags(acc, eSet)),
                                            secondDepthfiltered.foldLeft(List[String]())( (acc, eSet) => bagCombine.combineBags(acc, eSet))))
    })
  }

  /** Accumulates the similarity measure of provided elements in a matrix
    *
    * @param elements a list of hyperedges
    * @param domains domains of elements in a hyperedge
    * @param simFunction similarity measure
    * */
  protected def accumulateIntoMatrixHyperEdge(elements: List[List[String]], domains: List[String], simFunction: (List[NeighbourhoodGraph], List[NeighbourhoodGraph]) => Double, shouldBeInverted: Boolean, constId: Int) = {
    val similarityMatrix = DenseMatrix.zeros[Double](elements.length, elements.length)

    for(ind1 <- elements.indices; ind2 <- (ind1 + 1) until elements.length) {
      val simValue = simFunction(elements(ind1).zipWithIndex.map(x => getNeighbourhoodGraph(x._1, domains(x._2))),
                                 elements(ind2).zipWithIndex.map( x => getNeighbourhoodGraph(x._1, domains(x._2))))

      similarityMatrix(ind1, ind2) += simValue
      similarityMatrix(ind2, ind1) += simValue
    }

    shouldBeInverted match {
      case true => normalizeAndInvert(similarityMatrix, constId, "h")
      case false => normalizeMatrix(similarityMatrix, constId, "h")
    }
  }
}
