package relationalClustering.similarity

import breeze.linalg.{max, min, DenseMatrix}
import relationalClustering.neighbourhood.NodeRepository
import relationalClustering.representation.KnowledgeBase

/** Abstract Similarity measure class
  * Created by seb on 18.01.16.
  */
abstract class AbstractSimilarityMeasure(protected val knowledgeBase: KnowledgeBase,
                                         protected val depth: Int) {

  protected val nodeRepository = new NodeRepository(knowledgeBase)

  /** Returns the specified depth */
  def getDepth = {
    depth
  }

  /** Returns the knowledge base */
  protected def getKB = {
    knowledgeBase
  }

  /** Returns the node repository */
  protected def getRepo = {
    nodeRepository
  }

  /** Composes a list of object in specified domains
    *
    * @param domains list of domains to compose objects from
    * @return list of (object name, domain)
    * */
  protected def getObjectsFromDomains(domains: List[String]) = {
    domains.foldLeft(List[(String,String)]())( (acc, dom) => {
      acc ++ getKB.getDomain(dom).getElements.map( x => new Tuple2(x, dom))
    })
  }

  /** Method implementing an interface for accessing similarity of object(s) from specified domain(s)
    *
    * @param domains list of domains to cluster objects from: [[List]]
    * @return (ordering of objects, similarity matrix for corresponding element)
    * */
  def getObjectSimilarity(domains: List[String]): (List[String], DenseMatrix[Double])

  /** Uniquely identifies the filename to save similarity matrix (once calculated it can be reused)
    *
    * @param domains list of domains of interest
    * */
  def getFilename(domains: List[String]): String

  /** Method implementing an interface to assess the similarity of edges connecting objects from specified domains
    *
    * @param domains list of domains that hyperedges connect: [[List]]
    * @return (ordering of hyperEdges, similarity matrix)
    * */
  def getHyperEdgeSimilarity(domains: List[String]): (List[List[String]], DenseMatrix[Double])

  /** Normalizes the given matrix by its largest value
    *
    * @param matrix matrix to normalize: [[DenseMatrix]]
    * */
  def normalizeMatrix(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    val minValue = min(matrix)
    val matrixToUse = minValue < 0.0 match {
      case true => matrix - DenseMatrix.tabulate(matrix.rows, matrix.cols){ case x => minValue }
      case false => matrix
    }
    val normConstant = math.abs(max(matrixToUse))
    matrixToUse :/ DenseMatrix.tabulate(matrix.rows, matrix.cols) { case x => normConstant }
  }

  /** Normalizes and inverts a given matrix (1 - normalized matrix)
    *
    * @param matrix matrix to invert: [[DenseMatrix]]
    * */
  def normalizeAndInvert(matrix: DenseMatrix[Double]) = {
    DenseMatrix.tabulate(matrix.rows, matrix.cols) { case x => 1.0 } :- normalizeMatrix(matrix)
  }


}
