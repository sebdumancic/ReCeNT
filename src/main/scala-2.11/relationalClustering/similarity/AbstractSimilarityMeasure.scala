package relationalClustering.similarity

import java.io.{BufferedWriter, File, FileWriter}

import breeze.linalg.{DenseMatrix, max, min}
import relationalClustering.neighbourhood.{NeighbourhoodGraph, NodeRepository}
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.utils.Settings

/** Abstract Similarity measure class
  * Created by seb on 18.01.16.
  */
abstract class AbstractSimilarityMeasure(protected val knowledgeBase: KnowledgeBase,
                                         protected val depth: Int) {

  protected val nodeRepository = new NodeRepository(knowledgeBase)
  protected val objectsNormConstants = collection.mutable.Map[Int, Double]()
  protected val hyperEdgeNormConstants = collection.mutable.Map[Int, Double]()

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
    }).sortBy(_._1)
  }

  /** Checks whether a hyperedge is contained in a predicate domains
    *
    * @param domains domains of the hyperEdge
    * @param testHyperEdge domains of a predicate
    * */
  protected def hyperEdgeContained(domains: List[String], testHyperEdge: List[String]) = {
    val domainCounts = domains.map( dom => new Tuple2(dom, domains.count(_ == dom))).toSet

    domainCounts.map( cand => testHyperEdge.count(_ == cand._1) >= cand._2).reduce(_ && _)
  }

  /** Extracts all the hyper-edges in a graph
    *
    * @param domains hyperEdge domains (should be sorted alphabetically!)
    * */
  def getHyperEdges(domains: List[String]) = {
    require(domains.length > 1, "Hyperedge requires at least two domains to be specified")
    require(domains == domains.sorted, "Domains have to be sorted alphabetically")

    getKB.getPredicateNames.map(getKB.getPredicate).filter( _.getRole == Settings.ROLE_HYPEREDGE).filter( pred => hyperEdgeContained(domains, pred.getDomains)).foldLeft(Set[List[String]]())( (acc, pred) =>{
      val doms = pred.getDomains
      val validTuples = pred.getTrueGroundings.foldLeft(Set[List[String]]())( (acc_i, ground) => {
        acc_i ++ ground.zip(doms).combinations(domains.length).map( _.sortBy(_._2)).filter( curGround => curGround.map(_._2) == domains).map( c => c.map(_._1))
      })
      acc ++ validTuples
    }).toList.sortBy(_.mkString)
  }

  /** Method implementing an interface for accessing similarity of object(s) from specified domain(s)
    *
    * @param domains list of domains to cluster objects from: [[List]]
    * @return (ordering of objects, similarity matrix for corresponding element)
    * */
  def getObjectSimilarity(domains: List[String]): (List[String], DenseMatrix[Double])

  /** Get the similarity matrix and saves it to the file
    *
    * @param domains domains of interest
    * @param folder folder to save file
    * @return filename of the file containing similarity matrix, and the element ordering
    *
    * */
  def getObjectSimilaritySave(domains: List[String], folder: String) = {
    if (!new File(s"$folder/${getFilename(domains)}").exists()) {
      val (elems, sim) = getObjectSimilarity(domains)
      saveMatrixToFile(s"$folder/${getFilename(domains)}", domains, elems, sim)
    }

    val absolutePath = new File(s"$folder/${getFilename(domains)}")

    (absolutePath.getAbsolutePath, getObjectsFromDomains(domains))
  }

  /** Calculates similarity between two individual neighbourhood trees (normalizing constants have to be calculated before!!!)
    *
    * @param nt1 the first neighbourhood tree
    * @param nt2 the second neighbourhood tree
    * @return similarity
    * */
  def pairObjectSimilarity(nt1: NeighbourhoodGraph, nt2: NeighbourhoodGraph): Double

  /** Uniquely identifies the filename to save similarity matrix (once calculated it can be reused)
    *
    * @param domains list of domains of interest
    * */
  def getFilename(domains: List[String]): String

  /** Uniquely identifies the filename to save hyperedge similarity matrix (once calculated it can be reused)
    *
    * @param domains list of domains of interest
    * */
  def getFilenameHyperEdges(domains: List[String]): String

  /** Method implementing an interface to assess the similarity of edges connecting objects from specified domains
    *
    * @param domains list of domains that hyperedges connect: [[List]]
    * @return (ordering of hyperEdges, similarity matrix)
    * */
  def getHyperEdgeSimilarity(domains: List[String]): (List[List[String]], DenseMatrix[Double])

  /** Get the hyper-edge similarity matrix and save it to file
    *
    * @param domains domains of the hyperedges
    * @param folder folder to save the file to
    * @return fileName, an ordering of elements
    * */
  def getHyperEdgeSimilaritySave(domains: List[String], folder: String) = {
    if (!new File(s"$folder/${getFilenameHyperEdges(domains)}").exists()) {
      val (elems, sim) = getHyperEdgeSimilarity(domains)
      saveMatrixToFile(s"$folder/${getFilenameHyperEdges(domains)}", domains, elems.map( _.mkString(":")), sim)
    }

    val absolutePath = new File(s"$folder/${getFilenameHyperEdges(domains)}")

    //(s"$folder/${getFilenameHyperEdges(domains)}", getHyperEdges(domains))
    (absolutePath.getAbsolutePath,getHyperEdges(domains) )
  }

  /** Calculates similarity between two individual hyperedges (normalizing constants have to be calculated before!!!)
    *
    * @param nt1 an ordered set of neighbourhood trees
    * @param nt2 an ordered set of neirghbouyrhood trees
    *
    * */
  def getPairHyperEdgeSimilarity(nt1: List[NeighbourhoodGraph], nt2: List[NeighbourhoodGraph]): Double

  /** Normalizes the given matrix by its largest value
    *
    * @param matrix matrix to normalize: [[DenseMatrix]]
    * @param constInd identifier for the normalization constant (in case of multiple components)
    * @param typeFlag v for vertex constant, h for hyperedge constant
    * */
  def normalizeMatrix(matrix: DenseMatrix[Double], constInd: Int, typeFlag: String): DenseMatrix[Double] = {
    val minValue = min(matrix)
    val matrixToUse = minValue < 0.0 match {
      case true => matrix - DenseMatrix.tabulate(matrix.rows, matrix.cols){ case x => minValue }
      case false => matrix
    }
    val normConstant = math.abs(max(matrixToUse))

    // store the normalization constant, needed to assign new objects to the existing clusters
    typeFlag match {
      case "v" => objectsNormConstants(constInd) = normConstant
      case "h" => hyperEdgeNormConstants(constInd) = normConstant
    }

    normConstant == 0.0 match {
      case true => matrixToUse
      case false => matrixToUse :/ DenseMatrix.tabulate(matrix.rows, matrix.cols) { case x => normConstant }
    }
  }

  /** Normalizes and inverts a given matrix (1 - normalized matrix)
    *
    * @param matrix matrix to invert: [[DenseMatrix]]
    * */
  def normalizeAndInvert(matrix: DenseMatrix[Double], constInd: Int, typeFlag: String): DenseMatrix[Double] = {
    if (max(matrix) == 0.0 ) {
      return matrix
    }
    DenseMatrix.tabulate(matrix.rows, matrix.cols) { case x => 1.0 } :- normalizeMatrix(matrix, constInd, typeFlag)
  }


  /** Saves matrix in the file
    *
    * @param filename name of the file (filePath)
    * @param domainElements ordered list of element names
    * @param similarityMatrix similarity matrix corresponding to the provided list of elements
    */
  def saveMatrixToFile(filename: String, domains: List[String], domainElements: List[String], similarityMatrix: DenseMatrix[Double]) = {
    val writer = new BufferedWriter(new FileWriter(s"$filename"))
    try {
      writer.write("#" + domainElements.mkString(";") + "\n") //print element names

      //matrix content
      for (rowI <- 0 until similarityMatrix.rows) {
        for (columnI <- 0 until similarityMatrix.cols) {
          writer.write(BigDecimal(similarityMatrix(rowI, columnI)).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble + "")
          if (columnI < (similarityMatrix.cols - 1)) {
            writer.write(";")
          }
        }
        writer.write("\n")
      }
    }
    finally {
      writer.close()
    }
  }

  /** Clears any cache a measure might use*/
  def clearCache(): Unit


}
