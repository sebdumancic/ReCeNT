package relationalClustering.similarity

import java.io.{BufferedWriter, File, FileWriter}

import breeze.linalg.{DenseMatrix, max, min}
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
    }).sortBy(_._1)
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
    * @return filename of the file containing similarity matrix
    *
    * */
  def getObjectSimilaritySave(domains: List[String], folder: String) = {
    if (!new File(s"$folder/${getFilename(domains)}").exists()) {
      val (elems, sim) = getObjectSimilarity(domains)
      saveMatrixToFile(folder, domains, elems, sim)
    }
    s"$folder/${getFilename(domains)}"
  }

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
    normConstant == 0.0 match {
      case true => matrixToUse
      case false => matrixToUse :/ DenseMatrix.tabulate(matrix.rows, matrix.cols) { case x => normConstant }
    }
  }

  /** Normalizes and inverts a given matrix (1 - normalized matrix)
    *
    * @param matrix matrix to invert: [[DenseMatrix]]
    * */
  def normalizeAndInvert(matrix: DenseMatrix[Double]) = {
    DenseMatrix.tabulate(matrix.rows, matrix.cols) { case x => 1.0 } :- normalizeMatrix(matrix)
  }


  /** Saves matrix in the file
    *
    * @param folder name of the file (filePath)
    * @param domainElements ordered list of element names
    * @param similarityMatrix similarity matrix corresponding to the provided list of elements
    */
  def saveMatrixToFile(folder: String, domains: List[String], domainElements: List[String], similarityMatrix: DenseMatrix[Double]) = {
    val writer = new BufferedWriter(new FileWriter(s"$folder/${getFilename(domains)}"))
    try {
      writer.write("#" + domainElements.mkString(";") + "\n") //print element names

      //matrix content
      for (rowI <- 0 until similarityMatrix.rows) {
        for (columnI <- 0 until similarityMatrix.cols) {
          writer.write(similarityMatrix(rowI, columnI) + "")
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


}
