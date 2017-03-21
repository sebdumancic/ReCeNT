package relationalClustering.representation.definition

/**
  * Created by seb on 21.03.17.
  */
abstract class TupleContext(protected val depth: Int,
                            protected val vertexType: String,
                            protected val similaritySource: String,
                            protected val dimension: Int) {

  def getDepth: Int = {
    depth
  }

  def getVType: String = {
    vertexType
  }

  def getSimilaritySource: String = {
    similaritySource
  }

  def getDimension: Int = {
    dimension
  }

  def stringRep(initialOffset: Int = 0): String
}
