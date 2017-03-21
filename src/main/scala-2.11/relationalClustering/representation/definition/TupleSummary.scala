package relationalClustering.representation.definition

import breeze.linalg.DenseVector
import breeze.stats._

/**
  * Created by seb on 21.03.17.
  */
class TupleSummary(protected val numObj: Int,
                   protected val summaries: Map[String, (Double, Double)],
                   override protected val depth: Int,
                   override protected val vertexType: String,
                   override protected val similaritySource: String,
                   override protected val dimension: Int) extends TupleContext(depth, vertexType, similaritySource, dimension) {

  def this(numObj: Int, tuples: List[List[(String, Double)]], depth: Int, vertexType: String, similaritySource: String, dimension: Int) = {
    this(numObj, summarizeNumericTuples(tuples), depth, vertexType, similaritySource, dimension)
  }

  def getSummaries: Map[String, (Double, Double)] = {
    summaries
  }

  def isEmpty: Boolean = {
    summaries.isEmpty
  }

  def withFilter(maxDeviation: Double = 0.2): TupleSummary = {
    val finalSummaries = getSummaries.filter(elem => elem._2._2 <= (elem._2._1 * maxDeviation))
    new TupleSummary(numObj, finalSummaries, depth, vertexType, similaritySource, dimension)
  }


  /** Summarizes the numeric attributes in the neighbourhood
    *
    * tuples ->  a list of instances, where each instance in a list of tuples
    *
    * @return a map where the key is an attribute name, and the value is a tuple (mean, std)
    **/
  protected def summarizeNumericTuples(tuples: List[List[(String, Double)]]): Map[String, (Double, Double)] = {
    val tupleSummary = collection.mutable.Map[String, List[Double]]()

    tuples.foreach(instance => {
      instance.foreach(attr => {
        if (!tupleSummary.contains(attr._1)) {
          tupleSummary(attr._1) = List[Double]()
        }
        tupleSummary(attr._1) = tupleSummary(attr._1) :+ attr._2
      })
    })

    tupleSummary.foldLeft(Map[String, (Double, Double)]())((acc, attr) => {
      val breezeArray = DenseVector.tabulate(attr._2.length) { i => attr._2(i) }
      acc + (attr._1 -> (mean(breezeArray), stddev(breezeArray)))
    })
  }

  def stringRep(initialOffset: Int = 0): String = {
    getSummaries.toList.sortBy(_._2._2).map(el => {
      s"${"\t" * initialOffset}${el._1} -> mean ${el._2._1}, stddev ${el._2._2}"
    }).mkString("\n")
  }

  override def toString: String = {
    stringRep()
  }
}
