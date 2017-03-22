package relationalClustering.representation.definition

import breeze.linalg._
import breeze.stats._

/**
  * Created by seb on 22.03.17.
  */
object TupleHelper {

  /** Counts occurrence of individual tuples over difference instances
    *
    * tuples ->  list of instances, where each instance is a list of tuples
    *
    * @return a map where a tuples is a key, and its count is value
    *
    * */
  def processTuples(tuples: List[List[(String, String)]]): Map[(String, String), Int] = {
    val tupleCount = collection.mutable.Map[(String, String), Int]()

    tuples.foreach(instance => {
      instance.foreach(attr => {
        if (!tupleCount.contains(attr)) {
          tupleCount(attr) = 0
        }
        tupleCount(attr) = tupleCount(attr) + 1
      })
    })

    tupleCount.toMap
  }

  /** Summarizes the numeric attributes in the neighbourhood
    *
    * tuples ->  a list of instances, where each instance in a list of tuples
    *
    * @return a map where the key is an attribute name, and the value is a tuple (mean, std)
    * */
  def summarizeNumericTuples(tuples: List[List[(String, Double)]]): Map[String, (Double, Double)] = {
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
}
