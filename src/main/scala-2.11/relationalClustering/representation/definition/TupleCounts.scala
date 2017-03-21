package relationalClustering.representation.definition

/**
  * Created by seb on 21.03.17.
  */
class TupleCounts(protected val numObjects: Int,
                  protected val tuples: List[List[(String, String)]],
                  override protected val depth: Int,
                  override protected val vertexType: String,
                  override protected val similaritySource: String,
                  override protected val dimension: Int) extends TupleContext(depth, vertexType, similaritySource, dimension) {

  protected val counts: Map[(String, String), Int] = processTuples

  def getCounts: Map[(String, String), Int] = {
    counts
  }

  def getCountsGrouped: Map[String, List[(String, Int)]] = {
    val tmpMap = collection.mutable.Map[String, List[(String, Int)]]()

    getCounts.toList.groupBy(_._1._1).foreach(coll => {
      tmpMap(coll._1) = coll._2.map(it => (it._1._2, it._2))
    })

    tmpMap.toMap
  }

  /** Counts occurrence of individual tuples over difference instances
    *
    * tuples ->  list of instances, where each instance is a list of tuples
    *
    * @return a map where a tuples is a key, and its count is value
    *
    **/
  protected def processTuples: Map[(String, String), Int] = {
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

  def stringRep(initialOffset: Int = 0): String = {
    getCountsGrouped.toList.sortBy(el => el._2.map(_._2).sum).reverse.map(el => {
      s"${"\t" * initialOffset}${el._1} = \n" + el._2.map(it => s"${"\t" * (initialOffset + 1)}${it._1} -- > ${it._2}").mkString("\n")
    }).mkString("\n")
  }

  override def toString: String = {
    stringRep()
  }

}
