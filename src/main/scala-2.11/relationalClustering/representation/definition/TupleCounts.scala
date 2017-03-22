package relationalClustering.representation.definition

/**
  * Created by seb on 21.03.17.
  */
class TupleCounts(protected val numObjects: Int,
                  protected val counts: Map[(String, String), Int],
                  override protected val depth: Int,
                  override protected val vertexType: String,
                  override protected val similaritySource: String,
                  override protected val dimension: Int) extends TupleContext(depth, vertexType, similaritySource, dimension) {

  def this(numObjects: Int, intuples: List[List[(String, String)]], depth: Int, vertexType: String, similaritySource: String, dimension: Int) = {
    this(numObjects, TupleHelper.processTuples(intuples), depth, vertexType, similaritySource, dimension)
  }

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

  def isEmpty: Boolean = {
    counts.isEmpty
  }

  def withFilter(support: Double = 0.9): TupleCounts = {
    val finalCounts = collection.mutable.Map[(String, String), Int]()
    getCountsGrouped.filter(_._2.map(_._2).sum >= (numObjects * support).toInt).foreach(attr => {
      attr._2.foreach(item => {
        finalCounts((attr._1, item._1)) = item._2
      })
    })
    new TupleCounts(numObjects, finalCounts.toMap, depth, vertexType, similaritySource, dimension)
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
