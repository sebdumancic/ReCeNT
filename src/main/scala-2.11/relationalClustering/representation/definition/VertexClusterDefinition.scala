package relationalClustering.representation.definition

/**
  * Created by seb on 21.03.17.
  */
class VertexClusterDefinition(protected val tupleContexts: List[TupleContext]) {

  def getTupleContexts: List[TupleContext] = {
    tupleContexts
  }

  def isEmpty: Boolean = {
    tupleContexts.isEmpty
  }

  protected def stringRep(tuples: List[TupleContext], initialOffset: Int = 0): String = {
    tuples.groupBy(_.getSimilaritySource).map(simGroup => {

      s"${"\t" * initialOffset}SIMILARITY SOURCE ${simGroup._1}\n" + simGroup._2.groupBy(_.getDepth).map(depthGroup => {
        s"${"\t" * (initialOffset + 1)}DEPTH ${depthGroup._1}\n" + depthGroup._2.groupBy(_.getVType).map(typeGroup => {
          s"${"\t" * (initialOffset + 2)}VERTEX TYPE ${typeGroup._1}\n" + typeGroup._2.map(_.stringRep(initialOffset + 3)).mkString("\n") + "\n"
        }).mkString("\n")
      }).mkString("\n")
    }).mkString("\n\n")
  }

  override def toString: String = {
    stringRep(tupleContexts)
  }

  def withFilter(minSupport: Double, maxDeviance: Double): VertexClusterDefinition = {
    val finalContexts = tupleContexts.map(
      _ match {
        case x: TupleCounts => x.withFilter(minSupport)
        case x: TupleSummary => x.withFilter(maxDeviance)
      }).filterNot(_.isEmpty)

    new VertexClusterDefinition(finalContexts)
  }

  def topK(k: Int): VertexClusterDefinition = {
    new VertexClusterDefinition(tupleContexts.map(_.topK(k)))
  }

}
