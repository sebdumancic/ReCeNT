package relationalClustering.representation.definition

/**
  * Created by seb on 21.03.17.
  */
class EdgeClusterDefinition(override protected val tupleContexts: List[TupleContext]) extends VertexClusterDefinition(tupleContexts) {

  override protected def stringRep(tuples: List[TupleContext], initialOffset: Int = 0): String = {
    tupleContexts.groupBy(_.getDimension).toList.sortBy(_._1).map(dimGroup => {
      s"DIMENSION ${dimGroup._1}\n" + super.stringRep(dimGroup._2, 1)
    }).mkString("\n")
  }

  override def toString: String = {
    stringRep(tupleContexts)
  }

}
