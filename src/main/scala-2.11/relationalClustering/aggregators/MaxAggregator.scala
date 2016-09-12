package relationalClustering.aggregators

/**
  * Created by seb on 12.09.16.
  */
class MaxAggregator extends AbstractAggregator("max") {

  override def aggregate(elements: List[(String, Double)]): Double = {
    elements.maxBy(_._2)._2
  }
}
