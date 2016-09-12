package relationalClustering.aggregators

/**
  * Created by seb on 12.09.16.
  */
class MinAggregator extends AbstractAggregator("min") {

  override def aggregate(elements: List[(String, Double)]): Double = {
    elements.minBy(_._2)._2
  }
}
