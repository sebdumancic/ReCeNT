package relationalClustering.aggregators

/**
  * Created by seb on 12.09.16.
  */
class MinAggregator extends AbstractAggregator {

  override def aggregate(elements: List[(String, Double)]): Double = {
    elements.minBy(_._2)._2
  }
}
