package relationalClustering.aggregators

/**
  * Created by seb on 13.09.16.
  */
class SumAggregator extends AbstractAggregator("sum") {

  override def aggregate(elements: List[(String, Double)]): Double = {
    elements.map(_._2).sum
  }
}
