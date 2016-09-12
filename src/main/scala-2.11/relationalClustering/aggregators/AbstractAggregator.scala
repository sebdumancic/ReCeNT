package relationalClustering.aggregators

/**
  * Created by seb on 12.09.16.
  */
abstract class AbstractAggregator(val name: String) {

  def aggregate(elements: List[(String, Double)]): Double
}
