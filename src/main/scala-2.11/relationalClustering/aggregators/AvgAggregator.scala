package relationalClustering.aggregators

import breeze.linalg.DenseVector
import breeze.stats._

/**
  * Created by seb on 12.09.16.
  */
class AvgAggregator extends AbstractAggregator {

  override def aggregate(elements: List[(String, Double)]): Double = {
    mean(DenseVector.tabulate(elements.length){ i => elements(i)._2})
  }
}
