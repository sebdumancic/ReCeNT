package relationalClustering.aggregators

import breeze.linalg._
import breeze.stats._

/**
  * Created by seb on 12.09.16.
  */
class StdAggregator extends AbstractAggregator("stddev") {

  override def aggregate(elements: List[(String, Double)]): Double = {
    stddev(DenseVector.tabulate(elements.length){ i => elements(i)._2})
  }
}
