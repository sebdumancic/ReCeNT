package relationalClustering.bagComparison

/**
  * Implements a Chi Squared distance between two normalized histograms
  * Created by seb on 04.02.16.
  */
class ChiSquared() extends AbstractBagCompare("chiSquared") {

  def needsToBeInverted = {
    true
  }

  protected def similarity(hist1: List[Double], hist2: List[Double]) = {
    0.5 * hist1.zip(hist2).map( x => math.pow(x._1 - x._2, 2.0)/(x._1 + x._2)).map(x => if (x.isNaN) 0.0 else x).sum // smoothing added to avoid division by zero
  }
}
