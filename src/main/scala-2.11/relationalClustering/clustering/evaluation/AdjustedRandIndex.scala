package relationalClustering.clustering.evaluation

import java.io.FileWriter

import scala.sys.process._

/**
  * Created by seb on 05.02.16.
  */
class AdjustedRandIndex(override protected val rootFolder: String) extends AbstractEvaluatorWithLabels(rootFolder) {

  val ariScript =
    """
      |from sklearn.metrics.cluster import adjusted_rand_score
      |import argparse
      |import numpy as np
      |
      |parser = argparse.ArgumentParser(description='Read arguments for clustering.')
      |parser.add_argument('--input', help='filename containing distance|similarity matrix', nargs=1, required=True)
      |
      |args = parser.parse_args()
      |inputFile = args.input[0]
      |
      |clustersAndLabels = np.loadtxt(inputFile, delimiter=";", comments="#")
      |randScore = adjusted_rand_score(clustersAndLabels[:,1], clustersAndLabels[:,0])
      |print "ARI: {}".format(randScore)
    """.stripMargin

  prepareScript()

  protected def prepareScript() = {
    val writer = new FileWriter(s"$getRoot/ari_script.py")
    try {
      writer.write(ariScript + "\n")
    }
    finally {
      writer.close()
    }
  }

  /** Validate the provided clusters
    *
    * @param clusters a set of clusters, where a cluster is represented as a list of elements
    * @param labels ground truth cluster indications
    * @return [[Double]]
    * */
  def validate(clusters: Set[List[String]], labels: LabelsContainer) = {
    saveTuplesToFile(combineWithGroundTruth(clusters, labels), s"$getRoot/ari_ground_truth.txt")

    val stream = s"python $getRoot/ari_script.py --input $getRoot/ari_ground_truth.txt".!!
    stream.split(":")(1).toDouble
  }

}
