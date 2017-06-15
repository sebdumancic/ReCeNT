package relationalClustering.clustering.evaluation.supervised

/**
  * Created by seb on 03.10.16.
  */
class AdjustedNMI(override protected val rootFolder: String) extends AdjustedRandIndex(rootFolder) {

  override val ariScript =
    """
      |from sklearn.metrics.cluster import adjusted_mutual_info_score
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
      |randScore = adjusted_mutual_info_score(clustersAndLabels[:,1], clustersAndLabels[:,0])
      |print "ANMI: {}".format(randScore)
    """.stripMargin

}
