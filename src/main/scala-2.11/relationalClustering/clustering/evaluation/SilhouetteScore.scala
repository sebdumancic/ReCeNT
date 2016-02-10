package relationalClustering.clustering.evaluation

import java.io.FileWriter

import scala.sys.process._

/**
  * Created by seb on 09.02.16.
  */
class SilhouetteScore(override protected val rootFolder: String,
                      override protected val similarityMatrixFile: String) extends AbstractEvaluatorModel(rootFolder, similarityMatrixFile) {

  protected val script =
    """
      |import argparse
      |from sklearn import metrics
      |import numpy as np
      |
      |parser = argparse.ArgumentParser(description='Read arguments for clustering experiments.')
      |parser.add_argument('--similarityMatrix', help='similarity matrix to use', required=True)
      |parser.add_argument('--labels', help='cluster labels in the same order as the elements in the similarity matrix', required=True)
      |parser.add_argument('--score', help='Score to evaluate a clustering [silhouette]', nargs=1, default=['silhouette'], choices=['silhouette'])
      |
      |args = parser.parse_args()
      |
      |similarityMatrix = np.loadtxt(args.similarityMatrix, delimiter=";", comments="#")
      |maxNum = similarityMatrix.max()
      |distanceMatrix = 1.0 - np.divide(similarityMatrix, similarityMatrix.max())
      |
      |labels = np.loadtxt(args.labels, delimiter=";", comments="#")
      |
      |result = 0.0
      |
      |if args.score[0] == 'silhouette':
      |    result = metrics.silhouette_score(distanceMatrix, labels, metric='precomputed')
      |
      |print "{}: {}".format(args.score[0], result)
    """.stripMargin
  prepareScript()

  protected def prepareScript() = {
    val writer = new FileWriter(s"$getRoot/cluster_evaluate_script.py")
    writer.write(script)
    writer.close()
  }

  protected def labelsFile = {
    s"labels.txt"
  }

  protected def command = {
    s"python $getRoot/cluster_evaluate_script.py --similarityMatrix $similarityMatrixFile --labels $getRoot/$labelsFile --score silhouette "
  }


  def validate(clusters: Set[List[String]], elementOrder: List[String]) = {
    val labels = getLabels(clusters, elementOrder)
    saveLabelsToFile(labels, s"$labelsFile")

    val stream = command.!!
    stream.split(":")(1).toDouble
  }

}
