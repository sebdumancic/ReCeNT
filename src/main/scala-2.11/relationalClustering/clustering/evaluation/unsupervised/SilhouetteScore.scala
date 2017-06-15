package relationalClustering.clustering.evaluation.unsupervised

import java.io.{File, FileWriter}

import relationalClustering.representation.clustering.Clustering

import scala.sys.process._

/**
  * Created by seb on 09.02.16.
  */
class SilhouetteScore(override protected val rootFolder: String) extends AbstractEvaluatorModel(rootFolder) {

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

  protected def command(similarityFile: String) = {
    s"python $getRoot/cluster_evaluate_script.py --similarityMatrix $similarityFile --labels $getRoot/$labelsFile --score silhouette "
  }


  def validate(clusters: Clustering) = {
    prepareScript()
    val labels = getLabels(clusters, clusters.getElementOrdering.map(_.mkString(":")))
    saveLabelsToFile(labels, s"$labelsFile")

    val stream = command(clusters.getSimilarityFilename).!!
    cleanArtifacts
    stream.split(":")(1).toDouble
  }

  protected def cleanArtifacts = {
    val labels = new File(s" $getRoot/$labelsFile")
    val script = new File(s"$getRoot/cluster_evaluate_script.py")

    labels.delete()
    script.delete()
  }

}
