package relationalClustering.clustering.evaluation

import java.io.FileWriter

import relationalClustering.representation.clustering.{Cluster, Clustering}

/**
  * Created by seb on 05.02.16.
  */
abstract class AbstractEvaluatorWithLabels(override protected val rootFolder: String) extends AbstractEvaluator(rootFolder) {

  /** Validate the provided clusters
    *
    * @param clusters a set of clusters, where a cluster is represented as a list of elements
    * @param labels ground truth cluster indications
    * @return [[Double]]
    * */
  def validate(clusters: Clustering, labels: LabelsContainer): Double

  /** Transforms the cluster element in form (cluster id, element label id)
    *
    * @param clusterId id of the cluster
    * @param cluster list of elements in a cluster
    * @param labels label container of the corresponding elements
    * */
  protected def combineClusterWithGroundTruth(clusterId: Int, cluster: Cluster, labels: LabelsContainer) = {
    cluster.getInstances.map(_.mkString(",")).toList.map( e => (clusterId, labels.getLabelId(e)) ).filter( _._2 != -1)
  }

  /** Transforms the clusters in a list of (cluster id, labels id)
    *
    * @param clusters set of clusters
    * @param labels ground truth for elements in clusters
    * @return list of (cluster id, label)
    * */
  protected def combineWithGroundTruth(clusters: Clustering, labels: LabelsContainer) = {
    clusters.getClusters.zipWithIndex.foldLeft(List[(Int, Int)]())( (acc, clst) => {
      acc ++ combineClusterWithGroundTruth(clst._2, clst._1, labels)
    })
  }

  /** Saves tuples to file
    *
    * @param items tuples to save
    * @param filename path to the file
    * @param delimiter delimiter to separate tuple elements
    * */
  protected def saveTuplesToFile(items: List[(Int,Int)], filename: String, delimiter: String = ";") = {
    val writer = new FileWriter(filename)

    items.foreach( item => writer.write(s"${item._1}$delimiter${item._2}" + sys.props("line.separator")))
    writer.close()
  }
}
