package relationalClustering.clustering.evaluation.supervised

import relationalClustering.representation.clustering.Clustering

/**
  * Created by seb on 21.03.16.
  */
class MajorityClass extends AbstractEvaluatorWithLabels("./tmp") {

  /** Each examples is assigned with a majority label in a cluster
    *
    * @param clusters clustering
    * @param labels labels of the examples
    * @return accuracy of such assignment (#correctly assigned labels / #elements)
    * */
  def validate(clusters: Clustering, labels: LabelsContainer) = {

    // counts the number of examples within the class that have the same true label than the majority label
    val goodOnes = clusters.getClusters.map( cl => new Tuple2(cl.getInstances.map(_.mkString(":")).toList, AssignMajorityClass(cl.getInstances.map(_.mkString(":")).toList, labels))).foldLeft(List[(String,String)]())( (acc, clust) => {
      acc ++ clust._1.distinct.map( x => new Tuple2(labels.getLabel(x), clust._2))
    }).count( x => x._1 == x._2)

    val total = clusters.getClusters.foldLeft(0.0)( (acc, cl) => acc + cl.getSize.toDouble)

    goodOnes.toDouble/total
  }

  /** Returns the majority class in a cluster
    *
    * @param cluster a list of element names in a cluster
    * @param labels label container
    * */
  protected def AssignMajorityClass(cluster: List[String], labels: LabelsContainer) = {
    val labs = cluster.map( ex => labels.getLabel(ex))
    val counts = labs.distinct.map( l => new Tuple2(l, labs.count( _ == l)))

    counts.maxBy(_._2)._1
  }


}
