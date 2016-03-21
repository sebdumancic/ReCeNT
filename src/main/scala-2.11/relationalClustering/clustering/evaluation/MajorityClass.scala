package relationalClustering.clustering.evaluation

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
  def validate(clusters: Set[List[String]], labels: LabelsContainer) = {

    clusters.map( clust => new Tuple2(clust, AssignMajorityClass(clust, labels))).foldLeft(Set[(String,String)]())( (acc, cl) => {
      acc ++ cl._1.map( el => new Tuple2(labels.getLabel(el), cl._2))
    }).count( elemCl => elemCl._2 == elemCl._1).toDouble/clusters.map( _.size).sum
  }

  /** Returns the majority class in a cluster
    *
    * @param cluster a list of element names in a cluster
    * @param labels label container
    * */
  protected def AssignMajorityClass(cluster: List[String], labels: LabelsContainer) = {
    val labs = cluster.map( ex => labels.getLabel(ex))
    val counts = labs.distinct.map( lab => labs.count( _ == lab))

    labs.zip(counts).maxBy(_._2)._1
  }


}
