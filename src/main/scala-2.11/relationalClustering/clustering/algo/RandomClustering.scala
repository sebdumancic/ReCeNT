package relationalClustering.clustering.algo

import relationalClustering.representation.clustering.{Cluster, Clustering}
import relationalClustering.similarity.AbstractSimilarityNTrees

/**
  * Created by seb on 18.04.17.
  */
class RandomClustering(protected val rootFolder: String) extends AbstractCluster {

  override def clusterVertices(domains: List[String], similarityMeasure: AbstractSimilarityNTrees, k: Int, baseOffset: Int): Clustering = {
    val (filename, objects) = similarityMeasure.getObjectSimilaritySave(domains, rootFolder)

    val r = scala.util.Random
    val clusters = objects.map(elem => (elem, r.nextInt(k))).groupBy(_._2).map(group => {
      new Cluster(domains, s"Cluster_${domains.mkString("")}_${baseOffset}_${group._1}", group._2.map(el => List(el._1._1)).toSet, similarityMeasure.getNeighbourhoodGraphCache)
    })

    new Clustering(clusters.toList, similarityMeasure.copy, objects.map(el => List(el._1)), filename)
  }

  override def clusterEdges(domains: List[String], similarityMeasure: AbstractSimilarityNTrees, k: Int, baseOffset: Int): Clustering = {
    val (filename, objects) = similarityMeasure.getHyperEdgeSimilaritySave(domains, rootFolder)

    val r = scala.util.Random
    val clusters = objects.map(elem => (elem, r.nextInt(k))).groupBy(_._2).map(group => {
      new Cluster(domains, s"Cluster_${domains.mkString("")}_${baseOffset}_${group._1}", group._2.map(el => el._1).toSet, similarityMeasure.getNeighbourhoodGraphCache)
    })

    new Clustering(clusters.toList, similarityMeasure.copy, objects, filename)
  }

}
