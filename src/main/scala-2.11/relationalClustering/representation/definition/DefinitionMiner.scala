package relationalClustering.representation.definition

import relationalClustering.aggregators.AvgAggregator
import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.clustering.{Cluster, Clustering}

/**
  * Created by seb on 20.03.17.
  */
class DefinitionMiner(clustering: Clustering) {

  def getDefinitions(weights: List[Double]): Map[String, List[VertexClusterDefinition]] = {
    clustering.getClusters.foldLeft(Map[String, List[VertexClusterDefinition]]())((acc, clust) => {
      var definitions: List[VertexClusterDefinition] = List()
      if (weights.head > 0.0) {
        definitions = definitions :+ getAttributeDefinition(clust)
      }
      if (weights(1) > 0.0) {
        definitions = definitions :+ getNeighbourhoodAttributeDefinition(clust)
      }
      if (weights(3) > 0.0) {
        definitions = definitions :+ getIdentitiesDefinition(clust)
      }
      if (weights(4) > 0.0) {
        definitions = definitions :+ getEdgeDistributionDefinition(clust)
      }
      acc + (clust.getClusterName -> definitions)
    })
  }

  def getAttributeDefinition(cluster: Cluster): VertexClusterDefinition = {
    if (cluster.getTypes.length == 1) {
      new VertexClusterDefinition(mineRootAttributes(cluster.getInstances.toList.map(x => cluster.getInstanceNeighbourhoodTree(x).head), 0))
    }
    else {
      val instances = cluster.getInstances.map(x => cluster.getInstanceNeighbourhoodTree(x))
      val dimension = instances.head.length
      new EdgeClusterDefinition((0 until dimension).foldLeft(List[TupleContext]())((acc, dim) => {
        acc ++ mineRootAttributes(instances.map(_ (dim)).toList, dim)
      }))
    }
  }

  def mineRootAttributes(instances: List[NeighbourhoodGraph], dimension: Int): List[TupleContext] = {
    val discreteAttrs = instances.map(_.getRootAttributes.toList).filter(_.nonEmpty)
    val numericAttrs = instances.map(_.getRootNumericAttributes.toList).filter(_.nonEmpty)

    List(new TupleCounts(instances.length, discreteAttrs, 0, instances.head.getRootDomain, "rootAttributes", dimension),
      new TupleSummary(instances.length, numericAttrs, 0, instances.head.getRootDomain, "rootAttributes", dimension))
  }

  def getNeighbourhoodAttributeDefinition(cluster: Cluster): VertexClusterDefinition = {
    if (cluster.getTypes.length == 1) {
      new VertexClusterDefinition(mineNeighbourhoodAttributes(cluster.getInstances.toList.map(x => cluster.getInstanceNeighbourhoodTree(x).head), 0))
    }
    else {
      val instances = cluster.getInstances.map(x => cluster.getInstanceNeighbourhoodTree(x))
      val dimension = instances.head.length
      new EdgeClusterDefinition((0 until dimension).foldLeft(List[TupleContext]())((acc, dim) => {
        acc ++ mineNeighbourhoodAttributes(instances.map(_ (dim)).toList, dim)
      }))
    }
  }

  def mineNeighbourhoodAttributes(instances: List[NeighbourhoodGraph], dimension: Int): List[TupleContext] = {
    val depth = instances.head.getMaxDepth

    (0 to depth).foldLeft(List[TupleContext]())((acc, level) => {
      val discreteAttributes = instances.map(_.getAttributeValueDistribution.getOrElse(level, Map[String, List[(String, String)]]()))
      val numericAttributes = instances.map(_.aggregateNumericAttributes(new AvgAggregator).getOrElse(level, Map[String, List[(String, Double)]]()))

      val disc = discreteAttributes.foldLeft(Set[String]())((acc_i, inst) => acc_i ++ inst.keySet).foldLeft(List[TupleContext]())((acc_i, dom) => {
        val discAttrs = discreteAttributes.map(_.getOrElse(dom, List())).filter(_.nonEmpty)
        if (discAttrs.isEmpty) {
          acc_i
        }
        else {
          acc_i :+ new TupleCounts(instances.length, discAttrs, level, dom, "neighbourhoodAttrs", dimension)
        }
      })

      val num = numericAttributes.foldLeft(Set[String]())((acc_i, inst) => acc_i ++ inst.keySet).foldLeft(List[TupleContext]())((acc_i, dom) => {
        val numAttrs = numericAttributes.map(_.getOrElse(dom, List())).filter(_.nonEmpty)
        if (numAttrs.isEmpty) {
          acc_i
        }
        else {
          acc_i :+ new TupleSummary(instances.length, numAttrs, level, dom, "neighbourhoodAttrs", dimension)
        }
      })

      acc ++ disc ++ num
    })
  }

  def getEdgeDistributionDefinition(cluster: Cluster): VertexClusterDefinition = {
    if (cluster.getTypes.length == 1) {
      new VertexClusterDefinition(mineEdgeDistributions(cluster.getInstances.toList.map(x => cluster.getInstanceNeighbourhoodTree(x).head), 0))
    }
    else {
      val instances = cluster.getInstances.map(x => cluster.getInstanceNeighbourhoodTree(x))
      val dimension = instances.head.length
      new EdgeClusterDefinition((0 until dimension).foldLeft(List[TupleContext]())((acc, dim) => {
        acc ++ mineEdgeDistributions(instances.map(_ (dim)).toList, dim)
      }))
    }
  }

  def mineEdgeDistributions(instances: List[NeighbourhoodGraph], dimension: Int): List[TupleContext] = {
    val depth = instances.head.getMaxDepth

    (0 to depth).foldLeft(List[TupleContext]())((acc, level) => {
      val edges = instances.map(inst => {
        val allEdges = inst.getEdgeDistribution(level)
        allEdges.distinct.map(ed => (ed, allEdges.count(_ == ed).toDouble / allEdges.length))
      })

      acc :+ new TupleSummary(instances.length, edges, level, instances.head.getRootDomain, "edgeDistribution", dimension)
    })
  }

  def getIdentitiesDefinition(cluster: Cluster): VertexClusterDefinition = {
    if (cluster.getTypes.length == 1) {
      new VertexClusterDefinition(mineIdentities(cluster.getInstances.toList.map(x => cluster.getInstanceNeighbourhoodTree(x).head), 0))
    }
    else {
      val instances = cluster.getInstances.map(x => cluster.getInstanceNeighbourhoodTree(x))
      val dimension = instances.head.length
      new EdgeClusterDefinition((0 until dimension).foldLeft(List[TupleContext]())((acc, dim) => {
        acc ++ mineIdentities(instances.map(_ (dim)).toList, dim)
      }))
    }
  }

  def mineIdentities(instances: List[NeighbourhoodGraph], dimension: Int): List[TupleContext] = {
    val depth = instances.head.getMaxDepth

    (0 to depth).foldLeft(List[TupleContext]())((acc, level) => {
      val vertices = instances.map(inst => inst.collectVertexIdentity(level))
      val res = vertices.foldLeft(Set[String]())((acc, ver) => acc ++ ver.keySet).foldLeft(List[TupleContext]())((acc_i, dom) => {
        val allNeighbours = vertices.map(inst => inst.getOrElse(dom, List())).filter(it => it.nonEmpty).map(inst => inst.map(it => (it, "true")))

        acc_i :+ new TupleCounts(instances.length, allNeighbours, level, dom, "neighbourhoodIdentities", dimension)
      })
      acc ++ res
    })
  }
}
