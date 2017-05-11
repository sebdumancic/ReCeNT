# Relational clustering over neighbourhood trees

A versatile relational clustering toolbox. It contains several clustering algorithms, check the original paper for details (see below). Still under development.

## Installation

+ get [SBT](http://www.scala-sbt.org/)
+ clone this repository
+ position in the root folder and
  + build a jar with dependencies `sbt assembly`
  + build a jar without dependencies `sbt package`

## Usage

Version 3.0

Usage: RelationalClustering.jar [OPTIONS]

OPTIONS

--db filepath                                                       database(s) with data to cluster (May be specified multiple times.)

--declarations file path                                            predicate declarations

--query comma-separated list                                        domains to query

--similarity [RCNT|HS|RIBL|HSAG|CCFonseca]                          similarity measure

--domain filepath                                                   predicate definitions

--aggregates comma-separated list [mean/min/max]                    a list of aggregator functions to use for the numerical attributes

--algorithm [Spectral|Hierarchical|DBscan|Affinity]                 clustering algorithm

--bagCombination [union|intersection]                               multiset combination method

--bagSimilarity [chiSquared|maximum|minimum|union]                  multiset similarity measure

--clauseLength n                                                    (CC and RKOH) maximal length of clause/walk 

--damping d                                                         damping parameter for Affinity Propagation

--definitionsDeviance Double                                        maximum standard deviation for a numeric attribute to be preserved (in % of the mean value)

--definitionsK Int                                                  top K most occurring tuples to select

--depth n                                                           depth of the neighbourhood tree

--eps d                                                             eps value for DBscan

--labels filepath                                                   labels for the query objects (May be specified multiple times.)

--linkage [average|complete|ward]                                   (Hierarchical) linkage

--preference d                                                      (Affinity Propagation) preference parameter

--root filePath                                                     temporary folder to use

--selection [model|saturation]                                      method to choose a single clustering

--selectionValidation [intraCluster|silhouette]                     evaluation criteria for clustering selection

--validationMethod [ARI|RI|intraCluster|majorityClass]              cluster validation method

--vertexCombination [avg|min|max]                                   how to combine the similarities of individual vertices in a hyperedge

--weights Array[Double]                                             
                                                                    comma-separated list of weights [attributes,attribute distribution,connections,vertex neighbourhood,edge distribution]

--exportNTrees                                                      export neighbourhood trees as gspan

--findDefinitions                                                   extract definitions of clusters

-k n                                                                number of clusters to create

--localRepo                                                         use local NodeRepository for all neighbourhood trees

--selectSingle                                                      flag single clustering

--validate                                                          perform clustering validation


## Contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request 

## Citing

Please cite the following paper if you are using the code

```
@article{,
    author =       {Dumancic, Sebastijan and Blockeel, Hendrik},
    title =        {An expressive dissimilarity measure for relational clustering over neighbourhood trees},
    journal =      {Machine Learning journal}
}
```

## License

Release under  Apache License, version 2.