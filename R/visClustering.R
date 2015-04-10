#' Network visualization clustering options
#'
#' Network visualization clustering options. See \url{http://visjs.org/docs/network.html#Clustering}.
#'
#' @param initialMaxNodes : Number. Default to 100. If the initial amount of nodes is larger than this value, clustering starts until the total number of nodes is less than this value.
#' @param clusterThreshold : Number. Default to 500. While zooming in and out, clusters can open up. Once there are more than absoluteMaxNumberOfNodes nodes, clustering starts until reduceToMaxNumberOfNodes nodes are left. This is done to ensure performance is continuously fluid.
#' @param reduceToNodes : Number. Default to 300. While zooming in and out, clusters can open up. Once there are more than absoluteMaxNumberOfNodes nodes, clustering starts until reduceToMaxNumberOfNodes nodes are left. This is done to ensure performance is continiously fluid.
#' @param chainThreshold : Number. Default to 0.4. Because of the clustering methods used, long chains of nodes can be formed. To reduce these chains, this threshold is used. A chainThreshold of 0.4 means that no more than 40\% of all nodes are allowed to be a chain node (two connections). If there are more, they are clustered together.
#' @param clusterEdgeThreshold : Number. Default to 20. This is the absolute edge length threshold in pixels. If the edge is smaller on screen (that means zooming out reduces this length) the node will be clustered. This is triggered when zooming out.
#' @param sectorThreshold : Integer. Default to 50. If a cluster larger than sectorThreshold is opened, a seperate instance called a sector, will be created. All the simulation of nodes outside of this instance will be paused. This is to maintain performance and clarity when examining large clusters. A sector is collapsed when zooming out far enough. Also, when opening a cluster, if this cluster is smaller than this value, it is fully unpacked.
#' @param screenSizeThreshold : Number. Default to 0.2. When zooming in, the clusters become bigger. A screenSizeThreshold of 0.2 means that if the width or height of this cluster becomes bigger than 20\% of the width or height of the canvas, the cluster is opened. If a sector has been created, if the sector is smaller than 20\%, we collapse this sector.
#' @param fontSizeMultiplier : Number. Default to 4.0. This parameter denotes the increase in fontSize of the cluster when a single node is added to it.
#' @param maxFontSize : Number. Default to 1000. This parameter denotes the largest allowed font size. If the font becomes too large, some browsers experience problems displaying this.
#' @param forceAmplification : Number. Default to 0.6. This factor is used to calculate the increase of the repulsive force of a cluster. It is calculated by the following formula: repulsingForce *= 1 + (clusterSize * forceAmplification).
#' @param distanceAmplification : Number. Default to 0.2. This factor is used to calculate the increase in effective range of the repulsive force of the cluster. A larger cluster has a longer range. It is calculated by the following formula: minDistance *= 1 + (clusterSize * distanceAmplification).
#' @param edgeGrowth : Number. Default to 20. This factor determines the elongation of edges connected to a cluster.
#' @param nodeScaling : named List with "width", "height" and/or "radius". This factor determines how much the parameters of a cluster increases in pixels per added node.
#' @param maxNodeSizeIncrements : Number. Default to 600. This limits the size clusters can grow to. The default value, 600, implies that if a cluster contains more than 600 nodes, it will no longer grow.
#' @param activeAreaBoxSize : Number. Default to 100. Imagine a square with an edge length of activeAreaBoxSize pixels around your cursor. If a cluster is in this box as you zoom in, the cluster can be opened in a seperate sector. This is regardless of the zoom level.
#' @param clusterLevelDifference : Number. Default to 2. At every clustering session, Network will check if the difference between cluster levels is acceptable. When a cluster is formed when zooming out, that is one cluster level. If you zoom out further and it encompasses more nodes, that is another level. For example: If the highest level of your network at any given time is 3, nodes that have not clustered or have clustered only once will join their neighbour with the lowest cluster level.
#' @param clusterByZoom : Boolean. Default to true. You can toggle the clustering by zoom level using this option.
#'
#' @examples
#'
#' nodes <- data.frame(id = 1:100)
#' edges <- data.frame(from = round(runif(120)*100), to = round(runif(120)*100))
#'
#' visNetwork(nodes, edges) %>%
#'  visClustering(initialMaxNodes = 50, nodeScaling = list(width = 50, height = 50, radius = 50))
#
#'
#' @export
visClustering <- function(graph,
                          initialMaxNodes = NULL,
                          clusterThreshold = NULL,
                          reduceToNodes = NULL,
                          chainThreshold = NULL,
                          clusterEdgeThreshold = NULL,
                          sectorThreshold = NULL,
                          screenSizeThreshold = NULL,
                          fontSizeMultiplier = NULL,
                          maxFontSize = NULL,
                          forceAmplification = NULL,
                          distanceAmplification = NULL,
                          edgeGrowth = NULL,
                          nodeScaling = NULL,
                          maxNodeSizeIncrements  = NULL,
                          activeAreaBoxSize = NULL,
                          clusterLevelDifference = NULL,
                          clusterByZoom = NULL){

  clustering <- list()

  clustering$initialMaxNodes <- initialMaxNodes
  clustering$clusterThreshold <- clusterThreshold
  clustering$reduceToNodes <- reduceToNodes
  clustering$chainThreshold <- chainThreshold
  clustering$clusterEdgeThreshold <- clusterEdgeThreshold
  clustering$sectorThreshold <- sectorThreshold
  clustering$screenSizeThreshold <- screenSizeThreshold
  clustering$fontSizeMultiplier <- fontSizeMultiplier
  clustering$maxFontSize <- maxFontSize
  clustering$forceAmplification <- forceAmplification
  clustering$distanceAmplification <- distanceAmplification
  clustering$edgeGrowth <- edgeGrowth
  clustering$nodeScaling <- nodeScaling
  clustering$maxNodeSizeIncrements  <- maxNodeSizeIncrements
  clustering$activeAreaBoxSize <- activeAreaBoxSize
  clustering$clusterLevelDifference <- clusterLevelDifference
  clustering$clusterByZoom <- clusterByZoom

  graph$x$options$clustering <- clustering

  graph

}
