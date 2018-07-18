#' Network visualization clustering options - outliers
#' 
#' Network visualization clustering options - outliers
#' 
#' @param graph : a visNetwork object
#' @param clusterFactor : Number, from 0 to 1. 0.9 by default
#' @param stabilize : Boolean, default to false
#' 
#' @examples
#'  
#' nodes <- data.frame(id = 1:10)
#' edges <- data.frame(from = c(1,1,10,2,6,7,8,9,10), 
#'                     to = c(2,3,4,5,2,5,6,7,9))
#' 
#' visNetwork(nodes, edges) %>%
#'  visClusteringOutliers(1)
#' 
#'  
#' @export
visClusteringOutliers <- function(graph, clusterFactor = 0.9, stabilize = FALSE){
 
  if(any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visClusteringOutliers with visNetworkProxy object")
  }
  
  if(!any(class(graph) %in% "visNetwork")){
    stop("graph must be a visNetwork object")
  }
  
 clusteringOutliers <- list(clusterFactor = clusterFactor, stabilize = stabilize)
 
 graph$x$clusteringOutliers <- mergeLists(graph$x$clusteringOutliers, clusteringOutliers)
 
 graph
 
}

#' Network visualization clustering options - by color
#'
#' Network visualization clustering options - by color.
#' 
#' @param graph : a visNetwork object
#' @param colors : Character/vector. colors we want to cluster
#' @param label : Character. Label put before value(s). See example
#' @param shape : Character. Shape of cluster(s) if different shapes between nodes or \code{force = T}. "database" per default
#' @param force : If \code{force = FALSE}, Set shape of nodes if all equal, else directly defaut shape
#' 
#' @examples
#'
#' set.seed(124)
#' nodes <- data.frame(id = 1:10, color = c(rep("blue", 6), rep("red", 3), rep("green", 1)))
#' edges <- data.frame(from = round(runif(6)*10), to = round(runif(6)*10))
#'
#' visNetwork(nodes, edges) %>%
#'  visClusteringByColor(colors = c("blue"))
#'  
#'  nodes <- data.frame(id = 1:10, label = paste("Label", 1:10), 
#'    group = sample(c("A", "B"), 10, replace = TRUE))
#'  edges <- data.frame(from = c(2,5,10), to = c(1,2,10))
#'
#'  visNetwork(nodes, edges) %>%
#'    visGroups(groupname = "A", color = "red", shape = "square") %>%
#'    visGroups(groupname = "B", color = "yellow", shape = "triangle") %>%
#'    visClusteringByColor(colors = c("red"), label = "With color ") %>%
#'    visClusteringByGroup(groups = c("B"), label = "Group : ") %>%
#'    visLegend()
#'    
#'  visNetwork(nodes, edges) %>%
#'    visGroups(groupname = "A", color = "red", shape = "triangle") %>%
#'    visGroups(groupname = "B", color = "yellow", shape = "triangle") %>%
#'    visClusteringByGroup(groups = c("A","B")) %>%
#'    visLegend()
#' @export
visClusteringByColor <- function(graph, colors, label = "Cluster on color : ",
                                 shape = "database", force = FALSE){
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visClusteringByColor with visNetworkProxy object")
  }
  
  if(!any(class(graph) %in% "visNetwork")){
    stop("graph must be a visNetwork object")
  }
  
  shape <- rep(shape, length(colors))[1:length(colors)]
  force <- rep(force, length(colors))[1:length(colors)]
  
  if(length(colors) == 1){
    colors <- list(colors)
  }
  clusteringColor<- list(colors = colors, label = label, shape = shape, force = force)
  
  graph$x$clusteringColor <- clusteringColor 
  
  graph
  
}

#' Network visualization clustering options - by hubsize
#'
#' Network visualization clustering options - by hubsize
#' 
#' @param graph : a visNetwork object
#' @param size : Integer. This method checks all nodes in the network and those with a equal or higher amount of edges than specified with size argument. If size is null (defaut), the size will be determined as the average value plus two standard deviations. 
#' 
#' @examples
#'
#' set.seed(124)
#' nodes <- data.frame(id = 1:10, color = c(rep("blue", 6), rep("red", 3), rep("green", 1)))
#' edges <- data.frame(from = round(runif(6)*10), to = round(runif(6)*10))
#'
#' visNetwork(nodes, edges) %>%
#'  visClusteringByHubsize()
#'  
#' visNetwork(nodes, edges) %>%
#'  visClusteringByHubsize(size = 2)
#'  
#' @export
visClusteringByHubsize <- function(graph, size = NULL){
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visClusteringByHubsize with visNetworkProxy object")
  }
  
  if(!any(class(graph) %in% "visNetwork")){
    stop("graph must be a visNetwork object")
  }
  
  if(is.null(size)){
    clusteringHubsize <- list(size = 0)
  }else{
    clusteringHubsize <- list(size = size)
  }

  graph$x$clusteringHubsize <- clusteringHubsize 
  
  graph
  
}

#' Network visualization clustering options - by group
#'
#' Network visualization clustering options - by group.
#' 
#' @param graph : a visNetwork object
#' @param groups : Character/vector. groups we want to cluster
#' @param label : Character. Label put before value(s). See example
#' @param shape : Character. Shape of cluster(s) if different shapes between nodes or \code{force = T}. "database" per default
#' @param color : Character. Color of cluster(s) if different colors between nodes or \code{force = T}. "grey" per default
#' @param force : If \code{force = FALSE}, Set shape and color of nodes if all equal, else directly defaut shape and color
#' @param scale_size : Set size based on cluster population ? Default to TRUE.
#' 
#' @examples
#'
#'  
#'  nodes <- data.frame(id = 1:10, label = paste("Label", 1:10), 
#'    group = sample(c("A", "B"), 10, replace = TRUE))
#'  edges <- data.frame(from = c(2,5,10), to = c(1,2,10))
#'
#'  visNetwork(nodes, edges) %>%
#'    visGroups(groupname = "A", color = "red", shape = "database") %>%
#'    visGroups(groupname = "B", color = "yellow", shape = "triangle") %>%
#'    visClusteringByGroup(groups = c("B"), label = "Group : ", 
#'      shape = "ellipse", color = "blue", force = TRUE) %>%
#'    visLegend()
#'  
#' @export
visClusteringByGroup <- function(graph, groups, label = "Cluster on group : ", 
                                 shape = "database", color = "grey", 
                                 force = FALSE, scale_size = TRUE){
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visClusteringByGroup with visNetworkProxy object")
  }
  
  if(!any(class(graph) %in% "visNetwork")){
    stop("graph must be a visNetwork object")
  }
  
  color <- rep(color, length(groups))[1:length(groups)]
  shape <- rep(shape, length(groups))[1:length(groups)]
  force <- rep(force, length(groups))[1:length(groups)]
  scale_size <- rep(scale_size, length(groups))[1:length(groups)]
  
  if(length(groups) == 1){
    groups <- list(groups)
  }
  clusteringGroup<- list(groups = groups, label = label, shape = shape, color = color, force = force, scale_size = scale_size)
  
  graph$x$clusteringGroup <- clusteringGroup
  
  graph
  
}

#' Network visualization clustering options - by node id
#'
#' Network visualization clustering options - by node id
#' 
#' @param graph : a visNetwork object
#' @param nodes : Character/vector. id of nodes we want to cluster
#' 
#' @examples
#'
#' set.seed(124)
#' nodes <- data.frame(id = 1:10, color = c(rep("blue", 6), rep("red", 3), rep("green", 1)))
#' edges <- data.frame(from = round(runif(6)*10), to = round(runif(6)*10))
#'
#'  visNetwork(nodes, edges) %>%
#'    visClusteringByConnection(nodes = 9)
#'      
#' @export
visClusteringByConnection <- function(graph, nodes){
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visClusteringByConnection with visNetworkProxy object")
  }
  
  if(!any(class(graph) %in% "visNetwork")){
    stop("graph must be a visNetwork object")
  }
  
  if(length(nodes) == 1){
    nodes <- list(nodes)
  }
  
  clusteringConnection<- list(nodes = nodes)
  
  graph$x$clusteringConnection <- clusteringConnection
  
  graph
  
}