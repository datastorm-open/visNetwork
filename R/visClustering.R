#' Network visualization clustering options - outliers
#' 
#' Network visualization clustering options - outliers
#' 
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
#'  @export
visClusteringOutliers <- function(graph, clusterFactor = 0.9, stabilize = FALSE){
 
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
#'  visNetwork(nodes, edges, legend = TRUE) %>%
#'    visGroups(groupname = "A", color = "red", shape = "square") %>%
#'    visGroups(groupname = "B", color = "yellow", shape = "triangle") %>%
#'    visClusteringByColor(colors = c("red")) %>%
#'    visClusteringByGroup(groups = c("B"))
#'    
#'  visNetwork(nodes, edges, legend = TRUE) %>%
#'    visGroups(groupname = "A", color = "red", shape = "triangle") %>%
#'    visGroups(groupname = "B", color = "yellow", shape = "triangle") %>%
#'    visClusteringByGroup(groups = c("A","B"))
#' @export
visClusteringByColor <- function(graph, colors){
  
  if(length(colors) == 1){
    colors <- list(colors)
  }
  clusteringColor<- list(colors = colors)
  
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
#' 
#' @examples
#'
#'  
#'  nodes <- data.frame(id = 1:10, label = paste("Label", 1:10), 
#'    group = sample(c("A", "B"), 10, replace = TRUE))
#'  edges <- data.frame(from = c(2,5,10), to = c(1,2,10))
#'
#'  visNetwork(nodes, edges, legend = TRUE) %>%
#'    visGroups(groupname = "A", color = "red", shape = "database") %>%
#'    visGroups(groupname = "B", color = "yellow", shape = "triangle") %>%
#'    visClusteringByGroup(groups = c("B"))
#'  
#' @export
visClusteringByGroup <- function(graph, groups){
  
  if(length(groups) == 1){
    groups <- list(groups)
  }
  clusteringGroup<- list(groups = groups)
  
  graph$x$clusteringGroup <- clusteringGroup
  
  graph
  
}

#' Network visualization clustering options - by node id
#'
#' Network visualization clustering options - by node id
#' 
#' @param graph : a visNetwork object
#' @param nodes : Character/vector. gid of nodes we want to cluster
#' 
#' @examples
#'
#' set.seed(124)
#' nodes <- data.frame(id = 1:10, color = c(rep("blue", 6), rep("red", 3), rep("green", 1)))
#' edges <- data.frame(from = round(runif(6)*10), to = round(runif(6)*10))
#'
#'  visNetwork(nodes, edges, legend = TRUE) %>%
#'    visClusteringByConnection(nodes = 9)
#'      
#' @export
visClusteringByConnection <- function(graph, nodes){
  
  if(length(nodes) == 1){
    nodes <- list(nodes)
  }
  
  clusteringConnection<- list(nodes = nodes)
  
  graph$x$clusteringConnection <- clusteringConnection
  
  graph
  
}