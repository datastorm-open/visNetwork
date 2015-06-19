#' Network visualization clustering options
#'
#' Network visualization clustering options.
#'
#' @examples
#'
#' set.seed(123)
#' nodes <- data.frame(id = 1:100)
#' edges <- data.frame(from = round(runif(60)*100), to = round(runif(60)*100))
#'
#' visNetwork(nodes, edges) %>%
#'  visClusteringOutliers(clusterFactor = 1) %>%
#'  visLayout(randomSeed = 123)
#
#'
#' @export
visClusteringOutliers <- function(graph, clusterFactor = 0.9, stabilize = FALSE){
  
  clusteringOutliers <- list(clusterFactor = clusterFactor, stabilize = stabilize)
  
  graph$x$clusteringOutliers <- mergeLists(graph$x$clusteringOutliers, clusteringOutliers)
  
  graph
  
}

#' Network visualization clustering options - by color
#'
#' Network visualization clustering options - by color.
#'
#' @examples
#'
#' set.seed(123)
#' nodes <- data.frame(id = 1:10, color = c(rep("blue", 6), rep("red", 3), rep("green", 1)))
#' edges <- data.frame(from = round(runif(6)*10), to = round(runif(6)*10))
#'
#' visNetwork(nodes, edges) %>%
#'  visClusteringByColor(colors = c("blue"))
#'  
#'  nodes <- data.frame(id = 1:10, label = paste("Label", 1:10), group = sample(c("A", "B"), 10, replace = TRUE))
#'  edges <- data.frame(from = c(2,5,10), to = c(1,2,10))
#'
#'  visNetwork(nodes, edges, legend = TRUE) %>%
#'    visGroups(groupname = "A", color = "red", shape = "database") %>%
#'    visGroups(groupname = "B", color = "yellow", shape = "triangle") %>%
#'    visClusteringByColor(colors = c("red")) %>%
#'    visClusteringByGroup(groups = c("B"))
#'  
#' @export
visClusteringByColor <- function(graph, colors){
  
  if(length(colors) == 1){
    colors <- list(colors)
  }
  clusteringColor<- list(colors = colors)
  
  graph$x$clusteringColor <- clusteringColor 
  
  graph
  
}

#' Network visualization clustering options - by group
#'
#' Network visualization clustering options - by group.
#'
#' @examples
#'
#'  
#'  nodes <- data.frame(id = 1:10, label = paste("Label", 1:10), group = sample(c("A", "B"), 10, replace = TRUE))
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