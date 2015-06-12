#' Network visualization Hierarchical layout options
#'
#' Network visualization Hierarchical layout options. For full documentation, have a look at \link{visDocumentation}.
#' 
#'@param graph : a visNetwork object 
#'@param enabled : Boolean. Default to TRUE when calling this function.	Enable or disable the hierarchical layout.
#'@param levelSeparation : Number. Default to 150.  The distance between the different levels.
#'@param direction : String. Default to 'UD'. The direction of the hierarchical layout. The available options are: UD, DU, LR, RL. To simplify: up-down, down-up, left-right, right-left.
#'@param sortMethod : String. Default to 'hubsize'.  The algorithm used to ascertain the levels of the nodes based on the data. The possible options are: hubsize, directed.
#'
#'@examples
#'
#' nodes <- data.frame(id = 1:10)
#' edges <- data.frame(from = round(runif(8)*10), to = round(runif(8)*10))
#'
#' visNetwork(nodes, edges) %>%
#'  visHierarchicalLayout()
#'
#' visNetwork(nodes, edges) %>%
#'  visHierarchicalLayout(direction = "LR")
#'  
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLayout} & \link{visHierarchicalLayout} for layout, \link{visPhysics} for physics, \link{visInteraction} for interaction, ...
#'
#'
#'@export

visHierarchicalLayout <- function(graph,
                                  enabled = TRUE,
                                  levelSeparation = NULL,
                                  direction = NULL,
                                  sortMethod = NULL){

  hierarchicalLayout <- list()

  hierarchicalLayout$enabled <- enabled
  hierarchicalLayout$levelSeparation <- levelSeparation
  hierarchicalLayout$direction <- direction
  hierarchicalLayout$sortMethod <- sortMethod

  graph$x$options$layout$hierarchical <- hierarchicalLayout
  
  graph
}
