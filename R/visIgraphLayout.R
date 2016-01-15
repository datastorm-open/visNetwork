#' Use a igraph layout for compute coordinates & fast rendering
#'
#' Use a igraph layout for compute coordinates and fast rendering. 
#' This function affect x and y coordinates to nodes data.frame using a igraph layout, 
#' and then render network faster with no stabilization. 
#' We set some options as : visNodes(physics = FALSE) &
#' visEdges(smooth = FALSE) & visPhysics(stabilization= FALSE), but you can overwrite
#' them using arguments or by add another call after visIgraphLayout
#'
#'@param graph : a visNetwork object
#'@param layout : Character Name of igraph layout function to use. Default to "layout_nicely"
#'@param physics : Boolean. Default to FALSE. Enabled physics on nodes ?
#'@param smooth : Boolean. Default to FALSE. Use smooth edges ?
#'@param type : Character Type of scale from igrah to vis.js. "square" (defaut) render in a square limit by height. "full" use width and height to scale in a rectangle.
#'
#'@examples
#'
#'\dontrun{
#'nnodes <- 200
#'nnedges <- 400
#'
#'nodes <- data.frame(id = 1:nnodes)
#'edges <- data.frame(from = sample(1:nnodes, nnedges, replace = T), 
#'                    to = sample(1:nnodes, nnedges, replace = T))
#'
#'# with defaut layout
#'visNetwork(nodes, edges) %>% 
#'  visIgraphLayout() %>%
#'  visNodes(size = 10)
#'
#'# use full space
#'visNetwork(nodes, edges) %>% 
#'  visIgraphLayout(type = "full") %>%
#'  visNodes(size = 10)
#'
#'# in circle ?
#'visNetwork(nodes, edges) %>% 
#'  visIgraphLayout(layout = "layout_in_circle") %>%
#'  visNodes(size = 10) %>%
#'  visOptions(highlightNearest = T, nodesIdSelection = T)
#'  
#'# keep physics with smooth curves ?
#'visNetwork(nodes, edges) %>% 
#'  visIgraphLayout(physics = TRUE, smooth = TRUE) %>%
#'  visNodes(size = 10)
#'}
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'
#'@export

visIgraphLayout <- function(graph,
                            layout = "layout_nicely",
                            physics = FALSE, 
                            smooth = FALSE,
                            type = "square"){
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visClusteringOutliers with visNetworkProxy object")
  }
  
  if(!any(class(graph) %in% "visNetwork")){
    stop("graph must be a visNetwork object")
  }
  
  if(!all(c("nodes", "edges") %in% names(graph$x))){
    stop("Need 'nodes' and 'edges' informations on network")
  }
  
  if(!type %in% c("square", "full")){
    stop("type must be one of 'square' or 'full'")
  }
  
  if(!requireNamespace("igraph", quietly = TRUE)){
    stop("This function need 'igraph' package to compute layout. Please 
         install it before.")
  }
  
  ctrl <- getAnywhere(layout)
  if(length(ctrl$objs) == 0){
    stop("Can't find '", layout, "' function. Please verify it")
  }
  
  igraphlayout <- list(type = type)
  
  ig <- igraph::graph_from_data_frame(graph$x$edges, directed = TRUE, 
                                      vertices = graph$x$nodes)
  coord <- ctrl$objs[[1]](ig)
  
  graph$x$nodes$x <- coord[, 1]
  graph$x$nodes$y <- coord[, 2]
  
  to <- c(-2, 2)
  from <- range(graph$x$nodes$x, na.rm = TRUE, finite = TRUE)
  graph$x$nodes$x <- (graph$x$nodes$x - from[1])/diff(from) * diff(to) + to[1]
  
  from <- range(graph$x$nodes$y, na.rm = TRUE, finite = TRUE)
  graph$x$nodes$y <- (graph$x$nodes$y - from[1])/diff(from) * diff(to) + to[1]
  
  graph$x$igraphlayout <- igraphlayout
  
  graph %>% visNodes(physics = physics) %>% 
    visEdges(smooth = smooth) %>% visPhysics(stabilization = FALSE)
}
