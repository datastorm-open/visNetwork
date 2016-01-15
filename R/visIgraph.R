#' Render a visNetwork object from an igraph object
#'
#' Render a visNetwork object from an igraph object. This function call also \link{visIgraphLayout}. 
#' We actually try to keep color, size and label from igraph to visNetwork
#'
#'@param igraph : a igraph object
#'@param idToLabel : Boolean. Default to TRUE. Use nodes's id as label ?
#'@param layout : Character Name of igraph layout function to use. Default to "layout_nicely"
#'@param physics : Boolean. Default to FALSE. Enabled physics on nodes ?
#'@param smooth : Boolean. Default to FALSE. Use smooth edges ?
#'@param type : Character Type of scale from igrah to vis.js. "square" (defaut) render in a square limit by height. "full" use width and height to scale in a rectangle.
#'
#'@examples
#'
#'\dontrun{
#'igraph_network <- graph.famous("Walther")
#'visIgraph(igraph_network)
#'
#'# change layout
#'visIgraph(igraph_network, layout = "layout_in_circle")
#'
#'# options
#'visIgraph(igraph_network, layout = "layout_in_circle", 
#'  physics = FALSE, smooth = TRUE)
#'  
#'# passing some info  
#'g <- graph.star(8)
#'V(g)$color <- c("green", "grey")
#'V(g)$size <- 1:8 *5
#'V(g)$label <- LETTERS[1:8]
#'visIgraph(g, layout = "layout.circle", idToLabel = FALSE)  
#'
#'g <- graph.full(5)
#'E(g)$weight <- runif(ecount(g))
#'E(g)$width <- 1
#'E(g)$color <- "red"
#'E(g)[ weight < 0.5 ]$width <- 4
#'E(g)[ weight < 0.5 ]$color <- "green"
#'visIgraph(g)
#'
#'# color vertices of the largest component
#'largest_comp <- function(graph) {
#'  cl <- components(graph)
#'  V(graph)[which.max(cl$csize) == cl$membership]
#'}
#'g <- sample_(gnp(100, 2/100),
#'            with_vertex_(size = 3, label = ""),
#'             with_graph_(layout = layout_with_fr)
#')
#'giant_v <- largest_comp(g)
#'V(g)$color <- "blue"
#'V(g)[giant_v]$color <- "orange"
#'plot(g)
#'visIgraph(g)
#'}
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'
#'@export

visIgraph <- function(igraph,
                      idToLabel = TRUE,
                      layout = "layout_nicely",
                      physics = FALSE, 
                      smooth = FALSE,
                      type = "square"){
  
  if(!any(class(igraph) %in% "igraph")){
    stop("igraph must be a igraph object")
  }
  
  if(!type %in% c("square", "full")){
    stop("type must be one of 'square' or 'full'")
  }
  
  if(!requireNamespace("igraph", quietly = TRUE)){
    stop("This function need 'igraph'. Please 
         install it before.")
  }
  
  if(length(findFunction(layout)) == 0){
    stop("Can't find '", layout, "' function. Please verify it")
  }
  
  igraphdata <- igraph::get.data.frame(igraph, what = "both")
  
  nodes <- igraphdata$vertices
  if(!"name" %in% colnames(nodes)){
    nodes$id <- 1:nrow(nodes)
  }else{
    colnames(nodes) <- gsub("^name$", "id", colnames(nodes))
  }
  
  if("color" %in% colnames(nodes)){
    if(class(nodes$color) %in% c("numeric", "integer")){
      colnames(nodes) <- gsub("^color$", "group", colnames(nodes))
    }
  }
  
  nodes <- nodes[, c("id", setdiff(colnames(nodes), "id")), drop = FALSE]
  
  if(idToLabel){
    nodes$label <- nodes$id
  }
  
  edges <- igraphdata$edges

  directed <- FALSE
  if(igraph::is.directed(igraph)){
#     if(any(duplicated(edges[, c("from", "to")]))){
#       
#     }else{
      directed <- TRUE
    # }
  }
  
  graph <- visNetwork(nodes = nodes, edges = edges) %>%
    visIgraphLayout(layout = layout, type = type, physics = physics, smooth = smooth)
  if(directed){
    graph <- visEdges(graph, arrows = "to")
  }
  graph
}