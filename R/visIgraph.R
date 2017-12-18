#' Render a visNetwork object from an igraph object
#'
#' Render a visNetwork object from an igraph object. \link{toVisNetworkData} transfrom igraph data to visNetwork data.
#' We actually try to keep color, size and label from igraph to visNetwork.
#' \link{visIgraph} plot directly an igraph object in visNetwork, using \link{toVisNetworkData} to extract data, and
#' \link{visIgraphLayout} to compute layout and coordinates before rendering.
#' 
#'
#'@param igraph : a igraph object
#'@param idToLabel : Boolean. Default to TRUE. Use id of nodes as label ?
#'@param layout : Character Name of igraph layout function to use. Default to "layout_nicely"
#'@param physics : Boolean. Default to FALSE. Enabled physics on nodes ?
#'@param smooth : Boolean. Default to FALSE. Use smooth edges ?
#'@param type : Character Type of scale from igrah to vis.js. "square" (defaut) render in a square limit by height. "full" use width and height to scale in a rectangle.
#'@param randomSeed : Number. The nodes are randomly positioned initially. This means that the settled result is different every time. If you provide a random seed manually, the layout will be the same every time.
#'@param layoutMatrix : in case of layout = 'layout.norm'. the 'layout' argument (A matrix with two or three columns, the layout to normalize)
#'@param ... : Adding arguments to layout function
#'
#'@name visNetwork-igraph
#' 
#'@examples
#'
#'\dontrun{
#'require(igraph)
#'igraph_network <- graph.famous("Walther")
#'
#'# get data and plot :
#'data <- toVisNetworkData(igraph_network)
#'visNetwork(nodes = data$nodes, edges = data$edges)
#'
#'# or plot directly
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
#'V(g)$label.cex = seq(1, 2,length.out = 8)
#'V(g)$label.color = "red"
#'visIgraph(g, layout = "layout.circle", idToLabel = FALSE)  
#'
#'g <- graph.full(5)
#'E(g)$weight <- runif(ecount(g))
#'E(g)$width <- 1
#'E(g)$color <- "red"
#'E(g)[ weight < 0.5 ]$width <- 4
#'E(g)[ weight < 0.5 ]$color <- "green"
#'E(g)$label <- LETTERS[1:10]
#'E(g)$label.cex = seq(1, 2,length.out = 10)
#'E(g)$label.color = "red"
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
#' @importFrom  methods findFunction
#' 
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visIgraph <- function(igraph,
                      idToLabel = TRUE,
                      layout = "layout_nicely",
                      physics = FALSE, 
                      smooth = FALSE,
                      type = "square",
                      randomSeed = NULL, 
                      layoutMatrix = NULL, ...){
  
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
  
  visdata <- toVisNetworkData(igraph, idToLabel)
  
  directed <- FALSE
  if(igraph::is.directed(igraph)){
#     if(any(duplicated(edges[, c("from", "to")]))){
#       
#     }else{
      directed <- TRUE
    # }
  }
  
  graph <- visNetwork(nodes = visdata$nodes, edges = visdata$edges) %>%
    visIgraphLayout(layout = layout, type = type, physics = physics, 
                    smooth = smooth, randomSeed = randomSeed, 
                    layoutMatrix = layoutMatrix, ...)
  if(directed){
    graph <- visEdges(graph, arrows = "to")
  }
  graph
}

#'@rdname visNetwork-igraph
#'@export
toVisNetworkData <- function(igraph,
                             idToLabel = TRUE){
  if(!any(class(igraph) %in% "igraph")){
    stop("igraph must be a igraph object")
  }

  if(!requireNamespace("igraph", quietly = TRUE)){
    stop("This function need 'igraph'. Please 
         install it before.")
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
  
  if("label.cex" %in% colnames(nodes)){
      colnames(nodes) <- gsub("^label.cex$", "font.size", colnames(nodes))
      nodes$font.size <- nodes$font.size*40
  }
  
  if("label.color" %in% colnames(nodes)){
    colnames(nodes) <- gsub("^label.color$", "font.color", colnames(nodes))
  }
  
  nodes <- nodes[, c("id", setdiff(colnames(nodes), "id")), drop = FALSE]
  
  if(idToLabel){
    nodes$label <- nodes$id
  }
  
  edges <- igraphdata$edges
  
  if("label.cex" %in% colnames(edges)){
    colnames(edges) <- gsub("^label.cex$", "font.size", colnames(edges))
    edges$font.size <- edges$font.size*40
  }
  
  if("label.color" %in% colnames(edges)){
    colnames(edges) <- gsub("^label.color$", "font.color", colnames(edges))
  }
  
  list(nodes= nodes, edges = edges)
}