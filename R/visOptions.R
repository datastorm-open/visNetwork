#' Network visualization general options
#'
#' Network visualization general options.
#' 
#'@param graph : a visNetwork object
#'@param width : String. Default to "400px". The width of the network in pixels or as a percentage.
#'@param height : String. Default to "400px". The height of the network in pixels or as a percentage.
#'@param highlightNearest : Custom Option. Boolean. Default to false. Highlight nearest when clicking a node ? Based on \url{http://visjs.org/examples/network/29_neighbourhood_highlight.html}
#' This options use click event. Not available for DOT and Gephi.
#'@param nodesIdSelection :  Custom Option. Boolean. Default to false. A little bit experimental. Add an id node selection. This options use click event. Not available for DOT and Gephi.
#'@param autoResize : Boolean. Default to true. If true, the Network will automatically detect when its container is resized, and redraw itself accordingly. If false, the Network can be forced to repaint after its container has been resized using the function redraw() and setSize(). 
#'@param clickToUse : Boolean. Default to false. When a Network is configured to be clickToUse, it will react to mouse, touch, and keyboard events only when active. When active, a blue shadow border is displayed around the Network. The Network is set active by clicking on it, and is changed to inactive again by clicking outside the Network or by pressing the ESC key.
#'@param manipulation : Just a Boolean
#'@param clustering : Boolean. Default to false. Clustering is turned off by default. Set to true to enabled clustering or see \link{visClustering} to set options..
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLayout} & \link{visHierarchicalLayout} for layout, \link{visPhysics} for physics, \link{visInteraction} for interaction, ...
#'@export

visOptions <- function(graph,
                       width = NULL,
                       height = NULL,
                       highlightNearest = FALSE,
                       nodesIdSelection = FALSE,
                       autoResize = NULL,
                       clickToUse = NULL,
                       manipulation = NULL,
                       clustering = NULL){

  options <- list()

  options$autoResize <- autoResize
  options$clickToUse <- clickToUse

  if(is.null(manipulation)){
    options$manipulation <- list(enabled = FALSE)
  }else{
    options$manipulation <- list(enabled = manipulation)
  }
  
  options$clustering <- clustering

  options$height <- height
  options$width <- width

  if(!is.null(manipulation)){
    if(manipulation){
      graph$x$datacss <- paste(readLines(system.file("htmlwidgets/lib/css/dataManipulation.css", package = "visNetwork"), warn = FALSE), collapse = "\n")
    }
  }

  x <- list(highlight = highlightNearest, idselection = nodesIdSelection)
  
  if(highlightNearest){
    if(!"label"%in%colnames(graph$x$nodes)){
      graph$x$nodes$label <- as.character(graph$x$nodes$id)
    }
    if(!"group"%in%colnames(graph$x$nodes)){
      graph$x$nodes$group <- 1
    }
  }
  
  graph$x <- mergeLists(graph$x, x)
  graph$x$options <- mergeLists(graph$x$options, options)

  graph
}
