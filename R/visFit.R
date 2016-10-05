#' Network visualization fit method
#'
#' For use fit() method in a shiny app. For full documentation, have a look at \link{visDocumentation}.
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
#'@param nodes : NULL for all nodes (Default), or a vector of nodes id
#'@param animation : Optional. List. For animation you can define the duration (in milliseconds) and easing function manually. 
#'Available are: linear, easeInQuad, easeOutQuad, easeInOutQuad, easeInCubic, easeOutCubic, easeInOutCubic, easeInQuart, easeOutQuart, easeInOutQuart, easeInQuint, easeOutQuint, easeInOutQuint.
#'Default to list(duration = 1500, easingFunction = "easeInOutQuad") 
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visFit <- function(graph, nodes = NULL,
                     animation = list(duration = 1500, easingFunction = "easeInOutQuad")){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visFit with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  options <- list(animation = animation)
  
  if(!is.null(nodes)){
    if(length(nodes) == 1){
      nodes <- list(nodes)
    }
  }
  
  options$nodes <- nodes
  
  data <- list(id = graph$id, options = options)
  
  graph$session$sendCustomMessage("visShinyFit", data)
  
  graph
}
