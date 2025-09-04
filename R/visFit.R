#' Network visualization fit method
#'
#' For use fit() method in a shiny app. For full documentation, have a look at \link[pkg:visNetwork]{visDocumentation}.
#' 
#'@param graph : a \code{\link[pkg:visNetwork]{visNetworkProxy}}  object
#'@param nodes : NULL for all nodes (Default), or a vector of nodes id
#'@param animation : Optional. List. For animation you can define the duration (in milliseconds) and easing function manually. 
#'Available are: linear, easeInQuad, easeOutQuad, easeInOutQuad, easeInCubic, easeOutCubic, easeInOutCubic, easeInQuart, easeOutQuart, easeInOutQuart, easeInQuint, easeOutQuint, easeInOutQuint.
#'Default to list(duration = 1500, easingFunction = "easeInOutQuad") 
#'
#'@seealso \link[pkg:visNetwork]{visNodes} for nodes options, \link[pkg:visNetwork]{visEdges} for edges options, \link[pkg:visNetwork]{visGroups} for groups options, 
#'\link[pkg:visNetwork]{visLegend} for adding legend, \link[pkg:visNetwork]{visOptions} for custom option, \link[pkg:visNetwork]{visLayout} & \link[pkg:visNetwork]{visHierarchicalLayout} for layout, 
#'\link[pkg:visNetwork]{visPhysics} for control physics, \link[pkg:visNetwork]{visInteraction} for interaction, \link[pkg:visNetwork]{visNetworkProxy} & \link[pkg:visNetwork]{visFocus} & \link[pkg:visNetwork]{visFit} for animation within shiny,
#'\link[pkg:visNetwork]{visDocumentation}, \link[pkg:visNetwork]{visEvents}, \link[pkg:visNetwork]{visConfigure} ...
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
#'@references See online documentation \url{https://datastorm-open.github.io/visNetwork/}
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
