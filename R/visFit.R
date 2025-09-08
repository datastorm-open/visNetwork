#' Network visualization fit method
#'
#' For use fit() method in a shiny app. For full documentation, have a look at  \link[visNetwork]{visDocumentation}.
#' 
#'@param graph : a \code{ \link[visNetwork]{visNetworkProxy}}  object
#'@param nodes : NULL for all nodes (Default), or a vector of nodes id
#'@param animation : Optional. List. For animation you can define the duration (in milliseconds) and easing function manually. 
#'Available are: linear, easeInQuad, easeOutQuad, easeInOutQuad, easeInCubic, easeOutCubic, easeInOutCubic, easeInQuart, easeOutQuart, easeInOutQuart, easeInQuint, easeOutQuint, easeInOutQuint.
#'Default to list(duration = 1500, easingFunction = "easeInOutQuad") 
#'
#'@seealso  \link[visNetwork]{visNodes} for nodes options,  \link[visNetwork]{visEdges} for edges options,  \link[visNetwork]{visGroups} for groups options, 
#' \link[visNetwork]{visLegend} for adding legend,  \link[visNetwork]{visOptions} for custom option,  \link[visNetwork]{visLayout} &  \link[visNetwork]{visHierarchicalLayout} for layout, 
#' \link[visNetwork]{visPhysics} for control physics,  \link[visNetwork]{visInteraction} for interaction,  \link[visNetwork]{visNetworkProxy} &  \link[visNetwork]{visFocus} &  \link[visNetwork]{visFit} for animation within shiny,
#' \link[visNetwork]{visDocumentation},  \link[visNetwork]{visEvents},  \link[visNetwork]{visConfigure} ...
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
