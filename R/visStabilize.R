#' Network visualization stabilize method
#'
#' For use stabilize() method in a shiny app. For full documentation, have a look at \link[pkg:visNetwork]{visDocumentation}.
#' 
#'@param graph : a \code{\link[pkg:visNetwork]{visNetworkProxy}}  object
#'@param iterations : Optional. If wanted, the number of iterations
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
visStabilize <- function(graph, iterations = NULL){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visFit with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  options <- list()
  options$iterations <- iterations
  
  data <- list(id = graph$id, options = options)
  
  graph$session$sendCustomMessage("visShinyStabilize", data)
  
  graph
}

#' Network visualization startSimulation method
#'
#' For use startSimulation() method in a shiny app. For full documentation, have a look at \link[pkg:visNetwork]{visDocumentation}.
#' 
#'@param graph : a \code{\link[pkg:visNetwork]{visNetworkProxy}}  object
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

visStartSimulation <- function(graph){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visFit with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id)
  
  graph$session$sendCustomMessage("StartSimulation", data)
  
  graph
}

#' Network visualization stopSimulation method
#'
#' For use stopSimulation() method in a shiny app. For full documentation, have a look at \link[pkg:visNetwork]{visDocumentation}.
#' 
#'@param graph : a \code{\link[pkg:visNetwork]{visNetworkProxy}}  object
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

visStopSimulation <- function(graph){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visFit with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id)
  
  graph$session$sendCustomMessage("StopSimulation", data)
  
  graph
}
