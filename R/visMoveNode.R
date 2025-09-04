#' Network visualization moveNode method
#'
#' For use moveNode() method in a shiny app. For full documentation, have a look at \link[pkg:visNetwork]{visDocumentation}.
#' 
#'@param graph : a \code{\link[pkg:visNetwork]{visNetworkProxy}}  object
#'@param id : a node id
#'@param x : Number. x position, in canvas space
#'@param y : Number. y position, in canvas space
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
visMoveNode <- function(graph, id, x, y){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visMoveNode with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  stopifnot(length(id) == 1)
  
  data <- list(id = graph$id, nodeId = id, x = x, y = y)
  
  graph$session$sendCustomMessage("visShinyMoveNode", data)

  graph
}
