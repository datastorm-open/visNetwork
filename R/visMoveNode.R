#' Network visualization moveNode method
#'
#' For use moveNode() method in a shiny app. For full documentation, have a look at \link{visDocumentation}.
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
#'@param id : a node id
#'@param x : Number. x position, in canvas space
#'@param y : Number. y position, in canvas space
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
visMoveNode <- function(graph, id, x, y){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visMoveNode with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  stopifnot(length(id) == 1)
  
  data <- list(id = graph$id, nodeId = id, x = x, y = y)
  
  graph$session$sendCustomMessage("visShinyMoveNode", data)

  graph
}
