#' Network visualization unselectAll method
#'
#' For use unselectAll() method in a shiny app. For full documentation, have a look at \link{visDocumentation}.
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
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
visUnselectAll <- function(graph){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visUnselectAll with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id)
  
  graph$session$sendCustomMessage("visShinyUnselectAll", data)

  graph
}
