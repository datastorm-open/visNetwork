#' Function to update the information of edges, with shiny only.
#'
#' Function to update the information of edges, with shiny only. You can also use this function passing new edges.
#' The link is based on id.
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
#'@param edges : data.frame with the information of edges. See \link{visEdges}
#' \itemize{
#'  \item{"id"}{ : edge id, for update}
#'  \item{"from"}{ : node id, begin of the edge}
#'  \item{"to"}{ : node id, end of the edge}
#'  \item{"label"}{ : label}
#'  \item{"value"}{ : size}
#'  \item{"title"}{ : tooltip}
#'  \item{...}{}
#'}
#'@param legend : Boolean. Update edges on legend ? Default to FALSE
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
visUpdateEdges <- function(graph, edges, legend = FALSE){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visUpdateEdges with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id, edges = edges, legend = legend)
  
  graph$session$sendCustomMessage("visShinyUpdateEdges", data)

  graph
}
