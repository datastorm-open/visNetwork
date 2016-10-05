#' Function to select node(s) from network, with shiny only.
#'
#' Function to select node(s) from network, with shiny only. 
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
#'@param id : vector of id, node(s) to select
#'@param highlightEdges : Boolean. highlight Edges also ? Default to TRUE
#'@param clickEvent : Boolean. Launch click event ? (highlightNearest for example) Default to TRUE
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
visSelectNodes <- function(graph, id, highlightEdges = TRUE, clickEvent = TRUE){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visSelectNodes with visNetwork object. Only within shiny & using visNetworkProxy")
  }

  stopifnot(is.logical(highlightEdges))
  stopifnot(is.logical(clickEvent))
  
  if(!is.null(id)){
    if(length(id) == 1){
      id <- list(id)
    }
  }
  
  data <- list(id = graph$id, selid = id, highlightEdges = highlightEdges, clickEvent = clickEvent)
  
  graph$session$sendCustomMessage("visShinySelectNodes", data)

  graph
}
