#' Function to remove edges from network, with shiny only.
#'
#' Function to remove edges from network, with shiny only. 
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
#'@param id : vector of id, edges to remove
#'@param legend : Boolean. Remove edges on legend ? Default to FALSE
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
#'@references See online documentation \url{https://datastorm-open.github.io/visNetwork/}
#' 
#'@export
#'@references See online documentation \url{https://datastorm-open.github.io/visNetwork/}
visRemoveEdges <- function(graph, id, legend = FALSE){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visRemoveEdges with visNetwork object. Only within shiny & using visNetworkProxy")
  }

  if(!is.null(id)){
    if(length(id) == 1){
      id <- list(id)
    }
  }
  
  data <- list(id = graph$id, rmid = id, legend = legend)
  
  graph$session$sendCustomMessage("visShinyRemoveEdges", data)

  graph
}
