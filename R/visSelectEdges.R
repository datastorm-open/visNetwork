#' Function to select edge(s) from network, with shiny only.
#'
#' Function to select edges(s) from network, with shiny only. 
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
#'@param id : vector of id, edges(s) to select
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
visSelectEdges <- function(graph, id){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visSelectEdges with visNetwork object. Only within shiny & using visNetworkProxy")
  }

  if(!is.null(id)){
    if(length(id) == 1){
      id <- list(id)
    }
  }
  
  data <- list(id = graph$id, selid = id)
  
  graph$session$sendCustomMessage("visShinySelectEdges", data)

  graph
}
