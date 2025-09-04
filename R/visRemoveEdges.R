#' Function to remove edges from network, with shiny only.
#'
#' Function to remove edges from network, with shiny only. 
#' 
#'@param graph : a \code{\link[pkg:visNetwork]{visNetworkProxy}}  object
#'@param id : vector of id, edges to remove
#'@param legend : Boolean. Remove edges on legend ? Default to FALSE
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
