#' Function to remove edges from network, with shiny only.
#'
#' Function to remove edges from network, with shiny only. 
#' 
#'@param graph : a \code{ \link[visNetwork]{visNetworkProxy}}  object
#'@param id : vector of id, edges to remove
#'@param legend : Boolean. Remove edges on legend ? Default to FALSE
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
