#' Function to update the information of edges, with shiny only.
#'
#' Function to update the information of edges, with shiny only. You can also use this function passing new edges.
#' The link is based on id.
#' 
#'@param graph : a \code{ \link[visNetwork]{visNetworkProxy}}  object
#'@param edges : data.frame with the information of edges. See  \link[visNetwork]{visEdges}
#' \itemize{
#'  \item "id" edge id, for update
#'  \item "from" node id, begin of the edge
#'  \item "to" node id, end of the edge
#'  \item "label" label
#'  \item "value" size
#'  \item "title" tooltip
#'  \item ...
#'}
#'@param legend : Boolean. Update edges on legend ? Default to FALSE
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
#'@export
#'@references See online documentation \url{https://datastorm-open.github.io/visNetwork/}
visUpdateEdges <- function(graph, edges, legend = FALSE){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visUpdateEdges with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id, edges = edges, legend = legend)
  
  graph$session$sendCustomMessage("visShinyUpdateEdges", data)

  graph
}
