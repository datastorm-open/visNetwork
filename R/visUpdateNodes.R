#' Function to update the information of nodes, with shiny only.
#'
#' Function to update the information of nodes, with shiny only. You can also use this function passing new nodes.
#' The link is based on id.
#' 
#'@param graph : a \code{ \link[visNetwork]{visNetworkProxy}}  object
#'@param nodes : data.frame with the information of nodes. Needed at least column "id". See  \link[visNetwork]{visNodes} 
#' \itemize{
#'  \item "id" id of the node, needed in zhe definition of edges and for update nodes
#'  \item "label" label of the node
#'  \item "group" group of the node. Groups can be configure with  \link[visNetwork]{visGroups}
#'  \item "value" size of the node
#'  \item "title" tooltip of the node
#'  \item ...
#'}
#'@param updateOptions : Boolean. Update options (nodesIdSelection & selectedBy) if needed ? Default to TRUE.
#'@param legend : Boolean. Update nodes on legend ? Default to FALSE
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
visUpdateNodes <- function(graph, nodes, updateOptions = TRUE, legend = FALSE){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visUpdateNodes with visNetwork object. Only within shiny & using visNetworkProxy")
  }

  data <- list(id = graph$id, nodes = nodes, updateOptions = updateOptions, legend = legend)
  
  graph$session$sendCustomMessage("visShinyUpdateNodes", data)

  graph
}
