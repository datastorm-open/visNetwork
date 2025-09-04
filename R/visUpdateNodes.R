#' Function to update the information of nodes, with shiny only.
#'
#' Function to update the information of nodes, with shiny only. You can also use this function passing new nodes.
#' The link is based on id.
#' 
#'@param graph : a \code{\link[pkg:visNetwork]{visNetworkProxy}}  object
#'@param nodes : data.frame with the information of nodes. Needed at least column "id". See \link[pkg:visNetwork]{visNodes} 
#' \itemize{
#'  \item "id" id of the node, needed in zhe definition of edges and for update nodes
#'  \item "label" label of the node
#'  \item "group" group of the node. Groups can be configure with \link[pkg:visNetwork]{visGroups}
#'  \item "value" size of the node
#'  \item "title" tooltip of the node
#'  \item ...
#'}
#'@param updateOptions : Boolean. Update options (nodesIdSelection & selectedBy) if needed ? Default to TRUE.
#'@param legend : Boolean. Update nodes on legend ? Default to FALSE
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
