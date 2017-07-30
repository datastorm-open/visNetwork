#' Function to update the information of nodes, with shiny only.
#'
#' Function to update the information of nodes, with shiny only. You can also use this function passing new nodes.
#' The link is based on id.
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
#'@param nodes : data.frame with the information of nodes. Needed at least column "id". See \link{visNodes} 
#' \itemize{
#'  \item{"id"}{ : id of the node, needed in the definition of edges and for update nodes}
#'  \item{"label"}{ : label of the node}
#'  \item{"group"}{ : group of the node. Groups can be configure with \link{visGroups}}
#'  \item{"value"}{ : size of the node}
#'  \item{"title"}{ : tooltip of the node}
#'  \item{...}{}
#'}
#'@param updateOptions : Boolean. Update options (nodesIdSelection & selectedBy) if needed ? Default to TRUE.
#'@param legend : Boolean. Update nodes on legend ? Default to FALSE
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
visUpdateNodes <- function(graph, nodes, updateOptions = TRUE, legend = FALSE){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visUpdateNodes with visNetwork object. Only within shiny & using visNetworkProxy")
  }

  data <- list(id = graph$id, nodes = nodes, updateOptions = updateOptions, legend = legend)
  
  graph$session$sendCustomMessage("visShinyUpdateNodes", data)

  graph
}
