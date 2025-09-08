#' Function to select edge(s) / node(s) from network, with shiny only.
#'
#' Function to select edge(s) / node(s) from network, with shiny only. 
#' 
#'@param graph : a \code{ \link[visNetwork]{visNetworkProxy}}  object
#'@param nodesId : vector of id, nodes(s) to select
#'@param edgesId : vector of id, edges(s) to select
#'@param unselectAll : Boolean. Unselect all nodes & edges before current selection ? Default to TRUE
#'@param highlightEdges : Boolean. highlight Edges also ? Default to TRUE
#'@param clickEvent : Boolean. Launch click event ? (highlightNearest for example) Default to TRUE
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
visSetSelection <- function(graph, nodesId = NULL, edgesId = NULL, unselectAll = TRUE, 
                            highlightEdges = TRUE, clickEvent = TRUE){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visSetSelection with visNetwork object. Only within shiny & using visNetworkProxy")
  }

  if(!is.null(nodesId)){
    if(length(nodesId) == 1){
      nodesId <- list(nodesId)
    }
  }
  
  if(!is.null(edgesId)){
    if(length(edgesId) == 1){
      edgesId <- list(edgesId)
    }
  }
  
  data <- list(id = graph$id, selection = list(nodes = nodesId, edges = edgesId), 
               options = list(unselectAll = unselectAll, highlightEdges = highlightEdges), clickEvent = clickEvent)
  
  graph$session$sendCustomMessage("visShinySetSelection", data)

  graph
}
