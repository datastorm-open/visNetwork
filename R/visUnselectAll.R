#' Network visualization unselectAll method
#'
#' For use unselectAll() method in a shiny app. For full documentation, have a look at  \link[visNetwork]{visDocumentation}.
#' 
#'@param graph : a \code{ \link[visNetwork]{visNetworkProxy}}  object
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
visUnselectAll <- function(graph){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visUnselectAll with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id)
  
  graph$session$sendCustomMessage("visShinyUnselectAll", data)

  graph
}
