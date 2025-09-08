#' Network visualization collapse / uncollapsed method
#'
#'@param graph : a \code{ \link[visNetwork]{visNetworkProxy}}  object
#'@param nodes : a vector of nodes id. NULL for \code{visUncollapse} for open all collapsed nodes
#'@param fit : Optional. Boolean. Default to FALSE. Call fit method after collapse/uncollapse event ?
#'@param resetHighlight : Optional. Boolean. Default to TRUE to reset highlighted nodes after collapse/uncollapse event.
#'@param keepCoord : Optional. Boolean. Default to TRUE to keep nodes coordinates on collapse
#'@param clusterOptions : Optional. List. Default to NULL. A list of all options you want to pass to cluster collapsed node
#'@param labelSuffix : Optional. Character. Use node label + suffix or just suffix. Default to '(cluster)'
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
#'
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'# You can also disable / enabled the double-click event opening cluster
#'visNetworkProxy("network_id") %>% visEvents(type = "off", doubleClick = "networkOpenCluster")
#'visNetworkProxy("network_id") %>% visEvents(type = "on", doubleClick = "networkOpenCluster")
#'
#'}
#'
#'@export
#'@references See online documentation \url{https://datastorm-open.github.io/visNetwork/}
#'@name visNetwork-collapse
#' 
visCollapse <- function(graph, nodes, fit = FALSE, resetHighlight = TRUE, 
                        clusterOptions = NULL, labelSuffix = "(cluster)"){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visCollapse with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  if(length(nodes) == 1){
    nodes <- list(nodes)
  }
  
  data <- list(id = graph$id, nodes = nodes, fit = fit, 
               resetHighlight = resetHighlight, labelSuffix = labelSuffix)
  data$clusterOptions <- clusterOptions
  
  graph$session$sendCustomMessage("visShinyCollapse", data)

  graph
}

#' @name visNetwork-collapse
#'
#' @export  
visUncollapse <- function(graph, nodes = NULL, fit = FALSE, resetHighlight = TRUE, keepCoord = TRUE){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visUncollapse with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id, fit = fit, resetHighlight = resetHighlight, keepCoord = keepCoord)
  
  if(!is.null(nodes)){
    if(length(nodes) == 1){
      nodes <- list(nodes)
    }
  }
  data$nodes <- nodes
  
  graph$session$sendCustomMessage("visShinyUncollapse", data)
  
  graph
}
