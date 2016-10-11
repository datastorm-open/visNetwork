#' Function to nearest nodes of a target node, with shiny only.
#'
#' Function to nearest nodes of a target node, with shiny only.
#' 
#' @param graph : a \code{\link{visNetworkProxy}}  object
#' @param target : name of shiny input returning target node id
#' @param maxpoints : Number of nearest nodes. Default to 5
#' @param addDist : If TRUE, add a column named dist_ that contains the distance from the coordinate to the point, in pixels.
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
visNearestNodes <- function(graph, target, maxpoints = 5, addDist = T){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetNodes with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  force(target)
  
  visNetworkProxy(graph$id) %>%
    visGetNodes(input = "tmp_nearest_nodes")
  
  current_nodes <- graph$session$input$tmp_nearest_nodes
  if(is.list(current_nodes)){
    current_nodes <- do.call("rbind.data.frame", current_nodes)
  } else{
    current_nodes <- NULL
  }
  
  res <- NULL
  if(!is.null(current_nodes)){
    target_row <- which(current_nodes$id %in% target)
    if(length(target_row) > 0){
      res_dist <- do.call("rbind.data.frame", lapply(setdiff(1:nrow(current_nodes), target_row), function(x){
        dist <- sqrt((current_nodes[x, "x"] - current_nodes[target_row, "x"])^2 + 
                       (current_nodes[x, "y"] - current_nodes[target_row, "y"])^2)
        
        data.frame(id = current_nodes[x, "id"], dist_ = dist)
      }))
      
      res <- merge(current_nodes, res_dist, by = "id", all.x = F, all.y = T)
      res <- res[order(res$dist_), ]
      res <- res[1:min(maxpoints, nrow(res)), ]
      if(!addDist){
        res$dist_ <- NULL
      }
    }
    
  }
  res
}