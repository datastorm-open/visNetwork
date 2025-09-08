#' Network visualization groups options
#'
#' Network visualization groups options. For full documentation, have a look at  \link[visNetwork]{visDocumentation}.
#'
#' @param graph : a visNetwork object
#' @param useDefaultGroups : Boolean. Default to true. If your nodes have groups defined that are not in the Groups module, the module loops over the groups it does have, allocating one for each unknown group. When all are used, it goes back to the first group. By setting this to false, the default groups will not be used in this cycle. 
#' @param groupname : String. Name of target group.
#' @param ... :  \link[visNetwork]{visNodes}. You can add multiple groups containing styling information that applies to a certain subset of groups. All options described in the nodes module that make sense can be used here (you're not going to set the same id or x,y position for a group of nodes)
#'
#' @examples
#'
#' nodes <- data.frame(id = 1:10, label = paste("Label", 1:10), 
#'  group = sample(c("A", "B"), 10, replace = TRUE))
#'  edges <- data.frame(from = c(2,5,10), to = c(1,2,10))
#'
#' visNetwork(nodes, edges) %>%
#'  visLegend() %>%
#'  visGroups(groupname = "A", color = "red", shape = "database") %>%
#'  visGroups(groupname = "B", color = "yellow", shape = "triangle")
#'  
#'@seealso  \link[visNetwork]{visNodes} for nodes options,  \link[visNetwork]{visEdges} for edges options,  \link[visNetwork]{visGroups} for groups options, 
#' \link[visNetwork]{visLegend} for adding legend,  \link[visNetwork]{visOptions} for custom option,  \link[visNetwork]{visLayout} &  \link[visNetwork]{visHierarchicalLayout} for layout, 
#' \link[visNetwork]{visPhysics} for control physics,  \link[visNetwork]{visInteraction} for interaction,  \link[visNetwork]{visNetworkProxy} &  \link[visNetwork]{visFocus} &  \link[visNetwork]{visFit} for animation within shiny,
#' \link[visNetwork]{visDocumentation},  \link[visNetwork]{visEvents},  \link[visNetwork]{visConfigure} ...
#'
#' @export
#'
#' @references See online documentation \url{https://datastorm-open.github.io/visNetwork/}
visGroups <- function(graph,
                      useDefaultGroups = TRUE,
                      groupname = NULL,
                      ...){

  if(!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))){
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  
  groups <- list()
  groups$useDefaultGroups <- useDefaultGroups

  params <- list(...)
  
  if(length(params) > 0){
    if(is.null(groupname)){
      stop("Must have a groupname to identify group")
    }
    tmp <- list(...)
    if("icon" %in% names(tmp)){
      if(!"color" %in% names(tmp$icon)){
        tmp$icon$color <- '#2B7CE9'
      }
    }
    
    if("scaling" %in% names(tmp)){
      if("customScalingFunction"%in%names(tmp$scaling)){
        tmp$scaling$customScalingFunction <- JS(tmp$scaling$customScalingFunction)
      }
    }
    
    groups[[groupname]] <- tmp
  }
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    options <- list(groups = groups)
    data <- list(id = graph$id, options = options)
    graph$session$sendCustomMessage("visShinyOptions",data)
  }else{
    graph$x$options$groups <- mergeLists(graph$x$options$groups, groups)
  }
  graph
}
