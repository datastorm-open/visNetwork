#' Add a legend on a visNetwork object
#'
#' Add a legend on a visNetwork object
#' 
#' @param graph : a visNetwork object
#' @param enabled : Boolean. Default to TRUE.
#' @param useGroups : use groups options in legend ? Default to TRUE.
#' @param addNodes : a data.frame or a list for adding custom node(s)
#' @param addEdges : a data.frame or a list for adding custom edges(s)
#' @param width : Number, in [0,...,1]. Default to 0.2
#' @param position : one of "left" (Default) or "right"
#' @examples
#'
#' # minimal example
#' nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#' edges <- data.frame(from = c(1,2), to = c(2,3))
#' 
#' # default, on group
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "red") %>%
#'   visGroups(groupname = "B", color = "lightblue") %>%
#'   visLegend()
#'   
#' # default, on group, adjust width + change position
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "red") %>%
#'   visGroups(groupname = "B", color = "lightblue") %>%
#'   visLegend(width = 0.05, position = "right")
#'   
#' # passing custom nodes and/or edges
#' lnodes <- data.frame(label = c("Group A", "Group B"), 
#'  shape = c( "ellipse"), color = c("red", "lightblue"),
#'  title = "Informations", id = 1:2) 
#'    
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "red") %>%
#'   visGroups(groupname = "B", color = "lightblue") %>%
#'   visLegend(addNodes = lnodes, useGroups = FALSE)
#'   
#' ledges <- data.frame(color = c("lightblue", "red"), 
#'  label = c("reverse", "depends"), arrows =c("to", "from")) 
#'  
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "lightblue") %>%
#'   visGroups(groupname = "B", color = "red") %>%
#'   visLegend(addEdges = ledges)    
#'   
#' # for more complex option, you can use a list(of list...)
#' nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#' edges <- data.frame(from = c(1,2), to = c(2,3))
#' 
#' visNetwork(nodes, edges) %>%
#'  visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
#'  visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
#'  addFontAwesome() %>%
#'  visLegend(addNodes = list(
#'   list(label = "Group", shape = "icon", icon = list(code = "f0c0", size = 25)),
#'   list(label = "User", shape = "icon", icon = list(code = "f007", size = 50, color = "red"))
#'  ),
#'  addEdges = data.frame(label = "link"), useGroups = FALSE)   
#'  
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'
#'
#' @import htmlwidgets
#'
#' @export
#' 
visLegend <- function(graph,
                      enabled = TRUE,
                      useGroups = TRUE,
                      addNodes = NULL,
                      addEdges = NULL,
                      width =  0.2,
                      position = "left"){
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visLegend with visNetworkProxy object")
  }
  
  if(!any(class(graph) %in% "visNetwork")){
    stop("graph must be a visNetwork object")
  }
  
  if(enabled){
    legend <- list()
    if(!(width >= 0 & width <= 1)){
      stop("'width' must be between 0 and 1")
    }
    legend$width <- width
    
    if(!is.logical(useGroups)){
      stop("useGroups must be logical (TRUE/FALSE)")
    }
    legend$useGroups <- useGroups
    
    if(!position%in%c("left", "right")){
      stop("position must be one of 'left' or 'right'")
    }
    legend$position <- position
    
    if(!is.null(addEdges)){
      if(is.data.frame(addEdges)){
        legend$edges <- toArrayList(addEdges)
      }else if(is.list(addEdges)){
        legend$edges <- addEdges
      }else{
        stop("addEdges must be a data.frame or a list")
      }
    }
    
    if(!is.null(addNodes)){
      if(is.data.frame(addNodes)){
        legend$nodes <- toArrayList(addNodes)
      }else if(is.list(addNodes)){
        legend$nodes <- addNodes
      }else{
        stop("addNodes must be a data.frame or a list")
      }
    }
    graph$x$legend <- legend
  }
  graph
}

