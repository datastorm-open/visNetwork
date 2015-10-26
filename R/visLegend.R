#' Add a legend on a visNetwork object
#'
#' Add a legend on a visNetwork object
#' 
#'@param graph : a visNetwork object
#'@param dragNodes : Boolean. Default to true. When true, the nodes that are not fixed can be dragged by the user.
#'
#' @examples
#'
#' # minimal example
#' nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#' edges <- data.frame(from = c(1,2), to = c(2,3))
#' 
#' # default, on group
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "blue") %>%
#'   visGroups(groupname = "B", color = "yellow") %>%
#'   visLegend()
#'   
#' # default, on group, control width
#' visNetwork(nodes, edges) %>%
#'  visGroups(groupname = "A", color = "blue") %>%
#'   visGroups(groupname = "B", color = "yellow") %>%
#'   visLegend(width = 0.05)
#'   
#' # passing custom nodes and/or edges
#' nodesleg <- data.frame(label = c("Group A", "Group B"), shape = c( "ellipse"), color = c("yellow", "blue"),
#'  title = "Informations") 
#'    
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "blue") %>%
#'   visGroups(groupname = "B", color = "yellow") %>%
#'   visLegend(nodes = nodesleg)
#'   
#' edgesled <- data.frame(color = c("blue", "yellow"), label = c("Aefsfg", "Bqdgqdge"), arrows =c("to", "from")) 
#'  
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "blue") %>%
#'   visGroups(groupname = "B", color = "yellow") %>%
#'   visLegend(edges = edgesled)    
#'
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "blue") %>%
#'   visGroups(groupname = "B", color = "yellow") %>%
#'   visLegend(edges = edgesled, nodes = nodesleg) 
#'   
#' # passing custom information by list
#' nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#' edges <- data.frame(from = c(1,2), to = c(2,3))
#' 
#' visNetwork(nodes, edges) %>%
#'  visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
#'  visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
#'  addFontAwesome() %>%
#'  visLegend(nodes = list(
#'   list(label = "Group", shape = "icon", icon = list(code = "f0c0", size = 25)),
#'   list(label = "User", shape = "icon", icon = list(code = "f007", size = 50, color = "red"))
#'  ),
#'  edges = data.frame(label = "link"))   
#' 
#' @seealso \link{visOptions}, \link{visNodes}, \link{visEdges}, \link{visGroups}, \link{visEvents}
#'
#' @import htmlwidgets
#'
#' @export
#' @importFrom  utils browseURL
visLegend <- function(graph,
                      enabled = TRUE,
                      width =  0.2,
                      nodes = NULL,
                      edges = NULL){
  
  if(enabled){
    legend <- list()
    if(!(width >= 0 & width <= 1)){
      stop("'width' must be between 0 and 1")
    }
    legend$width <- width
    
    if(!is.null(edges)){
      
      edges$from <- seq(1, length.out = nrow(edges), by = 2)
      edges$to <- seq(2, length.out = nrow(edges), by = 2)
      edges$physics <- FALSE
      edges$smooth <- FALSE
      edges$value <- NULL
      
      if(!"arrows" %in% colnames(edges)){
        edges$arrows <- 'to'
      }
      
      if(!"width" %in% colnames(edges)){
        edges$width <- 1
      }

      dataedges <- data.frame(id = sort(unique(c(edges$from, edges$to))),
                             size = 0.00001, hidden = FALSE, shape = "square")

      legend$edges <- edges
      legend$dataedges <- dataedges
    }

 
    if(is.data.frame(nodes)){
      legend$nodesdataframe <- TRUE
    }else if(is.list(nodes)){
      legend$nodesdataframe <- FALSE
    }else{
      stop("nodes must be a data.frame or a list")
    }
    
    legend$nodes <- nodes
    graph$x$legend <- legend
    
  }
  graph
}

