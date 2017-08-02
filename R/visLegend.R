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
#' @param main : For add a title. Character or a named list.
#' \itemize{
#'  \item{"text"}{ : Character. Title.}
#'  \item{"style"}{ : Optional. Character. HTML style of title. Default to 'font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:14px;text-align:center;'.}
#' }
#' @param ncol : Divide legend in multiple columns ? Defaut to 1 
#' @param stepX : Experimental. Can use to control space between nodes. Defaut to 100
#' @param stepY : Experimental. Can use to control space between nodes. Defaut to 100
#' @param zoom : Boolean. Enable zoom on legend ? Defaut to TRUE
#' 
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
#' # on group, adding options
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "red") %>%
#'   visGroups(groupname = "B", color = "lightblue") %>%
#'   visLegend(width = 0.05, position = "right", main = "Legend")
#'   
#' # css on main   
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "red") %>%
#'   visGroups(groupname = "B", color = "lightblue") %>%
#'   visLegend(main = list(text = "Custom Legend",
#'  style = "font-family:Comic Sans MS;color:#ff0000;font-size:12px;text-align:center;"))
#'    
#' # passing custom nodes and/or edges
#' lnodes <- data.frame(label = c("Group A", "Group B"), 
#'  shape = c( "ellipse"), color = c("red", "lightblue"),
#'  title = "Informations") 
#'    
#' ledges <- data.frame(color = c("lightblue", "red"), 
#'  label = c("reverse", "depends"), arrows =c("to", "from"), 
#'  font.align = "top") 
#'  
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "red") %>%
#'   visGroups(groupname = "B", color = "lightblue") %>%
#'   visLegend(addNodes = lnodes, addEdges = ledges, useGroups = FALSE)
#'    
#' # divide in columns
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", color = "red") %>%
#'   visGroups(groupname = "B", color = "lightblue") %>%
#'   visLegend(addNodes = lnodes, useGroups = TRUE, ncol = 2)
#'   
#' # for more complex option, you can use a list(of list...)
#' # or a data.frame with specific notaion
#'
#' nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#' edges <- data.frame(from = c(1,2), to = c(2,3))
#' 
#' # using a list
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
#' # using a data.frame
#' addNodes <- data.frame(label = c("Group", "User"), shape = "icon",
#'  icon.code = c("f0c0", "f007"), icon.size = c(25, 50), icon.color = c(NA, "red"))
#'  
#' visNetwork(nodes, edges) %>%
#'  visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
#'  visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
#'  addFontAwesome() %>%
#'  visLegend(addNodes = addNodes,
#'    addEdges = data.frame(label = "link"), useGroups = FALSE)   
#'    
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'
#' @import htmlwidgets
#'
#' @export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visLegend <- function(graph,
                      enabled = TRUE,
                      useGroups = TRUE,
                      addNodes = NULL,
                      addEdges = NULL,
                      width =  0.2,
                      position = "left",
                      main = NULL,
                      ncol = 1, 
                      stepX = 100, 
                      stepY = 100, 
                      zoom = TRUE){
  
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
    
    if(!ncol >= 1){
      stop("ncol must be an integer >= 1")
    }
    legend$ncol <- ncol
    
    legend$stepX <- stepX
    legend$stepY <- stepY
    
    legend$zoom <- zoom
    
    if(!is.null(addEdges)){
      legend$edges <- addEdges
      if(is.data.frame(addEdges)){
        legend$edgesToDataframe <- TRUE
      }else if(is.list(addEdges)){
        legend$edgesToDataframe <- TRUE
      }else{
        stop("addEdges must be a data.frame or a list")
      }
    }
    
    if(!is.null(addNodes)){
      legend$nodes <- addNodes
      if(is.data.frame(addNodes)){
        legend$nodesToDataframe <- TRUE
      }else if(is.list(addNodes)){
        legend$nodesToDataframe <- FALSE
      }else{
        stop("addNodes must be a data.frame or a list")
      }
    }
    
    # main
    if(!is.null(main)){
      if(is.list(main)){
        if(any(!names(main)%in%c("text", "style"))){
          stop("Invalid 'main' argument")
        }
        if(!"text"%in%names(main)){
          stop("Needed a 'text' value using a list for 'main'")
        }
        if(!"style"%in%names(main)){
          main$style <- 'font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:14px;text-align:center;'
        }
      }else if(!inherits(main, "character")){
        stop("Invalid 'main' argument. Not a character")
      }else {
        main <- list(text = main, 
                     style = 'font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:14px;text-align:center;')
      }
      legend$main <- main
    }
    graph$x$legend <- legend
  }
  graph
}

