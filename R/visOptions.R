#' Network visualization general options
#'
#' Network visualization general options. For full documentation, have a look at \link{visDocumentation}.
#' 
#'@param graph : a visNetwork object
#'@param width : String. Default to "100\%". The width of the network in pixels or as a percentage.
#'@param height : String. Default to "100\%". The height of the network in pixels or as a percentage.
#'@param highlightNearest : Custom Option. Just a Boolean, or a named list. Default to false. Highlight nearest when clicking a node ? This options use click event. Not available for DOT and Gephi.
#'\itemize{
#'  \item{"enabled"}{ : Boolean. Default to false. Activated or not ?.}
#'  \item{"degree"}{ : Integer. Degree of depth of nodes to be colored. Default to 1}
#'}
#'@param nodesIdSelection :  Custom Option. Boolean. Default to false. A little bit experimental. Add an id node selection. This options use click event. Not available for DOT and Gephi.
#'@param autoResize : Boolean. Default to true. If true, the Network will automatically detect when its container is resized, and redraw itself accordingly. If false, the Network can be forced to repaint after its container has been resized using the function redraw() and setSize(). 
#'@param clickToUse : Boolean. Default to false. When a Network is configured to be clickToUse, it will react to mouse, touch, and keyboard events only when active. When active, a blue shadow border is displayed around the Network. The Network is set active by clicking on it, and is changed to inactive again by clicking outside the Network or by pressing the ESC key.
#'@param manipulation : Just a Boolean
#'@param selectedBy : Custom option. Column name of node data.frame on which you want to add a list selection. Default to NULL
#'
#'@examples
#'
#' # highlight nearest
#' nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
#'  group = sample(LETTERS[1:3], 15, replace = TRUE))
#'
#' edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
#'  to = trunc(runif(15)*(15-1))+1)
#'  
#' visNetwork(nodes, edges) %>% visOptions(highlightNearest = TRUE)
#' visNetwork(nodes, edges) %>% visOptions(highlightNearest = list(enabled = TRUE, degree = 2))
#' 
#' # with an id node selection 
#' visNetwork(nodes, edges) %>% 
#'  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
#'  
#' # or add a selection on another column
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = "group")
#'
#' nodes$sel <- sample(c("sel1", "sel2"), nrow(nodes), replace = TRUE)
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = "sel")
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLayout} & \link{visHierarchicalLayout} for layout, \link{visPhysics} for physics, \link{visInteraction} for interaction, ...
#'@export

visOptions <- function(graph,
                       width = NULL,
                       height = NULL,
                       highlightNearest = FALSE,
                       nodesIdSelection = FALSE,
                       autoResize = NULL,
                       clickToUse = NULL,
                       manipulation = NULL, 
                       selectedBy = NULL){

  options <- list()

  options$autoResize <- autoResize
  options$clickToUse <- clickToUse

  if(is.null(manipulation)){
    options$manipulation <- list(enabled = FALSE)
  }else{
    options$manipulation <- list(enabled = manipulation)
  }
  
  options$height <- height
  options$width <- width

  if(!is.null(manipulation)){
    if(manipulation){
      graph$x$datacss <- paste(readLines(system.file("htmlwidgets/lib/css/dataManipulation.css", package = "visNetwork"), warn = FALSE), collapse = "\n")
    }
  }

  degree <- 1
  
  if(is.list(highlightNearest)){
    if(any(!names(highlightNearest)%in%c("enabled", "degree"))){
      stop("Invalid 'highlightNearest' argument")
    }
    if("degree"%in%names(highlightNearest)){
      degree <- highlightNearest$degree
    }else{
      degree <- 1
    }
    if("enabled"%in%names(highlightNearest)){
      highlightNearest <- highlightNearest$enabled
    }else{
      highlightNearest <- TRUE
    }
  }else if(!is.logical(highlightNearest)){
    stop("Invalid 'highlightNearest' argument")
  }
  
  x <- list(highlight = highlightNearest, idselection = nodesIdSelection, degree = degree)
  x$selectedBy <- selectedBy
  
  if(highlightNearest){
    if(!"label"%in%colnames(graph$x$nodes)){
      graph$x$nodes$label <- as.character(graph$x$nodes$id)
    }
    if(!"group"%in%colnames(graph$x$nodes)){
      graph$x$nodes$group <- 1
    }
  }
  
  if(!is.null(selectedBy)){
    if(!selectedBy%in%colnames(graph$x$nodes)){
      warning("Can't find '", selectedBy, "' in node data.frame")
      x$selectedBy <- NULL
    }else{
      x$selectedValues <- unique(graph$x$nodes[, selectedBy])
      if(!"label"%in%colnames(graph$x$nodes)){
        graph$x$nodes$label <- ""
      }
      if(!"group"%in%colnames(graph$x$nodes)){
        graph$x$nodes$group <- 1
      }
    }
  }
  
  graph$x <- mergeLists(graph$x, x)
  graph$x$options <- mergeLists(graph$x$options, options)

  graph
}
