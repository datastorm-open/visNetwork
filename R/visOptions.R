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
#'@param nodesIdSelection :  Custom Option. Just a Boolean, or a named list. Default to false. A little bit experimental. Add an id node selection. This options use click event. Not available for DOT and Gephi.
#'\itemize{
#'  \item{"enabled"}{ : Boolean. Default to false. Activated or not ?.}
#'  \item{"selected"}{ : Integer/Character. id of the selected node}
#'}
#'@param selectedBy : Custom option. Character or a named list. Add a multiple selection based on column of node data.frame. Not available for DOT and Gephi.
#'\itemize{
#'  \item{"variable"}{ : Character. Column name of selection variable.}
#'  \item{"selected"}{ : Integer/Character. Initial selection.}
#'}
#'@param autoResize : Boolean. Default to true. If true, the Network will automatically detect when its container is resized, and redraw itself accordingly. If false, the Network can be forced to repaint after its container has been resized using the function redraw() and setSize(). 
#'@param clickToUse : Boolean. Default to false. When a Network is configured to be clickToUse, it will react to mouse, touch, and keyboard events only when active. When active, a blue shadow border is displayed around the Network. The Network is set active by clicking on it, and is changed to inactive again by clicking outside the Network or by pressing the ESC key.
#'@param manipulation : Just a Boolean
#'
#'@examples
#' nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
#'  group = sample(LETTERS[1:3], 15, replace = TRUE))
#'
#' edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
#'  to = trunc(runif(15)*(15-1))+1)
#'  
#' ###################  
#' # highlight nearest
#' ###################
#' 
#' visNetwork(nodes, edges) %>% visOptions(highlightNearest = TRUE)
#' visNetwork(nodes, edges) %>% visOptions(highlightNearest = list(enabled = TRUE, degree = 2))
#' 
#' ##########################
#' # nodesIdSelection
#' ##########################
#' 
#' visNetwork(nodes, edges) %>% 
#'  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
#'
#' # add a default selected node ?
#' visNetwork(nodes, edges) %>% 
#'  visOptions(highlightNearest = TRUE, 
#'  nodesIdSelection = list(enabled = TRUE, selected = "1"))
#'      
#' ##########################
#' # selectedBy
#' ##########################
#' 
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = "group")
#'  
#' # add a default value ?
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = list(variable = "group", selected = "A"))
#' 
#' # can also be on new column
#' nodes$sample <- sample(c("sample 1", "sample 2"), nrow(nodes), replace = TRUE)
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = "sample")
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'
#'@export

visOptions <- function(graph,
                       width = NULL,
                       height = NULL,
                       highlightNearest = FALSE,
                       nodesIdSelection = FALSE,
                       selectedBy = NULL,
                       autoResize = NULL,
                       clickToUse = NULL,
                       manipulation = NULL){
  
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
  if(!"nodes"%in%names(graph$x)){
    highlightNearest <- FALSE
    idselection <- list(enabled = FALSE)
    byselection <- list(enabled = FALSE)
  }else{
    #############################
    # highlightNearest
    #############################
    if(is.list(highlightNearest)){
      if(any(!names(highlightNearest)%in%c("enabled", "degree"))){
        stop("Invalid 'highlightNearest' argument")
      }
      if("degree"%in%names(highlightNearest)){
        degree <- highlightNearest$degree
      }
      if("enabled"%in%names(highlightNearest)){
        highlightNearest <- highlightNearest$enabled
      }else{
        highlightNearest <- TRUE
      }
    }else if(!is.logical(highlightNearest)){
      stop("Invalid 'highlightNearest' argument")
    }
    
    if(highlightNearest){
      if(!"label"%in%colnames(graph$x$nodes)){
        graph$x$nodes$label <- as.character(graph$x$nodes$id)
      }
      if(!"group"%in%colnames(graph$x$nodes)){
        graph$x$nodes$group <- 1
      }
    }
    
    #############################
    # nodesIdSelection
    #############################
    idselection <- list(enabled = FALSE)
    if(is.list(nodesIdSelection)){
      if(any(!names(nodesIdSelection)%in%c("enabled", "selected"))){
        stop("Invalid 'nodesIdSelection' argument")
      }
      if("selected"%in%names(nodesIdSelection)){
        if(!nodesIdSelection$selected%in%graph$x$nodes$id){
          stop("nodesIdSelection$selected must be a valid id, in nodes data")
        }
        idselection$selected <- nodesIdSelection$selected
      }
      if("enabled"%in%names(nodesIdSelection)){
        idselection$enabled <- nodesIdSelection$enabled
      }else{
        idselection$enabled <- TRUE
      }
    }else if(is.logical(nodesIdSelection)){
      idselection$enabled <- nodesIdSelection
    }else{
      stop("Invalid 'nodesIdSelection' argument")
    }
    
    #############################
    # selectedBy
    #############################
    byselection <- list(enabled = FALSE)
    if(!is.null(selectedBy)){
      if(is.list(selectedBy)){
        if(any(!names(selectedBy)%in%c("variable", "selected"))){
          stop("Invalid 'selectedBy' argument")
        }
        if("selected"%in%names(selectedBy)){
          byselection$selected <- as.character(selectedBy$selected)
        }
        if(!"variable"%in%names(selectedBy)){
          stop("'selectedBy' need at least 'variable' information")
        }
        byselection$variable <- selectedBy$variable
      }else if(is.character(selectedBy)){
        byselection$variable <- selectedBy
      }else{
        stop("Invalid 'selectedBy' argument")
      }
      
      if(!byselection$variable%in%colnames(graph$x$nodes)){
        warning("Can't find '", byselection$variable, "' in node data.frame")
      }else{
        byselection$enabled <- TRUE
        byselection$values <- sort(as.character(unique(graph$x$nodes[, byselection$variable])))
        if("selected"%in%names(byselection)){
          if(!byselection$selected%in%byselection$values){
            stop("selectedBy$selected must be a valid value, in node data")
          }
          byselection$selected <- byselection$selected
        }
        if(!"label"%in%colnames(graph$x$nodes)){
          graph$x$nodes$label <- ""
        }
        if(!"group"%in%colnames(graph$x$nodes)){
          graph$x$nodes$group <- 1
        }
      }
    }
  }
  
  x <- list(highlight = highlightNearest, degree = degree, idselection = idselection, 
            byselection = byselection)
  
  graph$x <- mergeLists(graph$x, x)
  graph$x$options <- mergeLists(graph$x$options, options)
  
  graph
}
