#' Network visualization general options
#'
#' Network visualization general options. For full documentation, have a look at \link{visDocumentation}.
#' 
#'@param graph : a visNetwork object
#'@param width : String. Default to "100\%". The width of the network in pixels or as a percentage.
#'@param height : String. Default to "100\%". The height of the network in pixels or as a percentage.
#'@param highlightNearest : Custom Option. Just a Boolean, or a named list. Default to false. Highlight nearest when clicking a node ? Not available for DOT and Gephi.
#'\itemize{
#'  \item{"enabled"}{ : Boolean. Default to false. Activated or not ?.}
#'  \item{"degree"}{ : Optional. Integer. Degree of depth of nodes to be colored. Default to 1. Set high number to have the entire sub-network. In case of "hierarchical" algorithm, you can also pass a list(from = 1, to = 1) to control degree in both direction}
#'  \item{"hover"}{ : Optional. Boolean. Enable highlightNearest alos hovering a node ? Default to FALSE}
#'  \item{"algorithm"}{ : Optional. String. highlightNearest algorithm. "all" highlight all nodes, without taking direction information. "hierarchical" look only at inputs/outputs nodes.}
#'  \item{"hideColor"}{ : Optional. String. Color for hidden nodes/edges. Use a rgba definition. Defaut to rgba(200,200,200,0.5)}
#'  \item{"labelOnly"}{ : Optional. Boolean. Keep just label for nodes on degree + 1 ? Default to TRUE}
#'}
#'@param nodesIdSelection :  Custom Option. Just a Boolean, or a named list. Default to false. Add an id node selection creating an HTML select element. This options use click event. Not available for DOT and Gephi.
#'\itemize{
#'  \item{"enabled"}{ : Boolean. Default to false. Activated or not ?.}
#'  \item{"values}{ : Optional. Vector of possible values (node's id), and so order is preserve. Defaut to all id in nodes data.frame.}
#'  \item{"selected"}{ : Optional. Integer/Character. Initial id selection. Defaut to NULL}
#'  \item{"style"}{ : Optional. Character. HTML style of list. Default to 'width: 150px; height: 26px'.}
#'  \item{"useLabels"}{ : Optional. Boolean. Use labels instead of id ? Default to TRUE.}
#'  \item{"main"}{ : Optional. Default to "Select by id"}
#'}
#'@param selectedBy : Custom option. Character or a named list. Add a multiple selection based on column of node data.frame creating an HTML select element. Not available for DOT and Gephi.
#'\itemize{
#'  \item{"variable"}{ : Character. Column name of selection variable.}
#'  \item{"values}{ : Optional. Vector of possible values. Defaut to all values in nodes data.frame.}
#'  \item{"selected"}{ : Optional. Integer/Character. Initial selection. Defaut to NULL}
#'  \item{"style"}{ : Optional. Character. HTML style of list. Default to 'width: 150px; height: 26px'.}
#'  \item{"multiple"}{ : Optional. Boolean. Default to FALSE. If TRUE, you can affect multiple groups per nodes using a comma ("gr1,gr2")}
#'  \item{"hideColor"}{ : Optional. String. Color for hidden nodes/edges. Use a rgba definition. Defaut to rgba(200,200,200,0.5)}
#'  \item{"main"}{ : Optional. Default to "Select by variable"}
#'  \item{"sort"}{ : Optional. If values is NULL, sort all possible values ?. Defaut to TRUE}
#'  \item{"highlight"}{ : Optional. Boolean. Run highlightNearest if defined on each selected node ? Defaut to FALSE}
#'}
#'@param collapse : Custom option. Just a Boolean, or a named list. Collapse / Uncollapse nodes using double-click. In dev.
#'\itemize{
#'  \item{"enabled"}{ : Boolean. Default to false. Activated or not ?}
#'  \item{"fit"}{ : Optional. Boolean. Default to FALSE. Call fit method after collapse/uncollapse event ?}
#'  \item{"resetHighlight"}{ : Optional. Boolean. Default to TRUE to reset highlighted nodes after collapse/uncollapse event.}
#'  \item{"clusterOptions"}{ : Optional. List. Defaut to NULL. A list of all options you want to pass to cluster collapsed node}
#'  \item{"keepCoord"}{ : Optional. Boolean. Default to TRUE to keep nodes coordinates on collapse}
#'  \item{"labelSuffix"}{ : Optional. Character. Use node label + suffix or just suffix. Default to '(cluster)'}
#'}
#'@param autoResize : Boolean. Default to true. If true, the Network will automatically detect when its container is resized, and redraw itself accordingly. If false, the Network can be forced to repaint after its container has been resized using the function redraw() and setSize(). 
#'@param clickToUse : Boolean. Default to false. When a Network is configured to be clickToUse, it will react to mouse, touch, and keyboard events only when active. When active, a blue shadow border is displayed around the Network. The Network is set active by clicking on it, and is changed to inactive again by clicking outside the Network or by pressing the ESC key.
#'@param manipulation : Just a Boolean or a list. See \link{visDocumentation}
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
#' # also when hover a node ?
#' visNetwork(nodes, edges) %>% visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE))
#' 
#' # don't show nodes/edges
#' visNetwork(nodes, edges) %>% visOptions(highlightNearest = list(enabled = TRUE, 
#'  hover = TRUE, hideColor = 'rgba(200,200,200,0)'))
#' 
#' # Using hierarchical information
#' nodes = data.frame(id = 1:6, level = c(1, 2, 3, 3, 4, 2))
#' edges = data.frame(from = c(1, 2, 2, 4, 6), to = c(2, 3, 4, 5, 4))
#' 
#' visNetwork(nodes, edges) %>% visHierarchicalLayout() %>% visEdges(arrows = "to") %>% 
#'  visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"))
#'  
#' visNetwork(nodes, edges) %>% visHierarchicalLayout() %>% visEdges(arrows = "to") %>% 
#'  visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
#'    degree = list(from = 0, to = 2)))
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
#' # subset on id values & don't use labels ?
#' visNetwork(nodes, edges) %>% 
#'  visOptions(highlightNearest = TRUE, 
#'  nodesIdSelection = list(enabled = TRUE, 
#'    selected = "2", values = c(2:10), useLabels = FALSE))
#'  
#' # some style
#' visNetwork(nodes, edges) %>% 
#'  visOptions(highlightNearest = TRUE, 
#'  nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
#'    background: #f8f8f8;
#'    color: darkblue;
#'    border:none;
#'    outline:none;'))   
#'  
#' ##########################
#' # collapse
#' ##########################
#'  
#' nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
#'  group = sample(LETTERS[1:3], 15, replace = TRUE))
#'
#' edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
#'  to = trunc(runif(15)*(15-1))+1)
#'  
#' # keeping all parent node attributes  
#' visNetwork(nodes, edges) %>% visEdges(arrows = "to") %>%
#'  visOptions(collapse = TRUE)
#'
#' # setting some properties  
#' visNetwork(nodes, edges) %>% visEdges(arrows = "to") %>%
#'  visOptions(collapse = list(enabled = TRUE, clusterOptions = list(shape = "square"))) 
#'    
#' # enable / disable open cluster (proxy only) : 
#' # visEvents(type = "off", doubleClick = "networkOpenCluster")
#' # visEvents(type = "on", doubleClick = "networkOpenCluster")  
#'  
#' ##########################
#' # selectedBy
#' ##########################
#' nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
#'  group = sample(LETTERS[1:3], 15, replace = TRUE))
#'
#' edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
#'  to = trunc(runif(15)*(15-1))+1)
#'  
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = "group")
#'  
#' # add a default value ?
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = list(variable = "group", selected = "A"))
#' 
#' # subset on values ?
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = list(variable = "group", 
#'    selected = "C",
#'    values = c("A", "C")))
#'  
#' # highlight also
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = list(variable = "group", 
#'    highlight = TRUE), highlightNearest = TRUE) 
#'      
#' # add some style
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = list(variable = "group", style = 'width: 200px; height: 26px;
#'    background: #f8f8f8;
#'    color: darkblue;
#'    border:none;
#'    outline:none;'))
#'      
#' # can also be on new column
#' nodes$sample <- sample(c("sample 1", "sample 2"), nrow(nodes), replace = TRUE)
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = "sample")
#' 
#' # and with multiple groups ?
#' nodes$group <- sample(c("group 1", "group 2", "group 1, group 2, group 3"), 
#'  nrow(nodes), replace = TRUE)
#'  
#' visNetwork(nodes, edges) %>% 
#'  visOptions(selectedBy = list(variable = "group", multiple = TRUE))
#'  
#' ##########################
#' # manipulation
#' ##########################
#'  
#'visNetwork(nodes, edges) %>% 
#'  visOptions(manipulation = TRUE)
#'
#'visNetwork(nodes, edges) %>% 
#'  visOptions(manipulation = list(enabled = TRUE, addNode = FALSE, addEdge = FALSE))
#'
#'visNetwork(nodes, edges) %>% 
#'  visOptions(manipulation = list(enabled = TRUE, deleteNode = FALSE, deleteEdge = FALSE))
#'
#'visNetwork(nodes, edges) %>% 
#'  visOptions(manipulation = list(enabled = TRUE, editNode = FALSE, editEdge = FALSE))
#'
#'visNetwork(nodes, edges)  %>% 
#'  visOptions(manipulation = list(enabled = TRUE, 
#'                                 editEdge = htmlwidgets::JS("function(data, callback) {
#'                                                            console.info('edit edge')
#'                                                            }")
#'                                     )
#'                                 )
#' ##########################
#' # collapse
#' ##########################
#' visNetwork(nodes, edges) %>% 
#'  visEdges(arrows = "to") %>% 
#'  visOptions(collapse = list(enabled = TRUE, 
#'    clusterOptions = list(shape = "square")))
#'   
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visOptions <- function(graph,
                       width = NULL,
                       height = NULL,
                       highlightNearest = FALSE,
                       nodesIdSelection = FALSE,
                       selectedBy = NULL,
                       collapse = FALSE,
                       autoResize = NULL,
                       clickToUse = NULL,
                       manipulation = NULL){
  
  if(!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))){
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  
  options <- list()
  
  options$autoResize <- autoResize
  options$clickToUse <- clickToUse
  
  if(is.null(manipulation)){
    options$manipulation <- list(enabled = FALSE)
  }else{
    if(is.logical(manipulation)){
      options$manipulation <- list(enabled = manipulation)
    } else if(is.list(manipulation)){
      options$manipulation <- manipulation
    } else {
      stop("Invalid 'manipulation' argument. logical or list")
    }
  }
  
  options$height <- height
  options$width <- width
  
  if(!is.null(manipulation)){
      graph$x$datacss <- paste(readLines(system.file("htmlwidgets/lib/css/dataManipulation.css", package = "visNetwork"), warn = FALSE), collapse = "\n")
  }
  
  if(!"nodes"%in%names(graph$x) && any(class(graph) %in% "visNetwork")){
    highlight <- list(enabled = FALSE)
    idselection <- list(enabled = FALSE)
    byselection <- list(enabled = FALSE)
    list_collapse <- list(enabled = FALSE, fit = FALSE, resetHighlight = TRUE, 
                          keepCoord = TRUE, labelSuffix = "(cluster)")
  }else{
    #############################
    # collapse
    #############################
    list_collapse <- list(enabled = FALSE, fit = FALSE, resetHighlight = TRUE, 
                          clusterOptions = NULL, keepCoord = TRUE, labelSuffix = "(cluster)")
    if(is.list(collapse)){
      if(any(!names(collapse)%in%c("enabled", "fit", "resetHighlight", "clusterOptions", "keepCoord", "labelSuffix"))){
        stop("Invalid 'collapse' argument")
      }
      
      if("enabled"%in%names(collapse)){
        stopifnot(is.logical(collapse$enabled))
        list_collapse$enabled <- collapse$enabled
      }
      if("fit"%in%names(collapse)){
        stopifnot(is.logical(collapse$fit))
        list_collapse$fit <- collapse$fit
      }
      if("resetHighlight"%in%names(collapse)){
        stopifnot(is.logical(collapse$resetHighlight))
        list_collapse$resetHighlight <- collapse$resetHighlight
      }
      if("keepCoord"%in%names(collapse)){
        stopifnot(is.logical(collapse$keepCoord))
        list_collapse$keepCoord <- collapse$keepCoord
      }
      if("labelSuffix"%in%names(collapse)){
        stopifnot(is.character(collapse$labelSuffix))
        list_collapse$labelSuffix <- collapse$labelSuffix
      }
      if("clusterOptions"%in%names(collapse)){
        stopifnot(is.list(collapse$clusterOptions))
        list_collapse$clusterOptions <- collapse$clusterOptions
      }
    } else {
      stopifnot(is.logical(collapse))
      list_collapse$enabled <- collapse
    }
      
    #############################
    # highlightNearest
    #############################
    highlight <- list(enabled = FALSE, hoverNearest = FALSE, degree = 1, algorithm = "all", hideColor = 'rgba(200,200,200,0.5)', labelOnly = TRUE)
    if(is.list(highlightNearest)){
      if(any(!names(highlightNearest)%in%c("enabled", "degree", "hover", "algorithm", "hideColor", "labelOnly"))){
        stop("Invalid 'highlightNearest' argument")
      }
      
      if("algorithm"%in%names(highlightNearest)){
        stopifnot(highlightNearest$algorithm %in% c("all", "hierarchical"))
        highlight$algorithm <- highlightNearest$algorithm
      }
      
      if("hideColor"%in%names(highlightNearest)){
        highlight$hideColor <- highlightNearest$hideColor
      }
      
      if("degree"%in%names(highlightNearest)){
        highlight$degree <- highlightNearest$degree
      }
      
      if(highlight$algorithm %in% "hierarchical"){
        if(is.list(highlight$degree)){
          stopifnot(all(names(highlight$degree) %in% c("from", "to")))
        }else{
          highlight$degree <- list(from = highlight$degree, to = highlight$degree)
        }
      }
      
      if("labelOnly"%in%names(highlightNearest)){
        stopifnot(is.logical(highlightNearest$labelOnly))
        highlight$labelOnly <- highlightNearest$labelOnly
      }
      
      if("hover"%in%names(highlightNearest)){
        stopifnot(is.logical(highlightNearest$hover))
        highlight$hoverNearest <- highlightNearest$hover
      }
      
      if("enabled"%in%names(highlightNearest)){
        stopifnot(is.logical(highlightNearest$enabled))
        highlight$enabled <- highlightNearest$enabled
      }
      
    } else {
      stopifnot(is.logical(highlightNearest))
      highlight$enabled <- highlightNearest
    }
    
    if(highlight$enabled && any(class(graph) %in% "visNetwork")){
      if(!"label"%in%colnames(graph$x$nodes)){
        if(is.data.frame(graph$x$nodes)){
          graph$x$nodes$label <- as.character(graph$x$nodes$id)
        } else if(is.list(graph$x$nodes)){
          ctrl <- lapply(1:length(graph$x$nodes), function(x){
            graph$x$nodes[[x]]$label <<- as.character(graph$x$nodes[[x]]$id)
          })
        }
      }
      # if(!"group"%in%colnames(graph$x$nodes)){
      #   if(is.data.frame(graph$x$nodes)){
      #     graph$x$nodes$group <- 1
      #   } else if(is.list(graph$x$nodes)){
      #     ctrl <- lapply(1:length(graph$x$nodes), function(x){
      #       graph$x$nodes[[x]]$group <<- 1
      #     })
      #   }
      # }
    }
    
    #############################
    # nodesIdSelection
    #############################
    idselection <- list(enabled = FALSE, style = 'width: 150px; height: 26px', useLabels = TRUE, main = "Select by id")
    if(is.list(nodesIdSelection)){
      if(any(!names(nodesIdSelection)%in%c("enabled", "selected", "style", "values", "useLabels", "main"))){
        stop("Invalid 'nodesIdSelection' argument. List can have 'enabled', 'selected', 'style', 'values', 'useLabels', 'main'")
      }
      if("selected"%in%names(nodesIdSelection)){
        if(any(class(graph) %in% "visNetwork")){
          if(!nodesIdSelection$selected%in%graph$x$nodes$id ){
            stop(nodesIdSelection$selected, " not in data. nodesIdSelection$selected must be valid.")
          }
        }
        idselection$selected <- nodesIdSelection$selected
      }
      if("enabled"%in%names(nodesIdSelection)){
        idselection$enabled <- nodesIdSelection$enabled
      }else{
        idselection$enabled <- TRUE
      }
      
      if("main"%in%names(nodesIdSelection)){
        idselection$main <- nodesIdSelection$main
      }
      
      if("useLabels"%in%names(nodesIdSelection)){
        idselection$useLabels <- nodesIdSelection$useLabels
      }else if(any(class(graph) %in% "visNetwork_Proxy")){
        idselection$useLabels <- NULL
      }
      
      if("style"%in%names(nodesIdSelection)){
        idselection$style <- nodesIdSelection$style
      }else if(any(class(graph) %in% "visNetwork_Proxy")){
        idselection$style <- NULL
      }
      
    }else if(is.logical(nodesIdSelection)){
      idselection$enabled <- nodesIdSelection
      if(any(class(graph) %in% "visNetwork_Proxy")){
        idselection$useLabels <- NULL
        idselection$style <- NULL
      }
    }else{
      stop("Invalid 'nodesIdSelection' argument")
    }
    
    if(idselection$enabled){
      if("values"%in%names(nodesIdSelection)){
        idselection$values <- nodesIdSelection$values
        if(length(idselection$values) == 1){
          idselection$values <- list(idselection$values)
        }
        if("selected"%in%names(nodesIdSelection)){
          if(!idselection$selected%in%idselection$values){
            stop(idselection$selected, " not in data/selection. nodesIdSelection$selected must be a valid value.")
          }
        }
      }
    }
    
    #############################
    # selectedBy
    #############################
    byselection <- list(enabled = FALSE, style = 'width: 150px; height: 26px', multiple = FALSE, 
                        hideColor = 'rgba(200,200,200,0.5)', highlight = FALSE)
    
    if(!is.null(selectedBy)){
      if(is.list(selectedBy)){
        if(any(!names(selectedBy)%in%c("variable", "selected", "style", "values", "multiple", "hideColor", "main", "sort", "highlight"))){
          stop("Invalid 'selectedBy' argument. List can have 'variable', 'selected', 'style', 'values', 'multiple', 'hideColor', 'main', 'sort', 'highlight'")
        }
        if("selected"%in%names(selectedBy)){
          byselection$selected <- as.character(selectedBy$selected)
        }
        
        if("hideColor"%in%names(selectedBy)){
          byselection$hideColor <- selectedBy$hideColor
        }
        
        if("highlight"%in%names(selectedBy)){
          byselection$highlight <- selectedBy$highlight
        }
        
        if(!"variable"%in%names(selectedBy)){
          stop("'selectedBy' need at least 'variable' information")
        }
        
        byselection$variable <- selectedBy$variable
        
        if("main" %in% names(selectedBy)){
          byselection$main <- selectedBy$main
        } else {
          byselection$main <- paste0("Select by ", selectedBy$variable)
        }
 
        if("style"%in%names(selectedBy)){
          byselection$style <- selectedBy$style
        }else if(any(class(graph) %in% "visNetwork_Proxy")){
          byselection$style <- NULL
        }
        
        if("multiple"%in%names(selectedBy)){
          byselection$multiple <- selectedBy$multiple
        }else if(any(class(graph) %in% "visNetwork_Proxy")){
          byselection$multiple <- NULL
        }
        
      }else if(is.character(selectedBy)){
        byselection$variable <- selectedBy
        
        byselection$main <- paste0("Select by ", selectedBy)
        
        if(any(class(graph) %in% "visNetwork_Proxy")){
          byselection$style <- NULL
          byselection$multiple <- NULL
        }
        
      }else{
        stop("Invalid 'selectedBy' argument. Must a 'character' or a 'list'")
      }
      
      if(any(class(graph) %in% "visNetwork_Proxy")){
        byselection$enabled <- TRUE
        
        if("values"%in%names(selectedBy)){
          if(length(selectedBy$values) > 1){
            byselection$values <- selectedBy$values
          } else {
            byselection$values <- list(selectedBy$values)
          }
        }
        
        if("selected"%in%names(byselection)){
          byselection$selected <- byselection$selected
        }
      }else{
        if(!byselection$variable%in%colnames(graph$x$nodes)){
          warning("Can't find '", byselection$variable, "' in node data.frame")
        }else{
          byselection$enabled <- TRUE
          byselection$values <- unique(graph$x$nodes[, byselection$variable])
          if(byselection$multiple){
            byselection$values <- unique(gsub("^[[:space:]]*|[[:space:]]*$", "",
                                              do.call("c",strsplit(as.character(byselection$values), split = ","))))
          }
          if(any(c("integer", "numeric") %in% class(graph$x$nodes[, byselection$variable]))){
            byselection$values <- byselection$values
          }else{
            byselection$values <- as.character(byselection$values)
          }
          
          if("sort"%in%names(selectedBy)){
            if(selectedBy$sort){
              byselection$values <- sort(byselection$values)
            }
          } else {
            byselection$values <- sort(byselection$values)
          }
          
          if("values"%in%names(selectedBy)){
            # byselection$values <- intersect(byselection$values, selectedBy$values)
            byselection$values <- selectedBy$values
          }
          
          if("values"%in%names(byselection)){
            if(length(byselection$values) == 1){
              byselection$values <- list(byselection$values)
            }
          }
          
          if("selected"%in%names(byselection)){
            if(!byselection$selected%in%byselection$values){
              stop(byselection$selected, " not in data/selection. selectedBy$selected must be a valid value.")
            }
            byselection$selected <- byselection$selected
          }
          
          if(!"label"%in%colnames(graph$x$nodes)){
            if(is.data.frame(graph$x$nodes)){
              graph$x$nodes$label <- ""
            } else if(is.list(graph$x$nodes)){
              ctrl <- lapply(1:length(graph$x$nodes), function(x){
                graph$x$nodes[[x]]$label <<- ""
              })
            }
          }
          # if(!"group"%in%colnames(graph$x$nodes)){
          #   if(is.data.frame(graph$x$nodes)){
          #     graph$x$nodes$group <- 1
          #   } else if(is.list(graph$x$nodes)){
          #     ctrl <- lapply(1:length(graph$x$nodes), function(x){
          #       graph$x$nodes[[x]]$group <<- 1
          #     })
          #   }
          # }
        }
      }
    }
  }
  
  # x <- list(highlight = highlightNearest, hoverNearest = hoverNearest, degree = degree, 
  #           idselection = idselection, byselection = byselection)
  
  x <- list(highlight = highlight, idselection = idselection, byselection = byselection, collapse = list_collapse)
  
  if(highlight$hoverNearest){
    graph <- visInteraction(graph, hover = TRUE)
  }
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    
    data <- list(id = graph$id, options = options)
    graph$session$sendCustomMessage("visShinyOptions",data)
    
    if(missing(highlightNearest)){
      x$highlight <- NULL
    }
    if(missing(nodesIdSelection)){
      x$idselection <- NULL
    }
    if(missing(selectedBy)){
      x$byselection <- NULL
    }
    if(missing(collapse)){
      x$collapse <- NULL
    }
    
    data <- list(id = graph$id, options = x)
    graph$session$sendCustomMessage("visShinyCustomOptions",data)
    
  }else{
    graph$x <- mergeLists(graph$x, x)
    graph$x$options <- mergeLists(graph$x$options, options)
  }
  graph
}
