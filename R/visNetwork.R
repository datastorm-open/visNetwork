#' Network visualization
#'
#' Network visualization using vis.js library. For full documentation, have a look at \link{visDocumentation}.
#'
#' @param nodes : data.frame or a list with nodes informations. Needed at least column "id". See \link{visNodes} 
#' \itemize{
#'  \item{"id"}{ : id of the node, needed in edges information}
#'  \item{"label"}{ : label of the node}
#'  \item{"group"}{ : group of the node. Groups can be configure with \link{visGroups}}
#'  \item{"value"}{ : size of the node}
#'  \item{"title"}{ : tooltip of the node}
#'  \item{...}{}
#'}
#'
#' @param edges : data.frame or a list  with edges informations. Needed at least columns "from" and "to". See \link{visEdges}
#' \itemize{
#'  \item{"from"}{ : node id of begin of the edge}
#'  \item{"to"}{ : node id of end of the edge}
#'  \item{"label"}{ : label of the edge}
#'  \item{"value"}{ : size of the node}
#'  \item{"title"}{ : tooltip of the node}
#'  \item{...}{}
#'}
#'
#' @param dot : Character DOT language.
#' 
#' @param gephi : Json export gephi path file.
#' 
#' @param width	: Width (optional, defaults to automatic sizing)
#' 
#' @param height	: Height (optional, defaults to automatic sizing)
#' 
#'@param main : For add a title. Character or a named list.
#'\itemize{
#'  \item{"text"}{ : Character. Title.}
#'  \item{"style"}{ : Optional. Character. HTML style of title. Default to 'font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:20px;text-align:center;'.}
#'}
#'
#'@param submain : For add a subtitle. Character or a named list.
#'\itemize{
#'  \item{"text"}{ : Character. Subtitle.}
#'  \item{"style"}{ : Optional. Character. HTML style of submain. Default to 'font-family:Georgia, Times New Roman, Times, serif;font-size:12px;text-align:center;'.}
#'}
#'
#'@param footer : For add a footer. Character or a named list.
#'\itemize{
#'  \item{"text"}{ : Character. footer.}
#'  \item{"style"}{ : Optional. Character. HTML style of footer. Default to 'font-family:Georgia, Times New Roman, Times, serif;font-size:12px;text-align:center;'.}
#'}
#'
#'@param background : Background color. Default to 'rgba(0, 0, 0, 0)' (transparent). Can be a valid color name ("red"), a HEX value ("#ff0000") or rgb/rgba ("rgb(255,0,0)")
#'
#' @param ... : Don't use.
#' 
#' @examples
#'
#' # minimal example
#' nodes <- data.frame(id = 1:3)
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#'
#' visNetwork(nodes, edges)
#'
#' # add a title
#' visNetwork(nodes, edges, main = "visNetwork minimal example")
#' visNetwork(nodes, edges, main = list(text = "visNetwork minimal example",
#'  style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;"))
#'  
#' # and subtitle and footer
#' visNetwork(nodes, edges, main = "visNetwork minimal example",
#'  submain = "For add a subtitle", footer = "Fig.1 minimal example")
#'  
#' # change background color
#' visNetwork(nodes, edges, background = "black")
#' 
#' # customization adding more variables (see visNodes and visEdges)
#' nodes <- data.frame(id = 1:10, 
#'                     label = paste("Node", 1:10),                                 # labels
#'                     group = c("GrA", "GrB"),                                     # groups 
#'                     value = 1:10,                                                # size 
#'                     shape = c("square", "triangle", "box", "circle", "dot", "star",
#'                               "ellipse", "database", "text", "diamond"),         # shape
#'                     title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),         # tooltip
#'                     color = c("darkred", "grey", "orange", "darkblue", "purple"),# color
#'                     shadow = c(FALSE, TRUE, FALSE, TRUE, TRUE))                  # shadow
#'
#' edges <- data.frame(from = sample(1:10,8), to = sample(1:10, 8),
#'                     label = paste("Edge", 1:8),                                 # labels
#'                     length = c(100,500),                                        # length
#'                     arrows = c("to", "from", "middle", "middle;to"),            # arrows
#'                     dashes = c(TRUE, FALSE),                                    # dashes
#'                     title = paste("Edge", 1:8),                                 # tooltip
#'                     smooth = c(FALSE, TRUE),                                    # smooth
#'                     shadow = c(FALSE, TRUE, FALSE, TRUE))                       # shadow
#'
#' visNetwork(nodes, edges) 
#' 
#' # use more complex configuration : 
#' # when it's a list, you can use data.frame with specific notation like this
#' nodes <- data.frame(id = 1:3, color.background = c("red", "blue", "green"), 
#'  color.highlight.background = c("red", NA, "red"), shadow.size = c(5, 10, 15))
#' edges <- data.frame(from = c(1,2), to = c(1,3),
#'  label = LETTERS[1:2], font.color =c ("red", "blue"), font.size = c(10,20))
#'
#' visNetwork(nodes, edges)
#' 
#' # highlight nearest
#' nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
#'  group = sample(LETTERS[1:3], 15, replace = TRUE))
#'
#' edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
#'  to = trunc(runif(15)*(15-1))+1)
#'  
#' visNetwork(nodes, edges) %>% visOptions(highlightNearest = TRUE)
#' 
#' # try an id node selection 
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
#' # add legend
#' visNetwork(nodes, edges) %>% visLegend()
#'     
#' # directed network
#' visNetwork(nodes, edges) %>% 
#'  visEdges(arrows = 'from', scaling = list(min = 2, max = 2))
#'
#' # custom navigation
#' visNetwork(nodes, edges) %>%
#'  visInteraction(navigationButtons = TRUE)
#'
#' # data Manipulation
#' visNetwork(nodes, edges) %>% visOptions(manipulation = TRUE)
#'
#' # Hierarchical Layout
#' visNetwork(nodes, edges) %>% visHierarchicalLayout()
#'
#' # freeze network
#' visNetwork(nodes, edges) %>%
#'  visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE)
#'
#' # use fontAwesome icons using groups or nodes options 
#' # font-awesome is not part of dependencies. use addFontAwesome() if needed
#' # http://fortawesome.github.io/Font-Awesome
#' 
#' nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#' edges <- data.frame(from = c(1,2), to = c(2,3))
#' 
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
#'   visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
#'   addFontAwesome()
#' 
#' nodes <- data.frame(id = 1:3)
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#' 
#' visNetwork(nodes, edges) %>%
#'   visNodes(shape = "icon", icon = list( face ='FontAwesome', code = "f0c0")) %>%
#'   addFontAwesome()
#'
#' # Save a network
#' \dontrun{
#' network <- visNetwork(nodes, edges) %>% 
#'  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
#'  manipulation = TRUE) %>% visLegend()
#'  
#' network %>% visSave(file = "network.html")
#' # same as
#' visSave(network, file = "network.html")
#' }
#' 
#' # Export as png/jpeg (shiny or browser only)
#' \dontrun{
#' visNetwork(nodes, edges) %>% 
#'  visExport()
#' }
#' 
#' # DOT language
#' visNetwork(dot = 'dinetwork {1 -> 1 -> 2; 2 -> 3; 2 -- 4; 2 -> 1 }')
#' 
#' # gephi json file
#' \dontrun{
#' visNetwork(gephi = 'WorldCup2014.json') %>% visPhysics(stabilization = FALSE,   barnesHut = list(
#'     gravitationalConstant = -10000,
#'     springConstant = 0.002,
#'     springLength = 150
#'   ))
#'}
#' 
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} for play with network using shiny,
#'\link{visTree} to visualize CART rpart tree, \link{visNetworkEditor} to edit your network, 
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'
#' @import htmlwidgets
#' 
#' @importFrom jsonlite fromJSON
#'
#' @export
#' @references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visNetwork <- function(nodes = NULL, edges = NULL, dot = NULL, gephi = NULL,
                       width = NULL, height = NULL, main = NULL, submain = NULL, 
                       footer = NULL, background = 'rgba(0, 0, 0, 0)', ...) {
  
  if(is.null(nodes) & is.null(edges) & is.null(dot) & is.null(gephi)){
    stop("Must 'dot' data, or 'gephi' data, or 'nodes' and 'edges' data.")
  }
  
  if(!is.null(nodes)){
    if(any(class(nodes)%in%c("tbl_df", "tbl", "data.table"))){
      nodes <- data.frame(nodes)
    }
    if(is.data.frame(nodes)){
      nodesToDataframe <- TRUE
      # unique id
      if(anyDuplicated(nodes$id)){
        stop("nodes must have unique ids")
      }
    }else if(is.list(nodes)){
      nodesToDataframe <- FALSE
    }else{
      stop("nodes must be a data.frame or a list")
    }
  } else {
    nodesToDataframe <- FALSE
  }
  
  if(!is.null(edges)){
    if(any(class(edges)%in%c("tbl_df", "tbl", "data.table"))){
      edges <- data.frame(edges)
    }
    if(is.data.frame(edges)){
      edgesToDataframe <- TRUE
    }else if(is.list(edges)){
      edgesToDataframe <- FALSE
    }else{
      stop("edges must be a data.frame or a list")
    }
  } else {
    edgesToDataframe <- FALSE
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
        main$style <- 'font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:20px;text-align:center;'
      }
    }else if(!inherits(main, "character")){
      stop("Invalid 'main' argument. Not a character")
    }else {
      main <- list(text = main, 
                   style = 'font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:20px;text-align:center;')
    }
  }
  
  # submain
  if(!is.null(submain)){
    if(is.list(submain)){
      if(any(!names(submain)%in%c("text", "style"))){
        stop("Invalid 'submain' argument")
      }
      if(!"text"%in%names(submain)){
        stop("Needed a 'text' value using a list for 'submain'")
      }
      if(!"style"%in%names(submain)){
        submain$style <- 'font-family:Georgia, Times New Roman, Times, serif;font-size:12px;text-align:center;'
      }
    }else if(!inherits(submain, "character")){
      stop("Invalid 'submain' argument. Not a character")
    }else {
      submain <- list(text = submain, 
                      style = 'font-family:Georgia, Times New Roman, Times, serif;font-size:12px;text-align:center;')
    }
  }
  
  # footer
  if(!is.null(footer)){
    if(is.list(footer)){
      if(any(!names(footer)%in%c("text", "style"))){
        stop("Invalid 'footer' argument")
      }
      if(!"text"%in%names(footer)){
        stop("Needed a 'text' value using a list for 'footer'")
      }
      if(!"style"%in%names(footer)){
        footer$style <- 'font-family:Georgia, Times New Roman, Times, serif;font-size:12px;text-align:center;'
      }
    }else if(!inherits(footer, "character")){
      stop("Invalid 'footer' argument. Not a character")
    }else {
      footer <- list(text = footer, 
                     style = 'font-family:Georgia, Times New Roman, Times, serif;font-size:12px;text-align:center;')
    }
  }
  
  if(!is.null(dot)){
    x <- list(dot = dot,
              options = list(width = '100%', height = "100%", nodes = list(shape = "dot"), 
                             manipulation = list(enabled = FALSE)),
              groups = NULL, width = width, height = height,
              idselection = list(enabled = FALSE),
              byselection = list(enabled = FALSE), main = main, 
              submain = submain, footer = footer, background = background)
    
  }else if(!is.null(gephi)){
    x <- list(gephi = jsonlite::fromJSON(txt = gephi, simplifyDataFrame = FALSE),
              options = list(width = '100%', height = "100%", nodes = list(shape = "dot"), 
                             manipulation = list(enabled = FALSE)),
              groups = NULL, width = width, height = height,
              idselection = list(enabled = FALSE),
              byselection = list(enabled = FALSE), main = main, 
              submain = submain, footer = footer, background = background)
  }else{
    
    # forward options using x
    groups = as.character(unique(nodes$group))
    if(length(groups) == 0){
      groups = NULL
    } else if(length(groups) == 1){
      groups = list(groups)
    }
    x <- list(nodes = nodes, edges = edges, nodesToDataframe = nodesToDataframe, 
              edgesToDataframe = edgesToDataframe, 
              options = list(width = '100%', height = "100%", nodes = list(shape = "dot"), 
                             manipulation = list(enabled = FALSE)),
              groups = groups, width = width, height = height,
              idselection = list(enabled = FALSE),
              byselection = list(enabled = FALSE), main = main, 
              submain = submain, footer = footer, background = background)
  }
  
  # previous legend control
  ctrl <- list(...)
  legend <- NULL
  if("legend"%in%names(ctrl)){
    warning("'legend' and 'legend.width' are deprecated (visNetwork >= 0.1.2). Please now prefer use visLegend function.")
    if(ctrl$legend){
      legend <- list()
      if("legend.width"%in%names(ctrl)){
        legend$width <- ctrl$legend.width
      }else{
        legend$width <- 0.2
      }
      legend$useGroups <- TRUE
      legend$position <- "left"
    }
  }
  
  x$legend <- legend
  
  # create widget
  htmlwidgets::createWidget(
    name = 'visNetwork',
    x,
    width = width,
    height = height,
    package = 'visNetwork'
  )
}