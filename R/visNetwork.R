#' Network visualization
#'
#' Network visualization using vis.js library. For full documentation, have a look at \link{visDocumentation}.
#'
#' @param nodes : data.frame with nodes informations. Needed at least column "id". See \link{visNodes} 
#' \itemize{
#'  \item{"id"}{ : id of the node, needed in edges information}
#'  \item{"label"}{ : label of the node}
#'  \item{"group"}{ : group of the node. Groups can be configure with \link{visGroups}}
#'  \item{"value"}{ : size of the node}
#'  \item{"title"}{ : tooltip of the node}
#'  \item{...}{}
#'}
#'
#' @param edges : data.frame with edges informations. Needed at least columns "from" and "to".
#' See See \link{visEdges}
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
#' @param legend : Boolean. Default to FALSE. A little bit experimental. Put a legend in case of groups.
#' 
#' @param legend.width : Number, in [0,...,1]. Default to 0.2
#' 
#' @param width	: Width (optional, defaults to automatic sizing)
#' 
#' @param height	: Height (optional, defaults to automatic sizing)
#' 
#' @examples
#'
#' # minimal example
#' nodes <- data.frame(id = 1:3)
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#'
#' visNetwork(nodes, edges)
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
#' # highlight nearest
#' nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
#'  group = sample(LETTERS[1:3], 15, replace = TRUE))
#'
#' edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
#'  to = trunc(runif(15)*(15-1))+1)
#'  
#' visNetwork(nodes, edges) %>% visOptions(highlightNearest = TRUE)
#' 
#' # try a legend...
#' visNetwork(nodes, edges, legend = TRUE)
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
#' # directed network
#' visNetwork(nodes, edges) %>% 
#'  visEdges(arrow = 'from', scaling = list(min = 2, max = 2))
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
#' network <- visNetwork(nodes, edges, legend = TRUE) %>% 
#'  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
#'  manipulation = TRUE)
#'  
#' htmlwidgets::saveWidget(network, "network.html")
#' }
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
#' @seealso \link{visOptions}, \link{visNodes}, \link{visEdges}, \link{visGroups}, \link{visEvents}
#'
#' @import htmlwidgets
#' 
#' @importFrom rjson fromJSON
#'
#' @export
#' 
visNetwork <- function(nodes = NULL, edges = NULL, dot = NULL, gephi = NULL, legend = FALSE, legend.width = 0.2,
                       width = NULL, height = NULL) {

  if(is.null(nodes) & is.null(edges) & is.null(dot) & is.null(gephi)){
    stop("Must 'dot' data, or 'gephi' data, or 'nodes' and 'edges' data.")
  }
  
  if(!is.null(dot)){
    x <- list(dot = dot,
              options = list(width = '100%', height = "100%", nodes = list(shape = "dot"), manipulation = list(enabled = FALSE)),
              groups = NULL, legend = legend, legendWidth = legend.width, width = width, height = height)
  }else if(!is.null(gephi)){
    x <- list(gephi = rjson::fromJSON(file = gephi),
              options = list(width = '100%', height = "100%", nodes = list(shape = "dot"), manipulation = list(enabled = FALSE)),
              groups = NULL, legend = legend, legendWidth = legend.width, width = width, height = height)
  }else{
    
    # forward options using x
    groups = as.character(unique(nodes$group))
    if(length(groups) == 0){
      groups = NULL
    }
    x <- list(nodes = nodes, edges = edges,
              options = list(width = '100%', height = "100%", nodes = list(shape = "dot"), manipulation = list(enabled = FALSE)),
              groups = groups, legend = legend, legendWidth = legend.width, width = width, height = height)
  }

  # create widget
  htmlwidgets::createWidget(
    name = 'visNetwork',
    x,
    width = width,
    height = height,
    package = 'visNetwork'
  )
}

#' Shiny bindings for visNetwork
#' 
#' Output and render functions for using visNetwork within Shiny 
#' applications and interactive Rmd documents.
#' 
#' @param outputId : output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#' @param expr An expression that generates a visNetwork
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This 
#'   is useful if you want to save an expression in a variable.
#'   
#' @name visNetwork-shiny
#'
#' @export
visNetworkOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'visNetwork', width, height, package = 'visNetwork')
}

#' @rdname visNetwork-shiny
#' @export
renderVisNetwork <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, visNetworkOutput, env, quoted = TRUE)
}

