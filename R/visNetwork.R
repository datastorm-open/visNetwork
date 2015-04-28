#' Network visualization
#'
#' Network visualization using vis.js library
#'
#' @param nodes : data.frame with nodes informations. Needed at least column "id".
#' See \url{http://visjs.org/docs/network.html#Nodes} and \url{http://visjs.org/docs/network.html#Nodes_configuration}
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
#' See \url{http://visjs.org/docs/network.html#Edges} and \url{http://visjs.org/docs/network.html#Edges_configuration}
#' \itemize{
#'  \item{"from"}{ : node id of begin of the edge}
#'  \item{"to"}{ : node id of end of the edge}
#'  \item{"label"}{ : label of the edge}
#'  \item{"value"}{ : size of the node}
#'  \item{"title"}{ : tooltip of the node}
#'  \item{...}{}
#'}
#'
#' @param dot : Character DOT language.  See \url{http://visjs.org/docs/network.html#DOT_language}
#' 
#' @param gephi : Json export gephi path file.  See \url{http://visjs.org/docs/network.html#Gephi_import}
#' 
#' @param legend : Boolean. Default to FALSE. A little bit experimental. Put a legend in case of groups.
#' 
#' @param legend.width : Number. Default to 1. Bootstrap column width (from 1 to 12)
#' 
#' @examples
#'
#' # minimal example
#' nodes <- data.frame(id = 1:3)
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#'
#' visNetwork(nodes, edges)
#'
#' # more variables
#' nb <- 15
#' nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
#'  group = sample(LETTERS[1:3], nb, replace = TRUE), value = 1:nb,
#'  title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)
#'
#' edges <- data.frame(from = trunc(runif(nb)*(nb-1))+1,
#'  to = trunc(runif(nb)*(nb-1))+1,
#'  value = rnorm(nb, 10), label = paste("Edge", 1:nb),
#'  title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))
#'
#' # simple network
#' visNetwork(nodes, edges)
#'
#' # highlight nearest
#' visNetwork(nodes, edges) %>% visOptions(highlightNearest = TRUE)
#' 
#' # try a legend...
#' visNetwork(nodes, edges, legend = TRUE)
#' 
#' # try an id node selection 
#' visNetwork(nodes, edges) %>% 
#'  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
#' 
#' # directed network
#' visNetwork(nodes, edges) %>% visEdges(style = "arrow")
#'
#' # custom navigation
#' visNetwork(nodes, edges) %>% visOptions(navigation = TRUE)
#'
#' # data Manipulation
#' visNetwork(nodes, edges) %>% visOptions(dataManipulation = TRUE)
#'
#' # Hierarchical Layout
#' visNetwork(nodes, edges) %>% visHierarchicalLayout()
#'
#' # freeze network
#' visNetwork(nodes, edges) %>% visOptions(dragNetwork = FALSE, dragNodes = FALSE)
#'
#' # clustering
#' nodes <- data.frame(id = 1:100)
#' edges <- data.frame(from = round(runif(120)*100), to = round(runif(120)*100))
#'
#' visNetwork(nodes, edges) %>%
#'  visClustering(initialMaxNodes = 50, nodeScaling = list(width = 50, height = 50, radius = 50))
#'
#' # Save a network
#' network <- visNetwork(nodes, edges, legend = TRUE) %>% 
#'  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
#'  navigation = TRUE, dataManipulation = TRUE)
#'  
#' htmlwidgets::saveWidget(network, "network.html")
#' 
#' # DOT language
#' visNetwork(dot = 'dinetwork {1 -> 1 -> 2; 2 -> 3; 2 -- 4; 2 -> 1 }')
#' 
#' # gephi json file
#' gephiNetwork <- visNetwork(gephi = 'WorldCup2014.json') %>%
#'  visOptions(smoothCurves = list(dynamic = FALSE, type ="continuous"), stabilize = FALSE, 
#'  hideEdgesOnDrag = TRUE) %>% visEdges(width = 0.15, inheritColor = "from") %>%
#'  visPhysics(barnesHut = list(gravitationalConstant = -10000, springConstant = 0.002, springLength= 150))
#'
#' 
#' @seealso \link{visOptions}, \link{visNodes}, \link{visEdges}, \link{visGroups}, \link{visEvents}, ...
#'
#' @import htmlwidgets
#'
#' @export
#' 
visNetwork <- function(nodes = NULL, edges = NULL, dot = NULL, gephi = NULL, legend = FALSE, legend.width = 1,
                       width = NULL, height = NULL) {

  if(is.null(nodes) & is.null(edges) & is.null(dot) & is.null(gephi)){
    stop("Must 'dot' data, or 'gephi' data, or 'nodes' and 'edges' data.")
  }
  
  if(!is.null(dot)){
    x <- list(dot = dot,
              options = list(width = '100%', height = "100%", nodes = list(shape = "dot")),
              groups = NULL, legend = legend, legendWidth = legend.width, width = width, height = height)
  }else if(!is.null(gephi)){
    x <- list(gephi = rjson::fromJSON(file = gephi),
              options = list(width = '100%', height = "100%", nodes = list(shape = "dot")),
              groups = NULL, legend = legend, legendWidth = legend.width, width = width, height = height)
  }else{
    
    # forward options using x
    groups = as.character(unique(nodes$group))
    if(length(groups) == 0){
      groups = NULL
    }
    x <- list(nodes = nodes, edges = edges,
              options = list(width = '100%', height = "100%", nodes = list(shape = "dot")),
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

#' Widget output function for use in Shiny
#'
#' @export
visNetworkOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'visNetwork', width, height, package = 'visNetwork')
}

#' Widget render function for use in Shiny
#'
#' @export
renderVisNetwork <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, visNetworkOutput, env, quoted = TRUE)
}
