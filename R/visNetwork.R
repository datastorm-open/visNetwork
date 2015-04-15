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
#' @param highlight.nearest : Highlight nearest when clicking a node ? Default to TRUE. Based on \url{http://visjs.org/examples/network/29_neighbourhood_highlight.html}
#' This options use click event
#' 
#' @param legend : Boolean. Default to FALSE. A little bit experimental. Put a legend in case of groups.
#' 
#' @param id.selection : Boolean. Default to FALSE. A little bit experimental. Add an id node selection.
#' 
#' @examples
#'
#' # minimal example
#' nodes <- data.frame(id = 1:3)
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#'
#' visNetwork(nodes, edges, highlight.nearest = FALSE)
#'
#' # more variables
#' nb = 15
#' nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
#'  group = sample(1:nb, nb, replace = TRUE), value = 1:nb,
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
#' # try a legend...
#' visNetwork(nodes, edges, legend = TRUE)
#' 
#' # try an id node selection 
#' visNetwork(nodes, edges, id.selection = TRUE)
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
#' @seealso \link{visOptions}, \link{visNodes}, \link{visEdges}, \link{visGroups}, \link{visEvents}, ...
#'
#' @import htmlwidgets
#'
#' @export
visNetwork <- function(nodes, edges, highlight.nearest = TRUE, legend = FALSE, id.selection = FALSE,
                       width = "100%", height = "400px") {

  # forward options using x
  
  groups = as.character(unique(nodes$group))
  if(length(groups) == 0){
    groups = NULL
  }
  x = list(nodes = dataToJSON(nodes), edges = dataToJSON(edges),
           options = list(width = '100%', height = "100%", nodes = list(shape = "dot")),
           highlight = highlight.nearest, groups = groups, legend = legend,
           idselection = id.selection, width = width, height = height )

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
