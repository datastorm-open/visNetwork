#' Network visualization full options setter
#'
#' Network visualization full options setter. Using this function, you can pass all network options you want,
#' respecting the library format rather than use \link{visNodes}, \link{visEdges},  \link{visGroups}....
#' There is no control, so it's at your own risk !
#'
#' @param graph : a visNetwork object
#' @param options : a named list with all options you want to add to your network.
#' 
#' @examples
#' nodes <- data.frame(id = 1:3)
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#' 
#' # using visNetwork functions
#' visNetwork(nodes, edges) %>% visNodes(shape = "square", color = "red") %>%
#'    visEdges(arrows = "to")
#' 
#' # directly use visSetOptions
#' visNetwork(nodes, edges) %>% 
#'  visSetOptions(options = list(nodes = list(shape = "square", color = "red"),
#'                               edges = list(arrows = "to")))
#' 
#' 
#' 
#' @export
#' @references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visSetOptions <- function(graph, options = NULL){

  if(!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))){
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    data <- list(id = graph$id, options = options)
    graph$session$sendCustomMessage("visShinyOptions", data)
  }else{
    graph$x$options <- mergeLists(graph$x$options, options)
  }
  graph
}
