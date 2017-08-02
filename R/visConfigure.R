#' Network visualization configure options
#'
#' Network visualization configure options. For full documentation, have a look at \link{visDocumentation}.
#' 
#' @param graph : a visNetwork object
#' @param enabled : Boolean. Default to true. Toggle the configuration interface on or off. This is an optional parameter. If left undefined and any of the other properties of this object are defined, this will be set to true.
#' @param filter : String, Array, Boolean, Function. Default to true. When a boolean, true gives you all options, false will not show any. If a string is supplied, any combination of the following is allowed: nodes, edges, layout, interaction, manipulation, physics, selection, renderer. Feel free to come up with a fun seperating character. Finally, when supplied an array of strings, any of the previously mentioned fields are accepted.
#' @param container : DOM element. This allows you to put the configure list in another HTML container than below the network.
#' @param showButton : Boolean. Default to true. Show the generate options button at the bottom of the configurator.
#'
#'@examples
#'
#'\dontrun{
#' 
#' nodes <- data.frame(id = 1:3, title = paste0("<p>", 1:3,"<br> tooltip</p>"))
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#' 
#' visNetwork(nodes, edges) %>%
#'  visConfigure(enabled = TRUE, filter = "interaction")
#'  
#' # using visNetworkEditor
#' custom_network <- visNetworkEditor(object = network)
#' custom_network
#' 
#' custom_network <- visNetworkEditor(object = network, filter = "nodes,edges")
#' custom_network
#'}
#' 
#'@seealso \link{visConfigure}, \link{visTree}, \link{visNetworkEditor}
#'
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
#'
#' @export
visConfigure <- function(graph,
                         enabled = NULL,
                         filter = NULL,
                         container = NULL,
                         showButton = NULL){

  if(!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))){
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  
  configure <- list()
  configure$enabled <- enabled
  configure$filter <- filter
  configure$container <- container
  configure$showButton <- showButton
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    options <- list(configure = configure)
    data <- list(id = graph$id, options = options)
    graph$session$sendCustomMessage("visShinyOptions",data)
  }else{
    graph$x$options$configure <- configure
  }
  graph
}
