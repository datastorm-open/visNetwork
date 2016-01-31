#' Save a a visNetwork object to an HTML file
#'
#' Save a a visNetwork object to an HTML file for sharing with others. The HTML can
#' include it's dependencies in an adjacent directory or can bundle all
#' dependencies into the HTML file (via base64 encoding).
#'
#' @param graph : a visNetwork object
#'
#' @inheritParams htmlwidgets::saveWidget
#' 
#' @examples
#' 
#'\dontrun{
#'nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#'edges <- data.frame(from = c(1,2), to = c(2,3))
#'
#'network <- visNetwork(nodes, edges)
#'network
#'
#'network %>% visSave(file = "network.html")
#'# same as
#'visSave(network, file = "network.html")
#'
#'}
#' @export
visSave <- function(graph, file, selfcontained = TRUE) {
  htmlwidgets::saveWidget(graph, file, selfcontained)
}