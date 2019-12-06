#' Save a a visNetwork object to an HTML file
#'
#' Save a a visNetwork object to an HTML file for sharing with others. The HTML can
#' include it's dependencies in an adjacent directory or can bundle all
#' dependencies into the HTML file (via base64 encoding).
#'
#' @param graph : a visNetwork object
#' @param file : File to save HTML into. See \link{saveWidget}
#' @param selfcontained	: Whether to save the HTML as a single self-contained file (with external resources base64 encoded) or a file with external resources placed in an adjacent directory.
#' @param background : Text string giving the html background color of the widget. Defaults to white.
#' 
#' 
#' @examples
#' 
#'\dontrun{
#'
#'nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#'edges <- data.frame(from = c(1,2), to = c(2,3))
#'
#'network <- visNetwork(nodes, edges)
#'network
#'
#'network %>% visSave(file = "network.html", background = "black")
#'
#'# same as
#'visSave(network, file = "network.html", background = "black")
#'
#'}
#' @export
#' 
#' @seealso \link{visExport}
#'
#' @references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visSave <- function(graph, file, selfcontained = TRUE, background = "white") {
  htmlwidgets::saveWidget(graph, file, selfcontained = selfcontained, background = background)
}