#' Network visualization tooltip options
#'
#' Network visualization tooltip options. See \url{http://visjs.org/docs/network.html#Tooltips}.
#'
#' @param delay : Number. Default to 300. Time in milliseconds a user must hover over a node or edge before a tooltip appears.
#' @param fontColor : String. Default to "black". Default color for tooltip text.
#' @param fontSize : Number. Default to 14. Size in pixels of tooltip text.
#' @param fontFace : String. Default to "verdana". Font family to used for tooltip text.
#' @param color.background : String. Default to "#FFFFFF". Background color for the node.
#' @param color.border : String. Default to "#666". Border color for the node.
#'
#' @examples
#'
#' nodes <- data.frame(id = 1:3, title = paste0("<p>", 1:3,"<br> tooltip</p>"))
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#'
#' visNetwork(nodes, edges) %>%
#'  visTooltips(delay = 100, color.background = "red", fontColor = "white")
#
#'
#' @export
visTooltips <- function(graph, delay = 300,
                        fontColor = "black",
                        fontSize = 14,
                        fontFace = "verdana",
                        color.background = "#FFFFFF",
                        color.border = "#666"){

  tooltip <- list()
  tooltip$delay <- delay
  tooltip$fontColor <- fontColor
  tooltip$fontSize <- fontSize
  tooltip$fontFace <- fontFace
  tooltip$color <- list(background = color.background, border = color.border)

  graph$x$options$tooltip <- tooltip

  graph

}
