#' Network visualization groups options
#'
#' Network visualization groups options. See \url{http://visjs.org/docs/network.html#Groups_configuration}.
#'
#'
#' @param groupname : String. Name of target group.
#' @param color : String | named list.	Color for the node. Can be just one color, or a list with several elements :
#'\itemize{
#'  \item{"background"}{ : String. Default to '#97C2FC'. Background color for the node.}
#'  \item{"border"}{ : String. Default to '#2B7CE9'. Border color for the node.}
#'  \item{"highlight"}{ : String | named list, 	Color of the node when selected.
#'    \itemize{
#'      \item{"background"}{ : String. Default to '#97C2FC'. Background color for the node when selected.}
#'      \item{"border"}{ : String. Default to '#2B7CE9'. Border color for the node when selected.}
#'    }
#'  }
#'}
#' @param image : String. Default to none. Default image for the nodes. Only applicable in combination with shape image.
#' @param fontColor : String. Default to "black". Font color of the node.
#' @param fontFace : String. Default to "sans". Font name of the node, for example "verdana" or "arial".
#' @param fontSize : Number. Default to 14. Font size for the node in pixels.
#' @param fontStrokeWidth : Number. Default to 0. The width of the label stroke (border around label's text) in pixels.
#' @param fontStrokeColor : String. Default to "white". The color of the label stroke.
#' @param shape : String. Default to "ellipse". Choose from ellipse (default), circle, box, database, image, label, dot, star, triangle, triangleDown, and square. In case of image, a property with name image must be provided, containing image urls.
#' @param radius : Number. Default to 5. Default radius for the node. Only applicable in combination with shapes box and dot.
#'
#' @examples
#'
#' nodes <- data.frame(id = 1:10, label = paste("Label", 1:10), group = round(runif(10)))
#' edges <- data.frame(from = c(2,5,10), to = c(1,2,10))
#'
#' visNetwork(nodes, edges) %>%
#'  visGroups(groupname = "1", color = "red", shape = "database") %>%
#'  visGroups(groupname = "0", color = "yellow", shape = "label")
#'
#' @export
#'

visGroups <- function(graph,
                      groupname = NULL,
                      color = NULL,
                      image = NULL,
                      fontColor = NULL,
                      fontFace = NULL,
                      fontSize = NULL,
                      fontStrokeWidth = NULL,
                      fontStrokeColor = NULL,
                      shape = NULL,
                      radius = NULL){

  if(is.null(groupname)){
    stop("Must have a groupname to identify group")
  }

  groups <- list(list())
  names(groups) <- groupname

  groups[[1]]$color = color
  groups[[1]]$image = image
  groups[[1]]$fontColor = fontColor
  groups[[1]]$fontFace = fontFace
  groups[[1]]$fontSize = fontSize
  groups[[1]]$fontStrokeWidth = fontStrokeWidth
  groups[[1]]$fontStrokeColor = fontStrokeColor
  groups[[1]]$shape = shape
  groups[[1]]$radius = radius

  graph$x$options$groups <- mergeLists(graph$x$options$groups, groups)

  graph
}
