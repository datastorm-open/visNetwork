#' Network visualization edges options
#'
#' Network visualization edges options
#'
#' @param title : String. Default to undefined. The title is shown in a pop-up when the mouse moves over the edge.
#' @param value : Number. Default to undefined. When a value is set, the edges' width will be scaled using the options in the scaling object defined above.
#' @param label : String. Default to undefined. The label of the edge. HTML does not work in here because the network uses HTML5 Canvas.
#' @param length : Number. Default to undefined. The physics simulation gives edges a spring length. This value can override the length of the spring in rest.
#' @param width : Number. Default to 1. The width of the edge. If value is set, this is not used.
#' @param dashes : Array or Boolean. Default to false. When true, the edge will be drawn as a dashed line. You can customize the dashes by supplying an Array. Array formart: Array of numbers, gap length, dash length, gap length, dash length, ... etc. The array is repeated until the distance is filled. When using dashed lines in IE versions older than 11, the line will be drawn straight, not smooth.
#' @param hidden : Boolean. Default to false. When true, the edge is not drawn. It is part still part of the physics simulation however!
#' @param hoverWidth : Number or Function. Default to 0.5. Assuming the hover behaviour is enabled in the interaction module, the hoverWidth determines the width of the edge when the user hovers over it with the mouse. If a number is supplied, this number will be added to the width. Because the width can be altered by the value and the scaling functions, a constant multiplier or added value may not give the best results. To solve this, you can supply a function.
#' @param id : String. Default to undefined. The id of the edge. The id is optional for edges. When not supplied, an UUID will be assigned to the edge.
#' @param physics : Boolean. Default to true. When true, the edge is part of the physics simulation. When false, it will not act as a spring.
#' @param selectionWidth : Number or Function. Default to 1. The selectionWidth determines the width of the edge when the edge is selected. If a number is supplied, this number will be added to the width. Because the width can be altered by the value and the scaling functions, a constant multiplier or added value may not give the best results. To solve this, you can supply a function.
#' @param selfReferenceSize : Number. Default to false.	When the to and from nodes are the same, a circle is drawn. This is the radius of that circle.
#' 
#' @seealso \url{../doc/network/nodes.html}
#' 
#' @export

visEdges <- function(graph,
                     title = NULL,
                     value = NULL,
                     label = NULL,
                     length = NULL,
                     width = NULL,
                     dashes = NULL,
                     hidden = NULL,
                     hoverWidth = NULL,
                     id = NULL,
                     physics = NULL,
                     selectionWidth = NULL,
                     selfReferenceSize = NULL){

  edges <- list()

  edges$title <- title
  edges$value <- value
  edges$label <- label
  edges$length <- length
  edges$width <- width
  edges$dashes <- dashes
  edges$hidden <- hidden
  edges$hoverWidth <- hoverWidth
  edges$id <- id
  edges$physics <- physics
  edges$selectionWidth <- selectionWidth
  edges$selfReferenceSize <- selfReferenceSize

  graph$x$options$edges <- mergeLists(graph$x$options$edges, edges)

  graph
}
