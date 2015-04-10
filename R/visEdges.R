#' Network visualization edges options
#'
#' Network visualization edges options. See \url{http://visjs.org/docs/network.html#Edges_configuration}.
#'
#'
#' @param arrowScaleFactor : Number. Default to 1. If you are using arrows, this will scale the arrow. Values < 1 give smaller arrows, > 1 larger arrows. Default: 1.
#' @param color : String | named list.	Color for the edge. Can be just one color, or a list with several elements :
#'\itemize{
#'  \item{"color"}{ : String. Default to' #848484'. Color of the edge when not selected.}
#'  \item{"highlight"}{ : String. Default to '#848484'. Color of the edge when selected.}
#'  \item{"hover"}{ : String. Default to '#848484'. Color of the edge when selected.}
#'}
#' @param hoverWidth : Number. Default to 1.5. This determines the thickness of the edge if it is hovered over. This will only manifest when the hover option is enabled.
#' @param dash : String | named list. Containing properties for dashed lines. Available properties: length, gap, altLength.
#'\itemize{
#'  \item{"altLength"}{ : Number. Undefined. Length of the alternated dash in pixels on a dashed line. Specifying dash.altLength allows for creating a dashed line with a dash-dot style, for example when dash.length=10 and dash.altLength=5. See also the option dahs.length. Only applicable when the line style is dash-line.}
#'  \item{"length"}{ : Number. Default to 10. Length of a dash in pixels on a dashed line. Only applicable when the line style is dash-line.}
#'  \item{"gap"}{ : Number. Default to 5. Length of a gap in pixels on a dashed line. Only applicable when the line style is dash-line.}
#'}
#' @param fontColor : String. Default to '#343434'. Font color for the text label of the edge. Only applicable when property label is defined.
#' @param fontFace : String. Default to 'arial'. Font face for the text label of the edge, for example "verdana" or "arial". Only applicable when property label is defined.
#' @param fontSize : Number. Default to 14. Font size in pixels for the text label of the edge. Only applicable when property label is defined.
#' @param fontFill : String. Default to 'white'. Font fill for the background color of the text label of the edge. Only applicable when property label is defined.
#' @param fontStrokeWidth : Number. Default to 	0. The width of the label stroke (border around label's text) in pixels. Only applicable when property label is defined.
#' @param fontStrokeColor : String. Default to 'white'. The color of the label stroke. Only applicable when property label is defined.
#' @param inheritColor : String | Boolean 	from 	Possible values: "to","from", true, false. If this value is set to false, the edge color information is used. If the value is set to true or "from", the color data from the borders of the "from" node is used. If this value is "to", the color data from the borders of the "to" node is used.
#' @param labelAlignment : String. Default to 'horizontal'. Possible values: "line-above", "line-center", "line-below". The alignment of the label when drawn on the edge. If horizontal it will align the label absolute horizontial.
#' @param opacity  : 	Number. Default to 1.0. Possible values: [0 .. 1]. This opacity value is added on top of the color information. This only happens for the unselected state.
#' @param style : String. Default to 'line'. Define a line style for the edge. Choose from line (default), arrow, arrow-center, or dash-line.
#' @param width : Number. Default to 1. Width of the line in pixels. The width will override a specified value, if a value is specified too.
#' @param widthSelectionMultiplier : Number. Default to 2. Determines the thickness scaling of an selected edge. This is applied when an edge, or a node connected to it, is selected.
#' @param widthMin : Number. Default to 1. The minimum thickness of the line when using per-edge defined values. This does nothing if you have not defined a value.
#' @param widthMax : Number. Default to 15. The maximum thickness of the line when using per-edge defined values. This does nothing if you have not defined a value.
#'
#' @export

visEdges <- function(graph,
                     arrowScaleFactor = NULL,
                     color = NULL,
                     hoverWidth = NULL,
                     dash = NULL,
                     fontColor = NULL,
                     fontFace = NULL,
                     fontSize = NULL,
                     fontFill = NULL,
                     fontStrokeWidth = NULL,
                     fontStrokeColor = NULL,
                     inheritColor = NULL,
                     labelAlignment = NULL,
                     opacity = NULL,
                     style = NULL,
                     width = NULL,
                     widthSelectionMultiplier = NULL,
                     widthMin = NULL,
                     widthMax = NULL){

  edges <- list()

  edges$arrowScaleFactor <- arrowScaleFactor
  edges$color <- color
  edges$hoverWidth <- hoverWidth
  edges$dash <- dash
  edges$fontColor <- fontColor
  edges$fontFace <- fontFace
  edges$fontSize <- fontSize
  edges$fontFill <- fontFill
  edges$fontStrokeWidth <- fontStrokeWidth
  edges$fontStrokeColor <- fontStrokeColor
  edges$inheritColor <- inheritColor
  edges$labelAlignment <- labelAlignment
  edges$opacity <- opacity
  edges$style <- style
  edges$width <- width
  edges$widthSelectionMultiplier <- widthSelectionMultiplier
  edges$widthMin <- widthMin
  edges$widthMax <- widthMax

  graph$x$options$edges <- mergeLists(graph$x$options$edges, edges)

  graph
}
