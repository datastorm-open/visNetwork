#' Network visualization nodes options
#'
#' Network visualization nodes options \url{http://visjs.org/docs/network.html#Nodes_configuration}
#'
#'
#' @param borderWidth : Number. Default to 1. The width of the border of the node when it is not selected, automatically limited by the width of the node.
#' @param borderWidthSelected : Number. Undefined. The width of the border of the node when it is selected. If left at undefined, double the borderWidth will be used.
#' @param customScalingFunction : Function. This is a function you can override to make the nodes scale the way you want them based on their values.
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
#'  \item{"hover"}{ : named list, when the hover option is enabled
#'    \itemize{
#'      \item{"background"}{ : String. Default to '#2B7CE9'. Border color of the node when selected.}
#'      \item{"border"}{ : String. Default to '#2B7CE9'. Border color of the node when the node is hovered over and the hover option is enabled.}
#'    }
#'  }
#'}
#' @param fontColor : String. Default to 'black'. Font color for label in the node.
#' @param fontFace : String. Default to 'verdana'. Font face for label in the node, for example "verdana" or "arial".
#' @param fontSize : Number. Default to 14. Font size in pixels for label in the node.
#' @param scaleFontWithValue : Boolean. Default to false. When using values, you can let the font scale with the size of the nodes if you enable the this option.
#' @param fontSizeMin : Number. Default to 14. When using values, you can let the font scale with the size of the nodes if you enable the scaleFontWithValue option. This is the minimum value of the fontSize.
#' @param fontSizeMax : Number. Default to 30. When using values, you can let the font scale with the size of the nodes if you enable the scaleFontWithValue option. This is the maximum value of the fontSize.
#' @param fontSizeMaxVisible : Number. Default to 30. When using values, you can let the font scale with the size of the nodes if you enable the scaleFontWithValue option. If you have a wide distribution of values and have a large max fontSize, the text will become huge if you zoom in on it. This option limits the percieved fontSize to avoid this. If you set it to 20, no label will be larger than fontsize 20 (at scale = 1) regardless of the scale.
#' @param fontDrawThreshold : Number. Default to 3. When zooming out, the text becomes smaller. This option sets the minimum size of the label before not being drawn. Just like the fontSizeMaxVisible option, this is the relative fontSize (fontSize * scale). You can combine this with the min and max values to have the labels of influential nodes show earlier when zooming in.
#' @param fontFill : String. Undefined. If a color is supplied, there will be a background color behind the label. If left undefined, no background color is shown.
#' @param fontStrokeWidth : Number. Default to 0. The width of the label stroke (border around label's text) in pixels.
#' @param fontStrokeColor : String. Default to 'white'. The color of the label stroke.
#' @param shape : String. Default to 'ellipse'. Define the shape for the node. Choose from ellipse (default), circle, box, database, image, circularImage, label, dot, star, triangle, triangleDown, square and icon. The shapes dot, star, triangle, triangleDown, and square, are scalable. The size is determined by the properties radius or value.
#' @param image : String. Undefined. Default image url for the nodes. only applicable to shape image.
#' @param brokenImage : String. Undefined. Image url to use in the event that the url specified in the image property fails to load. only applicable to shape image.
#' @param mass : Number. Default to 1. When using the Barnes Hut simulation method (which is selected by default), the mass of a node determines the gravitational repulsion during the simulation. Higher mass will push other nodes further away. Preferably use the physics configuration to alter the simulation.
#' @param widthMin : Number. Default to 16. The minimum width for a scaled image. Only applicable to shape image. This only does something if you supply a value.
#' @param widthMax : Number. Default to 64. The maximum width for a scaled image. Only applicable to shape image. This only does something if you supply a value.
#' @param radius : Number. Default to 10. The default radius for a node. Only applicable to shapes dot, star, triangle, triangleDown, and square.
#' @param radiusMin : Number. Default to 10. The minimum radius for a scaled node. Only applicable to shapes dot, star, triangle, triangleDown, and square. This only does something if you supply a value.
#' @param radiusMax : Number. Default to 30. The maximum radius for a scaled node. Only applicable to shapes dot, star, triangle, triangleDown, and square. This only does something if you supply a value.
#' @param iconFontFace : String. Undefined. Font face for icons, for example FontAwesome or Ionicon. You have to link to the css defining the font by yourself (see Examples)
#' @param icon : String. Undefined. Unicode of the icon f.e. (user-icon in FontAwesome)
#' @param iconSize : Number. Default to 50. Size of the icon
#'
#' @export

visNodes <- function(graph,
                     borderWidth = NULL,
                     borderWidthSelected = NULL,
                     customScalingFunction = NULL,
                     color = NULL,
                     fontColor = NULL,
                     fontFace = NULL,
                     fontSize = NULL,
                     scaleFontWithValue = NULL,
                     fontSizeMin = NULL,
                     fontSizeMax = NULL,
                     fontSizeMaxVisible = NULL,
                     fontDrawThreshold = NULL,
                     fontFill = NULL,
                     fontStrokeWidth = NULL,
                     fontStrokeColor = NULL,
                     shape = NULL,
                     image = NULL,
                     brokenImage = NULL,
                     mass = NULL,
                     widthMin = NULL,
                     widthMax = NULL,
                     radius = NULL,
                     radiusMin = NULL,
                     radiusMax = NULL,
                     iconFontFace = NULL,
                     icon = NULL,
                     iconSize = NULL){

  nodes <- list()

  nodes$borderWidth <- borderWidth
  nodes$borderWidthSelected <- borderWidthSelected
  nodes$customScalingFunction <- JS(customScalingFunction)
  nodes$color <- color
  nodes$fontColor <- fontColor
  nodes$fontFace <- fontFace
  nodes$fontSize <- fontSize
  nodes$scaleFontWithValue <- scaleFontWithValue
  nodes$fontSizeMin <- fontSizeMin
  nodes$fontSizeMax <- fontSizeMax
  nodes$fontSizeMaxVisible <- fontSizeMaxVisible
  nodes$fontDrawThreshold <- fontDrawThreshold
  nodes$fontFill <- fontFill
  nodes$fontStrokeWidth <- fontStrokeWidth
  nodes$fontStrokeColor <- fontStrokeColor
  nodes$shape <- shape
  nodes$image <- image
  nodes$brokenImage <- brokenImage
  nodes$mass <- mass
  nodes$widthMin <- widthMin
  nodes$widthMax <- widthMax
  nodes$radius <- radius
  nodes$radiusMin <- radiusMin
  nodes$radiusMax <- radiusMax
  nodes$iconFontFace <- iconFontFace
  nodes$icon <- icon
  nodes$iconSize <- iconSize

  graph$x$options$nodes <- mergeLists(graph$x$options$nodes, nodes)

  graph
}
