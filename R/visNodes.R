#' Network visualization nodes options
#'
#' Network visualization nodes options. For full documentation, have a look at \link{visDocumentation}.
#'
#' @param graph : a visNetwork object
#' @param id : String. Default to undefined. The id of the node. The id is mandatory for nodes and they have to be unique. This should obviously be set per node, not globally.
#' @param shape : String. Default to 'ellipse'. The shape defines what the node looks like. There are two types of nodes. One type has the label inside of it and the other type has the label underneath it. The types with the label inside of it are: ellipse, circle, database, box, text. The ones with the label outside of it are: image, circularImage, diamond, dot, star, triangle, triangleDown, hexagon, square and icon.
#' @param size : Number. Default to 25. The size is used to determine the size of node shapes that do not have the label inside of them. These shapes are: image, circularImage, diamond, dot, star, triangle, triangleDown, hexagon, square and icon
#' @param title : String or Element. Default to undefined. Title to be displayed when the user hovers over the node. The title can be an HTML element or a string containing plain text or HTML.
#' @param value : Number. Default to undefined. When a value is set, the nodes will be scaled using the options in the scaling object defined above.
#' @param x : Number. Default to undefined. This gives a node an initial x position. When using the hierarchical layout, either the x or y position is set by the layout engine depending on the type of view. The other value remains untouched. When using stabilization, the stabilized position may be different from the initial one. To lock the node to that position use the physics or fixed options.
#' @param y : Number. Default to undefined. This gives a node an initial y position. When using the hierarchical layout, either the x or y position is set by the layout engine depending on the type of view. The other value remains untouched. When using stabilization, the stabilized position may be different from the initial one. To lock the node to that position use the physics or fixed options. 
#' @param label : String. Default to undefined. The label is the piece of text shown in or under the node, depending on the shape.
#' @param level : Number. Default to undefined. When using the hierarchical layout, the level determines where the node is going to be positioned.
#' @param group : String. Default to undefined. When not undefined, the node will belong to the defined group. Styling information of that group will apply to this node. Node specific styling overrides group styling. 
#' @param hidden : Boolean. Default to false. When true, the node will not be shown. It will still be part of the physics simulation though!
#' @param image : List or String. Default to undefined. When the shape is set to image or circularImage, this option should be the URL to an image. If the image cannot be found, the brokenImage option can be used.
#'    \itemize{
#'      \item{"unselected"}{ : String. Unselected (default) image URL.}
#'      \item{"selected"}{ : String. Selected image URL.}
#'    }
#' @param mass : Number. Default to 1. The barnesHut physics model (which is enabled by default) is based on an inverted gravity model. By increasing the mass of a node, you increase it's repulsion. Values lower than 1 are not recommended.
#' @param physics : Boolean. Default to true. When false, the node is not part of the physics simulation. It will not move except for from manual dragging.
#' @param borderWidth : Number. Default to 1. The width of the border of the node when it is not selected, automatically limited by the width of the node.
#' @param borderWidthSelected : Number. Default to 2. The width of the border of the node when it is selected. If left at undefined, double the borderWidth will be used.
#' @param brokenImage : String. Undefined. When the shape is set to image or circularImage, this option can be an URL to a backup image in case the URL supplied in the image option cannot be resolved
#' @param labelHighlightBold : Boolean. Default to true. Determines whether or not the label becomes bold when the node is selected.
#' @param color : String | named list.	Color for the node. Can be 'rgba(120,32,14,1)', '#D2E5FF' (hexa notation on 7 char without transparency) or 'red'. Can be just one color, or a list with several elements :
#' \itemize{
#'  \item{"background"}{ : String. Default to '#D2E5FF'. Background color for the node.}
#'  \item{"border"}{ : String. Default to '#2B7CE9'. Border color for the node.}
#'  \item{"highlight"}{ : String | named list, 	Color of the node when selected.
#'    \itemize{
#'      \item{"background"}{ : String. Default to '#D2E5FF'. Background color for the node when selected.}
#'      \item{"border"}{ : String. Default to '#2B7CE9'. Border color for the node when selected.}
#'    }
#'  }
#'  \item{"hover"}{ : named list, when the hover option is enabled
#'    \itemize{
#'      \item{"background"}{ : String. Default to '#D2E5FF'. Background color of the node when the node is hovered over and the hover option is enabled.}
#'      \item{"border"}{ : String. Default to '#2B7CE9'. Border color of the node when the node is hovered over and the hover option is enabled.}
#'    }
#'  }
#'}
#' @param opacity :	Number. Overall opacity of a node (overrides any opacity on border, background, image, and shadow)
#' @param fixed : Boolean | named list. Default to false. When true, the node will not move but IS part of the physics simulation. When defined as an list, movement in either X or Y direction can be disabled.
#' \itemize{
#'  \item{"x"}{ : Boolean. When true, the node will not move in the X direction.}
#'  \item{"y"}{ : Boolean. When true, the node will not move in the Y direction.}
#'}
#'
#' @param font : Named list or String. This object defines the details of the label. A shorthand is also supported in the form 'size face color' for example: '14px arial red'
#' \itemize{
#'  \item{"color"}{ : String. Default to '#343434'. Color of the label text.}
#'  \item{"size"}{ : Number. Default to 14. Size of the label text.}
#'  \item{"face"}{ : String. Default to 'arial. Font face (or font family) of the label text.}
#'  \item{"background"}{ : String. Default to undefined. When not undefined but a color string, a background rectangle will be drawn behind the label in the supplied color.}
#'  \item{"strokeWidth"}{ : Number. Default to 0. As an alternative to the background rectangle, a stroke can be drawn around the text. When a value higher than 0 is supplied, the stroke will be drawn.}
#'  \item{"strokeColor"}{ : String. Default to '#ffffff'. This is the color of the stroke assuming the value for stroke is higher than 0.}
#'  \item{"align"}{ : String. Default to 'center'. This can be set to 'left' to make the label left-aligned}
#'  \item{"vadjust, multi, bold, ital, boldital, mono"}{See \link{visDocumentation}}
#'}
#'
#' @param icon : Named list. These options are only used when the shape is set to 'icon'. See \link{addFontAwesome}, \link{addIonicons}
#' \itemize{
#'  \item{"face"}{ : String. Default to 'FontAwesome'. These options are only used when the shape is set to icon. The possible options for the face are: 'FontAwesome', "'Font Awesome 5 Free'", and 'Ionicons'.}
#'  \item{"code"}{ : String. Default to undefined. This is the code of the icon, for example '\\uf007'.}
#'  \item{"size"}{ : Number. Default to 50. The size of the icon.}
#'  \item{"color"}{ : String. Default to '#2B7CE9'. The color of the icon.}
#'  \item{"weight"}{ : Number or String. Default to undefined. This allows for weight to be forced regardless of selection status. For example Font Awesome 5 doesn't work properly unless weight is forced to 'bold' or 700 (This is done automatically in visNetwork). If this option is set then selection is indicated by bigger size instead of bold font face. }
#'}
#'
#' @param shadow : Boolean | named list. Default to false. When true, the node casts a shadow using the default settings. This can be further refined by supplying a list
#' \itemize{
#'  \item{"enabled"}{ : Boolean. Default to false. Toggle the casting of shadows. If this option is not defined, it is set to true if any of the properties in this object are defined.}
#'  \item{"color"}{	: String. Default to	'rgba(0,0,0,0.5)'.	The color of the shadow as a string. Supported formats are 'rgb(255,255,255)', 'rgba(255,255,255,1)' and '#FFFFFF'.}
#'  \item{"size"}{ : Number. Default to 10. The blur size of the shadow.}
#'  \item{"x"}{ : Number. Default to 5. The x offset.}
#'  \item{"y"}{ : Number. Default to 5. The y offset.}
#'}
#'
#' @param scaling : Named list.  If the value option is specified, the size of the nodes will be scaled according to the properties in this object.
#' \itemize{
#'  \item{"min"}{ :  Number. Default to 10. If nodes have a value, their sizes are determined by the value, the scaling function and the min max values.}
#'  \item{"max"}{ : Number. Default to 30. This is the maximum allowed size when the nodes are scaled using the value option.}
#'  \item{"label"}{ : Named list or Boolean. Default to Named list. This can be false if the label is not allowed to scale with the node. If true it will scale using default settigns. For further customization, you can supply an object.
#'    \itemize{
#'      \item{"enabled"}{ : Boolean. Default to false. Toggle the scaling of the label on or off. If this option is not defined, it is set to true if any of the properties in this object are defined.}
#'      \item{"min"}{ : Number. Default to 14. The minimum font-size used for labels when scaling.}
#'      \item{"max"}{ : Number. Default to 30. The maximum font-size used for labels when scaling.}
#'      \item{"maxVisible"}{ :   Number. Default to 30. When zooming in, the font is drawn larger as well. You can limit the perceived font size using this option. If set to 30, the font will never look larger than size 30 zoomed at 100\%.}
#'      \item{"drawThreshold"}{ : Number. Default to 5. When zooming out, the font will be drawn smaller. This defines a lower limit for when the font is drawn. When using font scaling, you can use this together with the maxVisible to first show labels of important nodes when zoomed out and only show the rest when zooming in.}
#'    }
#'  }
#'  \item{"customScalingFunction"}{ : Function. If nodes have value fields, this function determines how the size of the nodes are scaled based on their values.}
#'}
#'
#' @param shapeProperties : See \link{visDocumentation}  
#'  
#' @param heightConstraint : See \link{visDocumentation}  
#' @param widthConstraint : See \link{visDocumentation}  
#' @param margin : See \link{visDocumentation} 
#' @param chosen : See \link{visDocumentation}  
#' @param imagePadding : See \link{visDocumentation}  
#' @param ctxRenderer : See \link{visDocumentation}  
#' 
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#' 
#' \dontrun{
#' nodes <- data.frame(id = 1:3)
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#' 
#' visNetwork(nodes, edges) %>% 
#'   visNodes(shape = "square", title = "I'm a node", borderWidth = 3)
#' 
#' visNetwork(nodes, edges) %>% 
#'   visNodes(color = list(hover = "green")) %>%
#'   visInteraction(hover = TRUE)
#' 
#' 
#' visNetwork(nodes, edges) %>% visNodes(color = "red")
#' 
#' visNetwork(nodes, edges) %>% 
#'   visNodes(color = list(background = "red", border = "blue", 
#'     highlight = "yellow"))
#' 
#' visNetwork(nodes, edges) %>% visNodes(shadow = TRUE)
#' 
#' visNetwork(nodes, edges) %>% visNodes(shadow = list(enabled = TRUE, size = 50))
#' 
#' }
#' 
#' @export
#' @references See online documentation \url{https://datastorm-open.github.io/visNetwork/}
visNodes <- function(graph,
                     id = NULL,
                     shape = NULL,
                     size = NULL,
                     title = NULL,
                     value = NULL,
                     x = NULL,
                     y = NULL,
                     label = NULL,
                     level = NULL,
                     group = NULL,
                     hidden = NULL,
                     image = NULL,
                     mass = NULL,
                     physics = NULL,
                     borderWidth = NULL,
                     borderWidthSelected = NULL,
                     brokenImage = NULL,
                     labelHighlightBold = NULL,
                     color = NULL,
                     opacity = NULL,
                     fixed = NULL,
                     font = NULL,
                     icon = NULL, 
                     shadow = NULL,
                     scaling = NULL, 
                     shapeProperties = NULL, 
                     heightConstraint = NULL,
                     widthConstraint = NULL,
                     margin = NULL,
                     chosen = NULL, 
                     imagePadding = NULL, 
                     ctxRenderer = NULL){

  if(!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))){
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  
  nodes <- list()

  nodes$id <- id
  nodes$shape <- shape
  nodes$size <- size
  nodes$title <- title
  nodes$value <- value
  nodes$x <- x
  nodes$y <- y
  nodes$label <- label
  nodes$level <- level
  nodes$group <- group
  nodes$hidden <- hidden
  nodes$image <- image
  nodes$imagePadding <- imagePadding
  nodes$mass <- mass
  nodes$physics <- physics
  nodes$borderWidth <- borderWidth
  nodes$borderWidthSelected <- borderWidthSelected
  nodes$brokenImage <- brokenImage
  nodes$labelHighlightBold <- labelHighlightBold
  nodes$color <- color
  nodes$opacity <- opacity
  nodes$fixed <- fixed
  nodes$font <- font
  nodes$icon <- icon
  nodes$shadow <- shadow
  nodes$shapeProperties <- shapeProperties
  nodes$margin <- margin
  nodes$chosen <- chosen
  nodes$widthConstraint <- widthConstraint
  nodes$heightConstraint <- heightConstraint
  
  if(!is.null(scaling)){
    if("customScalingFunction"%in%names(scaling)){
      scaling$customScalingFunction <- JS(scaling$customScalingFunction)
    }
  }
  
  nodes$scaling <- scaling
  nodes$ctxRenderer <- ctxRenderer
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    options <- list(nodes = nodes)
    data <- list(id = graph$id, options = options)
    graph$session$sendCustomMessage("visShinyOptions",data)
  }else{
    graph$x$options$nodes <- mergeLists(graph$x$options$nodes, nodes)
  }
  graph
}
