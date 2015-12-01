#' Network visualization interaction
#'
#' Network visualization interaction. For full documentation, have a look at \link{visDocumentation}.
#'
#'@param graph : a visNetwork object
#'@param dragNodes : Boolean. Default to true. When true, the nodes that are not fixed can be dragged by the user.
#'@param dragView : Boolean. Default to true. When true, the view can be dragged around by the user.
#'@param hideEdgesOnDrag : Boolean. Default to false. When true, the edges are not drawn when dragging the view. This can greatly speed up responsiveness on dragging, improving user experience.
#'@param hideNodesOnDrag : Boolean. Default to false. When true, the nodes are not drawn when dragging the view. This can greatly speed up responsiveness on dragging, improving user experience.
#'@param hover : Boolean. Default to false. When true, the nodes use their hover colors when the mouse moves over them.
#'@param hoverConnectedEdges : Boolean. Default to true. When true, on hovering over a node, it's connecting edges are highlighted.
#'@param keyboard : Just a Boolean, or a named list. When true, the keyboard shortcuts are enabled with the default settings. For further customization, you can supply an object.
#'\itemize{
#'  \item{"enabled"}{ : Boolean. Default to false.	Toggle the usage of the keyboard shortcuts. If this option is not defined, it is set to true if any of the properties in this object are defined.}
#'  \item{"speed"}{ : a named list
#'   \itemize{
#'    \item{"x"}{ : Number. Default to 1. This defines the speed of the camera movement in the x direction when using the keyboard navigation.}
#'    \item{"y"}{ : Number. Default to 1. This defines the speed of the camera movement in the y direction when using the keyboard navigation.}
#'    \item{"zoom"}{ : Number. Default to 0.02. This defines the zoomspeed when using the keyboard navigation.Number   0.02   This defines the zoomspeed when using the keyboard navigation.}
#'    }
#'  }
#'  \item{"bindToWindow"}{ : Boolean. Default to true. If this is true, global keyboard events will be used. If it is false, the keyboard events are only used when the network is active. It is activated on mouseOver automatically.}
#'}
#'@param multiselect : Boolean. Default to false. When true, a longheld click (or touch) as well as a control-click will add to the selection.
#'@param navigationButtons : Boolean. Default to false. When true, navigation buttons are drawn on the network canvas. These are HTML buttons and can be completely customized using CSS.
#'@param selectable : Boolean. Default to true When true, the nodes and edges can be selected by the user.
#'@param selectConnectedEdges : Boolean. Default to true. When true, on selecting a node, its connecting edges are highlighted.
#'@param tooltipDelay : Number. Default to 300. When nodes or edges have a defined 'title' field, this can be shown as a pop-up tooltip. The tooltip itself is an HTML element that can be fully styled using CSS. The delay is the amount of time in milliseconds it takes before the tooltip is shown.
#'@param zoomView : Boolean. Default to true. When true, the user can zoom in.
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'
#' @examples
#'
#'nodes <- data.frame(id = 1:10)
#'edges <- data.frame(from = round(runif(8)*10), to = round(runif(8)*10))
#'
#'#frozen network
#'visNetwork(nodes, edges) %>%
#'  visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE)
#'
#'visNetwork(nodes, edges) %>%
#'  visInteraction(hideEdgesOnDrag = TRUE)
#'  
#'visNetwork(nodes, edges) %>%
#'  visInteraction(hover = TRUE)
#'  
#'#navigation button  
#'visNetwork(nodes, edges) %>%
#'  visInteraction(navigationButtons = TRUE)
#'  
#'visNetwork(nodes, edges) %>%
#'  visInteraction(selectConnectedEdges = FALSE)
#'  
#'visNetwork(nodes, edges) %>%
#'  visInteraction(multiselect = TRUE)
#'  
#'visNetwork(nodes, edges) %>%
#'   visInteraction(keyboard = TRUE)
#'  
#'@export

visInteraction <- function(graph,
                       dragNodes = NULL,
                       dragView = NULL,
                       hideEdgesOnDrag = NULL,
                       hideNodesOnDrag = NULL,
                       hover = NULL,
                       hoverConnectedEdges = NULL,
                       keyboard = NULL,
                       multiselect = NULL,
                       navigationButtons = NULL,
                       selectable = NULL,
                       selectConnectedEdges = NULL,
                       tooltipDelay = NULL,
                       zoomView = NULL){

  
  if(!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))){
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  
  interaction <- list()
  interaction$dragNodes <- dragNodes
  interaction$dragView <- dragView
  interaction$hideEdgesOnDrag <- hideEdgesOnDrag
  interaction$hideNodesOnDrag <- hideNodesOnDrag
  interaction$hover <- hover
  interaction$hoverConnectedEdges <- hoverConnectedEdges
  interaction$keyboard <- keyboard
  interaction$multiselect <- multiselect
  interaction$navigationButtons <- navigationButtons
  interaction$selectable <- selectable
  interaction$selectConnectedEdges <- selectConnectedEdges
  interaction$tooltipDelay <- tooltipDelay
  interaction$zoomView <-zoomView

  if(any(class(graph) %in% "visNetwork_Proxy")){
    options <- list(interaction = interaction)
    data <- list(id = graph$id, options = options)
    graph$session$sendCustomMessage("Options",data)
  }else{
    graph$x$options$interaction <- interaction
  }
  graph
}
