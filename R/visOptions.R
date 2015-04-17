#' Network visualization general options
#'
#' Network visualization general options. See \url{http://visjs.org/docs/network.html#Configuration_options}.
#'
#'@param highlightNearest : Custom Option. Boolean. Default to false. Highlight nearest when clicking a node ? Based on \url{http://visjs.org/examples/network/29_neighbourhood_highlight.html}
#' This options use click event. Not available for DOT and Gephi.
#'@param nodesIdSelection :  Custom Option. Boolean. Default to false. A little bit experimental. Add an id node selection.. Not available for DOT and Gephi.
#'@param clickToUse : Boolean. Default to false. When a Network is configured to be clickToUse, it will react to mouse, touch, and keyboard events only when active. When active, a blue shadow border is displayed around the Network. The Network is set active by clicking on it, and is changed to inactive again by clicking outside the Network or by pressing the ESC key.
#'@param useDefaultGroups : Boolean. Default to true. If true, the default groups are used when groups are used. If you have defined your own groups those will be used. If you have an item with a group that is NOT in your own group list, setting useDefaultGroups true will iterate over the default groups for unknown groups. If it is set to false, it will iterate over your own groups for unknown groups.
#'@param configurePhysics : Boolean. Default to false. Enabling this setting will create a physics configuration div above the network. You can use this to fine tune the physics system to suit your needs. Because of the many possible configurations, there is not a one-size-fits-all setting. By using this tool, you can adapt the physics to your dataset.
#'@param dataManipulation : Just a Boolean, or a named list. Settings for manipulating the Dataset. See \url{http://visjs.org/docs/network.html#Data_manipulation}
#'\itemize{
#'  \item{"enabled"}{ : Boolean. Default to false. Enabling or disabling of the data manipulation toolbar. If it is initially hidden, an edit button appears in the top left corner.}
#'  \item{"initiallyVisible"}{ : Boolean. Default to false. Initially hide or show the data manipulation toolbar.}
#'}
#'@param clustering : Boolean. Default to false. Clustering is turned off by default. Set to true to enabled clustering or see \link{visClustering} to set options..
#'@param freezeForStabilization : Boolean. Default to false. With the advent of the storePositions() function, the positions of the nodes can be saved after they are stabilized. The smoothCurves require support nodes and those positions are not stored. In order to speed up the initialization of the network by using storePositions() and loading the nodes with the stored positions, the freezeForStabilization option freezes all nodes that have been supplied with an x and y position in place during the stabilization. That way only the support nodes for the smooth curves have to stabilize, greatly speeding up the stabilization process with cached positions.
#'@param height : String. Default to "400px". The height of the network in pixels or as a percentage.
#'@param hover : Boolean. Default to false. Enabling the change of the colors of nodes and edges when the mouse hovers over them. Enabling this may have a minor impact on performance.
#'@param keyboard : Just a Boolean, or a named list. Configuration options for shortcuts keys. Shortcut keys are turned off by default. See \url{http://visjs.org/docs/network.html#Keyboard_navigation}
#'\itemize{
#'  \item{"speed}{ : a named list} \itemize{
#'    \item{"x"}{ : Number. Default to 10. This defines the speed of the camera movement in the x direction when using the keyboard navigation.}
#'    \item{"y"}{ : Number. Default to 10. This defines the speed of the camera movement in the y direction when using the keyboard navigation.}
#'    \item{"zoom"}{ : Number. Default to 0.02. This defines the zoomspeed when using the keyboard navigation.Number   0.02 	This defines the zoomspeed when using the keyboard navigation.}
#'    }
#'  \item{"bindToWindow"}{ : Boolean. Default to true. If this is true, global keyboard events will be used. If it is false, the keyboard events are only used when the network is active. It is activated on mouseOver automatically.}
#'}
#'@param dragNetwork : Boolean. Default to true. Toggle if the network can be dragged. This will not affect the dragging of nodes.
#'@param dragNodes : Boolean. Default to true. Toggle if the nodes can be dragged. This will not affect the dragging of the network.
#'@param hideNodesOnDrag : Boolean. Default to false. Toggle if the nodes are drawn during a drag. This can greatly improve performance if you have many nodes.
#'@param hideEdgesOnDrag : Boolean. Default to false. Toggle if the edges are drawn during a drag. This can greatly improve performance if you have many edges.
#'@param navigation : Boolean. Default to false. Configuration options for the navigation controls.
#'@param smoothCurves : Just a Boolean, or a named list. If true, edges are drawn as smooth curves. This is more computationally intensive since the edge now is a quadratic Bezier curve. This can be further configured by the options below.
#'\itemize{
#'  \item{"dynamic"}{ : Boolean. Default to true. By default, the edges are dynamic. This means there are support nodes placed in the middle of the edge. This support node is also handed by the physics simulation. If false, the smoothness will be based on the relative positions of the to and from nodes. This is computationally cheaper but there is no self organisation.}
#'  \item{"type"}{ : String. Default to "continuous". This option only affects NONdynamic smooth curves. The supported types are: continuous, discrete, diagonalCross, straightCross, horizontal, vertical, curvedCW, curvedCCW. The effects of these types are shown in examples 26 and 27}
#'  \item{"roundness"}{ : Number. Default to 0.5. This only affects NONdynamic smooth curves. The roundness can be tweaked with the parameter. The value range is from 0 to 1 with a maximum roundness at 0.5.}
#'}
#'@param selectable : Boolean. Default to true. If true, nodes in the network can be selected by clicking them. Long press can be used to select multiple nodes.
#'@param stabilize : Boolean. Default to true. If true, the network is stabilized before displaying it. If false, the nodes move to a stabe position visibly in an animated way.
#'@param stabilizationIterations : Number. Default to 1000. If stabilize is set to true, this number is the (maximum) amount of physics steps the stabilization process takes before showing the result. If your simulation takes too long to stabilize, this number can be reduced. On the other hand, if your network is not stabilized after loading, this number can be increased.
#'@param zoomExtentOnStabilize : Boolean. Default to true. When the internal stabilize function is called because the stabilize option is set to true OR the hierarchical system (re)initializes, a call to zoomExtent is done by default. By setting this to false, you can avoid this call.
#'@param width : String. Default to "400px". The width of the network in pixels or as a percentage.
#'@param zoomable : Boolean. Default to true. Toggle if the network can be zoomed.
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options
#'@export

visOptions <- function(graph,
                       highlightNearest = FALSE,
                       nodesIdSelection = FALSE,
                       clickToUse = NULL,
                       useDefaultGroups = NULL,
                       configurePhysics = NULL,
                       dataManipulation = NULL,
                       clustering = NULL,
                       freezeForStabilization = NULL,
                       height = NULL,
                       hover = NULL,
                       keyboard = NULL,
                       dragNetwork = NULL,
                       dragNodes = NULL,
                       hideNodesOnDrag = NULL,
                       hideEdgesOnDrag = NULL,
                       navigation = NULL,
                       smoothCurves = NULL,
                       selectable = NULL,
                       stabilize = NULL,
                       stabilizationIterations = NULL,
                       zoomExtentOnStabilize = NULL,
                       width = NULL,
                       zoomable = NULL){

  options <- list()

  options$clickToUse <- clickToUse
  options$useDefaultGroups <- useDefaultGroups
  options$configurePhysics <- configurePhysics
  options$dataManipulation <- dataManipulation
  options$clustering <- clustering
  options$freezeForStabilization <- freezeForStabilization
  options$height <- height
  options$hover <- hover
  options$keyboard <- keyboard
  options$dragNetwork <- dragNetwork
  options$dragNodes <- dragNodes
  options$hideNodesOnDrag <- hideNodesOnDrag
  options$hideEdgesOnDrag <- hideEdgesOnDrag
  options$navigation <- navigation
  options$smoothCurves <- smoothCurves
  options$selectable <- selectable
  options$stabilize <- stabilize
  options$stabilizationIterations <- stabilizationIterations
  options$zoomExtentOnStabilize <- zoomExtentOnStabilize
  options$width <- width
  options$zoomable <- zoomable

  if(!is.null(dataManipulation)){
    if(dataManipulation){
      graph$x$datacss <- paste(readLines(system.file("htmlwidgets/lib/css/dataManipulation.css", package = "visNetwork"), warn = FALSE), collapse = "\n")
    }
  }

  x <- list(highlight = highlightNearest, idselection = nodesIdSelection)
  graph$x <- mergeLists(graph$x, x)
  graph$x$options <- mergeLists(graph$x$options, options)

  graph
}
