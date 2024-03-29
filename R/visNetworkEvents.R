#' Network visualization events
#'
#' Network visualization events. For full documentation, have a look at \link{visDocumentation}.
#' Use \code{type = "once"} to set an event listener only once, and \code{type = "off"} to disable all the related events.
#'
#' @param graph : a visNetwork object
#' @param type : Character. "on" (Default) to full listener, "once" to set an event listener only once, or "off" to disable a listener.
#' @param click : Fired when the user clicks the mouse or taps on a touchscreen device.
#' @param doubleClick : Fired when the user double clicks the mouse or double taps on a touchscreen device. Since a double click is in fact 2 clicks, 2 click events are fired, followed by a double click event. If you do not want to use the click events if a double click event is fired, just check the time between click events before processing them.
#' @param oncontext : Fired when the user click on the canvas with the right mouse button. The right mouse button does not select by default. You can use the method getNodeAt to select the node if you want.
#' @param hold : Fired when the user clicks and holds the mouse or taps and holds on a touchscreen device. A click event is also fired in this case.
#' @param release : Fired after drawing on the canvas has been completed. Can be used to draw on top of the network.
#' @param select : Fired when the selection has changed by user action. This means a node or edge has been selected, added to the selection or deselected. All select events are only triggered on click and hold.
#' @param selectNode : Fired when a node has been selected by the user.
#' @param selectEdge : Fired when a edge has been selected by the user.
#' @param deselectNode : Fired when a node (or nodes) has (or have) been deselected by the user. The previous selection is the list of nodes and edges that were selected before the last user event.
#' @param deselectEdge : Fired when a edge (or edges) has (or have) been deselected by the user. The previous selection is the list of nodes and edges that were selected before the last user event.
#' @param dragStart : Fired when starting a drag.
#' @param dragging : Fired when dragging node(s) or the view.
#' @param dragEnd : Fired when the drag has finished.
#' @param controlNodeDragging : Fired when dragging control node. Control Edge is edge that is being dragged and contains ids of 'from' and 'to' nodes. If control node is not dragged over another node, 'to' field is undefined. See \link{visDocumentation}.
#' @param controlNodeDragEnd : Fired when the control node drag has finished. See \link{visDocumentation}.
#' @param hoverNode : Fired interaction:{hover:true} and the mouse hovers over a node.
#' @param blurNode : Fired interaction:{hover:true} and the mouse moved away from a node it was hovering over before.
#' @param hoverEdge : Fired interaction:{hover:true} and the mouse hovers over a edge
#' @param blurEdge : Fired interaction:{hover:true} and the mouse moved away from a edge it was hovering over before.
#' @param zoom : Fired when the user zooms in or out. The properties tell you which direction the zoom is in. The scale is a number greater than 0, which is the same that you get with network.getScale().
#' @param showPopup : Fired when the popup (tooltip) is shown.
#' @param hidePopup : Fired when the popup (tooltip) is hidden.
#' @param startStabilizing : Fired when stabilization starts. This is also the case when you drag a node and the physics simulation restarts to stabilize again. Stabilization does not neccesarily imply 'without showing'.
#' @param stabilizationProgress : Fired when a multiple of the updateInterval number of iterations is reached. This only occurs in the 'hidden' stabilization. Passes an object with properties structured as:
#' @param stabilizationIterationsDone : Fired when the 'hidden' stabilization finishes. This does not necessarily mean the network is stabilized; it could also mean that the amount of iterations defined in the options has been reached.
#' @param stabilized : Fired when the network has stabilized or when the stopSimulation() has been called. The amount of iterations it took could be used to tweak the maximum amount of iterations needed to stabilize the network.
#' @param resize : Fired when the size of the canvas has been resized, either by a redraw call when the container div has changed in size, a setSize() call with new values or a setOptions() with new width and/or height values.
#' @param initRedraw : Fired before the redrawing begins. The simulation step has completed at this point. Can be used to move custom elements before starting drawing the new frame.
#' @param beforeDrawing : Fired after the canvas has been cleared, scaled and translated to the viewing position but before all edges and nodes are drawn. Can be used to draw behind the network.
#' @param afterDrawing : Fired after drawing on the canvas has been completed. Can be used to draw on top of the network.
#' @param animationFinished : Fired when an animation is finished.
#' @param configChange : Fired when a user changes any option in the configurator. The options object can be used with the setOptions method or stringified using JSON.stringify(). You do not have to manually put the options into the network: this is done automatically. You can use the event to store user options in the database.
#' 
#' @examples
#'
#' nodes <- data.frame(id = 1:3)
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#'
#' visNetwork(nodes, edges) %>%
#'  visEvents(select = "function(properties) {
#'      alert('selected nodes: ' + properties.nodes);}", 
#'      dragEnd = "function(properties) {
#'      alert('finish to drag');}")
#'
#' # set one 
#' visNetwork(nodes, edges) %>%
#'  visEvents(type = "once", select = "function() {
#'      alert('first selection');}") %>%
#'  visEvents(select = "function(properties) {
#'      alert('selected nodes: ' + properties.nodes);}", 
#'      dragEnd = "function(properties) {
#'      alert('finish to drag');}")
#'       
#' # use this to get the network
#' visNetwork(nodes, edges) %>%
#'   visEvents(type = "once", startStabilizing = "function() {
#'             this.moveTo({scale:0.1})}") %>%
#'   visPhysics(stabilization = FALSE)
#' 
#' # shift+click, .....
#' visNetwork(nodes, edges) %>%
#'     visEvents(click = "function(e) {
#'             if(e.event.srcEvent.shiftKey){
#'               alert('shift+click event')
#'             } else if(e.event.srcEvent.ctrlKey){
#'               alert('ctrl+click event')
#'             }else if(e.event.srcEvent.altKey){
#'               alert('alt+click event')
#'             } else {
#'               alert('click event')
#'             }
#'           }")
#'           
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'
#' @export
#' @references See online documentation \url{https://datastorm-open.github.io/visNetwork/}
visEvents <- function(graph,
                      type = "on",
                      click = NULL,
                      doubleClick = NULL,
                      oncontext = NULL,
                      hold = NULL,
                      release = NULL,
                      select = NULL,
                      selectNode = NULL,
                      selectEdge = NULL,
                      deselectNode = NULL,
                      deselectEdge = NULL,
                      dragStart = NULL,
                      dragging = NULL,
                      dragEnd = NULL,
                      controlNodeDragging = NULL,
                      controlNodeDragEnd = NULL,
                      hoverNode = NULL,
                      blurNode = NULL,
                      hoverEdge = NULL,
                      blurEdge = NULL,
                      zoom = NULL,
                      showPopup = NULL,
                      hidePopup = NULL,
                      startStabilizing = NULL,
                      stabilizationProgress = NULL,
                      stabilizationIterationsDone = NULL,
                      stabilized = NULL,
                      resize = NULL,
                      initRedraw = NULL,
                      beforeDrawing = NULL,
                      afterDrawing = NULL,
                      animationFinished = NULL, 
                      configChange = NULL){

  if(!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))){
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  
  stopifnot(type %in% c("on", "once", "off"))
  
  events <- list()
  events$click  <- click
  events$doubleClick  <- doubleClick
  events$oncontext  <- oncontext
  events$hold  <- hold
  events$release  <- release
  events$select  <- select
  events$selectNode  <- selectNode
  events$selectEdge  <- selectEdge
  events$deselectNode  <- deselectNode
  events$deselectEdge  <- deselectEdge
  events$dragStart  <- dragStart
  events$dragging  <- dragging
  events$dragEnd  <- dragEnd
  events$controlNodeDragging <- controlNodeDragging
  events$controlNodeDragEnd <- controlNodeDragEnd
  events$hoverNode  <- hoverNode
  events$blurNode  <- blurNode
  events$hoverEdge  <- hoverEdge
  events$blurEdge  <- blurEdge
  events$zoom  <- zoom
  events$showPopup  <- showPopup
  events$hidePopup  <- hidePopup
  events$startStabilizing  <- startStabilizing
  events$stabilizationProgress  <- stabilizationProgress
  events$stabilizationIterationsDone  <- stabilizationIterationsDone
  events$stabilized  <- stabilized
  events$resize  <- resize
  events$initRedraw  <- initRedraw
  events$beforeDrawing  <- beforeDrawing
  events$afterDrawing  <- afterDrawing
  events$animationFinished <- animationFinished
  events$configChange <- configChange
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    data <- list(id = graph$id, type = type, events = events)
    graph$session$sendCustomMessage("visShinyEvents",data)
  }else{
    events <- lapply(events, function(x) htmlwidgets::JS(x))
    if(type %in% "on"){
      graph$x$events <- mergeLists(graph$x$events, events)
    } else if (type %in% "once"){
      graph$x$OnceEvents <- mergeLists(graph$x$OnceEvents, events)
    } else if (type %in% "off"){
      graph$x$ResetEvents <- mergeLists(graph$x$ResetEvents, events)
    }
  }
  
  graph

}