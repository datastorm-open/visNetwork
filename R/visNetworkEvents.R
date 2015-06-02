#' Network visualization events
#'
#' Network visualization events.
#'
#' @param graph : a visNetwork object
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
#' @param hoverNode : Fired interaction:{hover:true} and the mouse hovers over a node.
#' @param blurNode : Fired interaction:{hover:true} and the mouse moved away from a node it was hovering over before.
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
#' 
#' @examples
#'
#' nodes <- data.frame(id = 1:3)
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#'
#' visNetwork(nodes, edges) %>%
#'  visEvents(select = "function onSelect (properties) {
#'      alert('selected nodes: ' + properties.nodes);}", 
#'      dragEnd = "function onSelect (properties) {
#'      alert('finsih to drag');}")
#'
#'
#'@seealso \url{../doc/network/index.html}
#'
#' @export
visEvents <- function(graph,
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
                      hoverNode = NULL,
                      blurNode = NULL,
                      zoom = NULL,
                      showPopup = NULL,
                      hidePopup = NULL,
                      startStabilizing = NULL,
                      stabilizationProgress = NULL,
                      stabilizationIterationsDone = NULL,
                      stabilized = NULL,
                      resize = NULL,
                      initRedraw = NULL,
                      afterDrawing = NULL,
                      animationFinished = NULL){

  events <- list()
  events$click  <- JS(click)
  events$doubleClick  <- JS(doubleClick)
  events$oncontext  <- JS(oncontext)
  events$hold  <- JS(hold)
  events$release  <- JS(release)
  events$select  <- JS(select)
  events$selectNode  <- JS(selectNode)
  events$selectEdge  <- JS(selectEdge)
  events$deselectNode  <- JS(deselectNode)
  events$deselectEdge  <- JS(deselectEdge)
  events$dragStart  <- JS(dragStart)
  events$dragging  <- JS(dragging)
  events$dragEnd  <- JS(dragEnd)
  events$hoverNode  <- JS(hoverNode)
  events$blurNode  <- JS(blurNode)
  events$zoom  <- JS(zoom)
  events$showPopup  <- JS(showPopup)
  events$hidePopup  <- JS(hidePopup)
  events$startStabilizing  <- JS(startStabilizing)
  events$stabilizationProgress  <- JS(stabilizationProgress)
  events$stabilizationIterationsDone  <- JS(stabilizationIterationsDone)
  events$stabilized  <- JS(stabilized)
  events$resize  <- JS(resize)
  events$initRedraw  <- JS(initRedraw)
  events$afterDrawing  <- JS(afterDrawing)
  events$animationFinished <- JS(animationFinished)

  graph$x$events <- mergeLists(graph$x$events, events)

  graph

}
