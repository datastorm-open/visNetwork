#' Network visualization events
#'
#' Network visualization events.
#'
#' @param graph : a visNetwork object
#' @param animationFinished : Fired after an animation is finished
#' @param select : Fired after the user selects or deselects a node by clicking it. Not fired when the method setSelectionis executed.
#' @param click : Fired after the user clicks or taps on a touchscreen.
#' @param doubleClick : Fired after the user double clicks or double taps on a touchscreen.
#' @param hoverNode : Fired when the mouse is moved over a node (assuming the hover option is enabled).
#' @param blurNode : Fired when the mouse is moved off a node (assuming the hover option is enabled).
#' @param resize : Fired when the size of the canvas has been resized, either by a redraw call when the container div has changed in size, a setSize() call with new values or a setOptions() with new width and/or height values.
#' @param dragStart : Fired when a node is being dragged.
#' @param dragEnd : Fired when the dragging of a node(s) has ended.
#' @param startStabilization : Fired once when the network starts the physics calculation. This ends with the stabilized event. 	none
#' @param stabilizationIterationsDone : Fired once when the network finished the initial stabilization run. This is fired REGARDLESS if the network has stabilized. It only means that the amount of configured stabilizationIterations have been completed. 	none
#' @param stabilized : Fired every time the network has been stabilized. This event can be used to trigger the .storePositions() function after stabilization. Fired with an object having the following properties:
#' @param viewChanged : Fired when the view has changed. This is when the network has moved or zoomed. 	none
#' @param zoom : Fired when the network has zoomed. This event can be used to trigger the .storePositions() function after stabilization.
#'
#' @examples
#'
#' nodes <- data.frame(id = 1:3)
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#'
#' visNetwork(nodes, edges) %>%
#'  visEvents(select = "function onSelect (properties) {
#'      alert('selected nodes: ' + properties.nodes);}")
#'
#' @export
visEvents <- function(graph, animationFinished = NULL,
                      select = NULL,
                      click = NULL,
                      doubleClick = NULL,
                      hoverNode = NULL,
                      blurNode = NULL,
                      resize = NULL,
                      dragStart = NULL,
                      dragEnd = NULL,
                      startStabilization = NULL,
                      stabilizationIterationsDone = NULL,
                      stabilized = NULL,
                      viewChanged = NULL,
                      zoom = NULL){

  events <- list()
  events$animationFinished <- JS(animationFinished)
  events$select <- JS(select)
  events$click <- JS(click)
  events$doubleClick <- JS(doubleClick)
  events$hoverNode <- JS(hoverNode)
  events$blurNode <- JS(blurNode)
  events$resize <- JS(resize)
  events$dragStart <- JS(dragStart)
  events$dragEnd <- JS(dragEnd)
  events$startStabilization <- JS(startStabilization)
  events$stabilizationIterationsDone <- JS(stabilizationIterationsDone)
  events$stabilized <- JS(stabilized)
  events$viewChanged <- JS(viewChanged)
  events$zoom <- JS(zoom)

  graph$x$events <- mergeLists(graph$x$events, events)

  graph

}
