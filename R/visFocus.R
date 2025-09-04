#' Network visualization focus method
#'
#' For use focus() method in a shiny app. For full documentation, have a look at \link[pkg:visNetwork]{visDocumentation}.
#' 
#'@param graph : a \code{\link[pkg:visNetwork]{visNetworkProxy}}  object
#'@param id : a node id
#'@param scale : Optional. Number. The scale is the target zoomlevel. Default value is 2.0. 
#'@param offset : Optional. List. The offset (in DOM units) is how many pixels from the center the view is focussed. Default value is list(x = 0, y = 0). 
#'@param locked : Optional. Boolean. Locked denotes whether or not the view remains locked to the node once the zoom-in animation is finished. Default value is true.
#'@param animation : Optional. List. For animation you can define the duration (in milliseconds) and easing function manually. 
#'Available are: linear, easeInQuad, easeOutQuad, easeInOutQuad, easeInCubic, easeOutCubic, easeInOutCubic, easeInQuart, easeOutQuart, easeInOutQuart, easeInQuint, easeOutQuint, easeInOutQuint.
#'Default to list(duration = 1500, easingFunction = "easeInOutQuad") 
#'
#'@seealso \link[pkg:visNetwork]{visNodes} for nodes options, \link[pkg:visNetwork]{visEdges} for edges options, \link[pkg:visNetwork]{visGroups} for groups options, 
#'\link[pkg:visNetwork]{visLegend} for adding legend, \link[pkg:visNetwork]{visOptions} for custom option, \link[pkg:visNetwork]{visLayout} & \link[pkg:visNetwork]{visHierarchicalLayout} for layout, 
#'\link[pkg:visNetwork]{visPhysics} for control physics, \link[pkg:visNetwork]{visInteraction} for interaction, \link[pkg:visNetwork]{visNetworkProxy} & \link[pkg:visNetwork]{visFocus} & \link[pkg:visNetwork]{visFit} for animation within shiny,
#'\link[pkg:visNetwork]{visDocumentation}, \link[pkg:visNetwork]{visEvents}, \link[pkg:visNetwork]{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{https://datastorm-open.github.io/visNetwork/}
visFocus <- function(graph, id, scale =  2,
                     offset =  list(x = 0, y = 0),
                     locked = TRUE,
                     animation = list(duration = 1500, easingFunction = "easeInOutQuad")){

  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visFocus with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  stopifnot(length(id) == 1)
  
  options <- list(scale = scale, offset = offset, locked = locked, animation = animation)
  data <- list(id = graph$id, focusId = id, options = options)
  
  graph$session$sendCustomMessage("visShinyFocus", data)

  graph
}
