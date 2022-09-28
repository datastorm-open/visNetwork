#' View full documentation of vis.js on network
#'
#' View full documentation of vis.js on network
#' 
#' 
#' @examples
#'
#' \dontrun{
#' visDocumentation()
#' }
#' 
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'
#' @import htmlwidgets
#'
#' @export
#' @importFrom  utils browseURL
#' @references See online documentation \url{https://datastorm-open.github.io/visNetwork/}
#'
visDocumentation <- function(){
    browseURL(system.file("docjs/network/index.html", package = "visNetwork"))
}

