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
#'@seealso \link[pkg:visNetwork]{visNodes} for nodes options, \link[pkg:visNetwork]{visEdges} for edges options, \link[pkg:visNetwork]{visGroups} for groups options, 
#'\link[pkg:visNetwork]{visLegend} for adding legend, \link[pkg:visNetwork]{visOptions} for custom option, \link[pkg:visNetwork]{visLayout} & \link[pkg:visNetwork]{visHierarchicalLayout} for layout, 
#'\link[pkg:visNetwork]{visPhysics} for control physics, \link[pkg:visNetwork]{visInteraction} for interaction, \link[pkg:visNetwork]{visNetworkProxy} & \link[pkg:visNetwork]{visFocus} & \link[pkg:visNetwork]{visFit} for animation within shiny,
#'\link[pkg:visNetwork]{visDocumentation}, \link[pkg:visNetwork]{visEvents}, \link[pkg:visNetwork]{visConfigure} ...
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

