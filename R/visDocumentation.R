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
#'@seealso  \link[visNetwork]{visNodes} for nodes options,  \link[visNetwork]{visEdges} for edges options,  \link[visNetwork]{visGroups} for groups options, 
#' \link[visNetwork]{visLegend} for adding legend,  \link[visNetwork]{visOptions} for custom option,  \link[visNetwork]{visLayout} &  \link[visNetwork]{visHierarchicalLayout} for layout, 
#' \link[visNetwork]{visPhysics} for control physics,  \link[visNetwork]{visInteraction} for interaction,  \link[visNetwork]{visNetworkProxy} &  \link[visNetwork]{visFocus} &  \link[visNetwork]{visFit} for animation within shiny,
#' \link[visNetwork]{visDocumentation},  \link[visNetwork]{visEvents},  \link[visNetwork]{visConfigure} ...
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

