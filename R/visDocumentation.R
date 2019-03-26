#' View full documentation of vis.js on network
#'
#' View full documentation of vis.js on network
#' 
#' @param viewer : Set to NULL to open in a browser
#' 
#' @examples
#'
#' # minimal example
#' \dontrun{
#' visDocumentation()
#' visDocumentation(NULL)
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
#' @references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
#'
visDocumentation <- function(viewer = getOption("viewer")){
  if (!is.null(viewer)){
    tempDir <- tempdir()
    ctrl <- file.copy(from = system.file("docjs", package = "visNetwork"), 
              to = tempDir, overwrite = TRUE , recursive = TRUE)
    viewer(paste0(tempDir, "/docjs/network/index.html"))
  }else{
    browseURL(system.file("docjs/network/index.html", package = "visNetwork"))
  }
}

