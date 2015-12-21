#' View full documentation of vis.js on network
#'
#' View full documentation of vis.js on network
#' 
#' @examples
#'
#' # minimal example
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
visDocumentation <- function(){
  viewer <- getOption("viewer")
  if (!is.null(viewer)){
    tempDir <- tempdir()
    ctrl <- file.copy(from = system.file("doc", package = "visNetwork"), 
              to = tempDir, overwrite = TRUE , recursive = TRUE)
    viewer(paste0(tempDir, "/doc/network/index.html"))
  }else{
    browseURL(system.file("doc/network/index.html", package = "visNetwork"))
  }
}

