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
#' @seealso \link{visOptions}, \link{visNodes}, \link{visEdges}, \link{visGroups}, \link{visEvents}
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

