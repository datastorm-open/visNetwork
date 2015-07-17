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
  browseURL(system.file("doc/network/index.html", package = "visNetwork"))
}

