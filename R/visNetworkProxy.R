#' Send methods to a visNetwork instance in a Shiny app
#'
#' Creates a visNetwork object that can be used to customize and control after rendering. 
#' For use in Shiny apps and Shiny docs only.
#'
#' With \code{visNetworkProxy}, you can use various methods like
#' \code{\link{visOptions}}, \code{\link{visNodes}}, \code{\link{visEdges}},
#' \code{\link{visPhysics}}, ... and more with special call for \code{visNetworkProxy} object like
#' \code{\link{visFocus}}, \code{\link{visFit}}, \code{\link{visRedraw}}.
#' 
#' @param shinyId single-element character vector indicating the shiny output ID of the
#'   network to modify
#' @param session the Shiny session object to which the map belongs; usually the
#'   default value will suffice
#'   
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'   
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#' @export   
visNetworkProxy <- function(shinyId,  session = shiny::getDefaultReactiveDomain()){
  if (is.null(session)) {
    stop("visNetworkProxy must be called from the server function of a Shiny app")
  }
  
  object <- list(id = shinyId, session = session)
  class(object) <- "visNetwork_Proxy"
  object
}
