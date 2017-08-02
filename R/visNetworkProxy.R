#' Shiny bindings for visNetwork
#' 
#' Output and render functions for using visNetwork within Shiny 
#' applications and interactive Rmd documents. With \code{visNetworkProxy}, 
#' you can update your network without redraw in shiny.
#' 
#' @param outputId : output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#' @param expr An expression that generates a visNetwork
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This 
#'   is useful if you want to save an expression in a variable.
#' @param shinyId single-element character vector indicating the shiny output ID of the
#'   network to modify
#' @param session the Shiny session object to which the map belongs; usually the
#'   default value will suffice  
#'   
#' @name visNetwork-shiny
#' 
#' @details 
#' 
#' With \code{visNetworkProxy}, you can update your network and use various methods :
#' \itemize{
#'  \item{"all 'visNetwork' functons"}{ : \code{\link{visOptions}}, \code{\link{visNodes}}, \code{\link{visEdges}}, \code{\link{visPhysics}}, \code{\link{visEvents}}, ...}
#'  \item{\code{\link{visFocus}}}{ : Focus to one or more nodes}
#'  \item{\code{\link{visFit}}}{ : Set view on a set of nodes}
#'  \item{\code{\link{visUpdateNodes}}}{ : Update and add nodes}
#'  \item{\code{\link{visUpdateEdges}}}{ : Update and add edges}
#'  \item{\code{\link{visRemoveNodes}}}{ : Remove nodes}
#'  \item{\code{\link{visRemoveEdges}}}{ : Remove edges}
#'  \item{\code{\link{visSelectNodes}}}{ :Select nodes}
#'  \item{\code{\link{visSelectEdges}}}{ : Select edges}
#'  \item{\code{\link{visGetNodes}}}{ : Get nodes dataset}
#'  \item{\code{\link{visGetEdges}}}{ : Get edges dataset}
#'  \item{\code{\link{visSetSelection}}}{ : Select edges/nodes}
#'  \item{\code{\link{visNearestNodes}}}{ : Get nearest nodes}
#'  \item{\code{\link{visCollapse}}}{ : Collapse nodes}
#'  \item{\code{\link{visUncollapse}}}{ : Uncollpase nodes}
#'  \item{\code{\link{visSetTitle}}}{ : Set and update main, submain, footer}
#'  \item{and also...}{ : \code{\link{visGetSelectedEdges}}, \code{\link{visGetSelectedNodes}}, \code{\link{visGetSelection}},
#'     \code{\link{visGetConnectedEdges}}, \code{\link{visGetConnectedNodes}}, \code{\link{visRedraw}}, \code{\link{visStabilize}}, 
#'     \code{\link{visSetData}}, \code{\link{visGetPositions}}, \code{\link{visMoveNode}}, \code{\link{visUnselectAll}},
#'     \code{\link{visGetScale}}, \code{\link{visGetBoundingBox}}, \code{\link{visGetViewPosition}},\code{\link{visSetOptions}}}
#'}
#' 
#' @examples 
#' 
#'\dontrun{
#'
#' # have a look to : 
#' shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#' @export
#' 
#' @references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
#' 
visNetworkOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'visNetwork', width, height, package = 'visNetwork')
}

#' @rdname visNetwork-shiny
#' @export
renderVisNetwork <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, visNetworkOutput, env, quoted = TRUE)
}


#' @name visNetwork-shiny
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
