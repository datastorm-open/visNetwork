#' Set title, subtitle, and footer using \code{visNetworkProxy}
#' 
#' @param graph : a \code{\link{visNetworkProxy}}  object
#' @param main : For add a title. Character or a named list.
#' \itemize{
#'  \item{"text"}{ : Character. Title.}
#'  \item{"style"}{ : Optional. Character. HTML style of title.}
#'  \item{'hidden'}{ : Optional. Boolean. Force title to be hidden} 
#' }
#'
#' @param submain : For add a subtitle. Character or a named list.
#' \itemize{
#'  \item{"text"}{ : Character. Subtitle.}
#'  \item{"style"}{ : Optional. Character. HTML style of submain.}
#'  \item{'hidden'}{ : Optional. Boolean. Force submain to be hidden} 
#' }
#'
#' @param footer : For add a footer. Character or a named list.
#' \itemize{
#'  \item{"text"}{ : Character. footer.}
#'  \item{"style"}{ : Optional. Character. HTML style of footer.}
#'  \item{'hidden'}{ : Optional. Boolean. Force footer to be be hidden} 
#' }
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
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visSetTitle <- function(graph, main = NULL, submain = NULL, footer = NULL){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visSetTitle with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  
  # main
  if(!is.null(main)){
    if(is.list(main)){
      if(any(!names(main)%in%c("text", "style", "hidden"))){
        stop("Invalid 'main' argument")
      }
      if(!"hidden"%in%names(main)){
        main$hidden <- FALSE
      }
    }else if(!inherits(main, "character")){
      stop("Invalid 'main' argument. Not a character")
    }else {
      main <- list(text = main, hidden = FALSE)
    }
  }
  
  # submain
  if(!is.null(submain)){
    if(is.list(submain)){
      if(any(!names(submain)%in%c("text", "style", "hidden"))){
        stop("Invalid 'submain' argument")
      }
      if(!"hidden"%in%names(submain)){
        submain$hidden <- FALSE
      }
    }else if(!inherits(submain, "character")){
      stop("Invalid 'submain' argument. Not a character")
    }else {
      submain <- list(text = submain, hidden = FALSE)
    }
  }
  
  # footer
  if(!is.null(footer)){
    if(is.list(footer)){
      if(any(!names(footer)%in%c("text", "style", "hidden"))){
        stop("Invalid 'footer' argument")
      }
      if(!"hidden"%in%names(footer)){
        footer$hidden <- FALSE
      }
    }else if(!inherits(footer, "character")){
      stop("Invalid 'footer' argument. Not a character")
    }else {
      footer <- list(text = footer, hidden = FALSE)
    }
  }
  
  data <- list(id = graph$id, main = main, submain = submain, footer = footer)

  graph$session$sendCustomMessage("visShinySetTitle", data)
  
  graph
}
