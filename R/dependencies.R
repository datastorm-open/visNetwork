#' Use fontAwesome icons in visNetwork \code{graph}
#'
#' Add \href{http://fortawesome.github.io/Font-Awesome/}{Font-Awesome} for styling
#'   our \code{graph} with beautiful, professional icons.  Please note
#'   that you'll already have these icons if using Shiny.
#'   
#' @param  graph : a visNetwork object
#' 
#' @return \code{graph} htmlwidget with Font-Awesome dependencies attached.
#' 
#' @examples
#' 
#' # use fontAwesome icons using groups or nodes options 
#' # font-awesome is not part of dependencies. use addFontAwesome() if needed.
#' # http://fortawesome.github.io/Font-Awesome/
#' 
#' nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#' edges <- data.frame(from = c(1,2), to = c(2,3))
#' 
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
#'   visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
#'   addFontAwesome()
#' 
#' nodes <- data.frame(id = 1:3)
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#' 
#' visNetwork(nodes, edges) %>%
#'   visNodes(shape = "icon", icon = list( face ='FontAwesome', code = "f0c0")) %>%
#'   addFontAwesome()
#' 
#' @import htmltools
#'
#' @export

addFontAwesome <- function(graph){
  if(!inherits(graph,"htmlwidget")){
    stop("graph should be a htmlwidget.", call.=F)
  } 
  
  font_dep <- htmltools::htmlDependency(
    name = "font-awesome",
    version = "4.3.0",
    src = c(file=system.file("htmlwidgets/lib/font-awesome", package="visNetwork")),
    stylesheet = "font-awesome.min.css"
  )
  
  if(length(graph$dependencies) == 0){
    graph$dependencies = list()
  }
  
  graph$dependencies[[length(graph$dependencies)+1]] <- font_dep
  
  graph
}

addExport <- function(graph){
  if(!inherits(graph,"htmlwidget")){
    stop("graph should be a htmlwidget.", call.=F)
  } 
  
  fileSaver_dep <- htmltools::htmlDependency(
    name = "FileSaver",
    version = "1.1.20151003",
    src = c(file=system.file("htmlwidgets/lib/export/FileSaver", package="visNetwork")),
    script = "FileSaver.min.js"
  )
  
  Blob_dep <- htmltools::htmlDependency(
    name = "Blob",
    version = "1.0",
    src = c(file=system.file("htmlwidgets/lib/export/Blob", package="visNetwork")),
    script = "Blob.js"
  )
  
  canvastoBlob_dep <- htmltools::htmlDependency(
    name = "canvas-toBlob",
    version = "1.0",
    src = c(file=system.file("htmlwidgets/lib/export/canvas-toBlob", package="visNetwork")),
    script = "canvas-toBlob.js"
  )
  
  html2canvas_dep <- htmltools::htmlDependency(
    name = "html2canvas",
    version = "0.5.0",
    src = c(file=system.file("htmlwidgets/lib/export/html2canvas", package="visNetwork")),
    script = "html2canvas.js"
  )

  if(length(graph$dependencies) == 0){
    graph$dependencies = list()
  }
  
  graph$dependencies[[length(graph$dependencies)+1]] <- fileSaver_dep
  graph$dependencies[[length(graph$dependencies)+1]] <- Blob_dep
  graph$dependencies[[length(graph$dependencies)+1]] <- canvastoBlob_dep
  graph$dependencies[[length(graph$dependencies)+1]] <- html2canvas_dep
  
  graph
}