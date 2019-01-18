#' Use fontAwesome icons in visNetwork \code{graph}
#'
#' Add \href{http://fortawesome.github.io/Font-Awesome/}{Font-Awesome} for styling
#' our \code{graph} with beautiful, professional icons.  Please note
#' that you'll already have these icons if using Shiny.
#' Can also use \link{addIonicons}   
#' 
#' @param  graph : a visNetwork object
#' @param  name  : name of dependency
#' 
#' @return \code{graph} htmlwidget with Font-Awesome dependencies attached.
#' 
#' @examples
#' 
#' # use fontAwesome icons using groups or nodes options 
#' # font-awesome is not part of dependencies. use addFontAwesome() if needed.
#' # https://fontawesome.com/v4.7.0/
#' # Version in package (and compatible with vis.js) : 4.7.0
#' # cheatsheet available in package: 
#' # system.file("fontAwesome/Font_Awesome_Cheatsheet.pdf", package = "visNetwork")
#' 
#' # definition in groups
#' nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#' edges <- data.frame(from = c(1,2), to = c(2,3))
#' 
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
#'   visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
#'   addFontAwesome()
#' 
#' # definition in nodes
#' nodes <- data.frame(id = 1:3, shape = "icon", icon.face = 'FontAwesome', 
#'    icon.code = "f0c0")
#' edges <- data.frame(from = c(1,2), to = c(1,3))
#' 
#' visNetwork(nodes, edges) %>%
#'   addFontAwesome()
#' 
#' # using shinydashboard : change name if needed
#' visNetwork(nodes, edges) %>%
#'   addFontAwesome(name = "font-awesome-visNetwork")
#'   
#' @import htmltools
#'
#' @export
addFontAwesome <- function(graph, name = "font-awesome"){
  if(!inherits(graph,"htmlwidget")){
    stop("graph should be a htmlwidget.", call.=F)
  } 
  
  font_dep <- htmltools::htmlDependency(
    name = name,
    version = "4.7.0",
    src = c(file=system.file("htmlwidgets/lib/font-awesome", package="visNetwork")),
    stylesheet = "css/font-awesome.min.css"
  )
  
  if(length(graph$dependencies) == 0){
    graph$dependencies = list()
  }
  
  graph$dependencies[[length(graph$dependencies)+1]] <- font_dep
  graph$x$iconsRedraw <- TRUE
  graph
}

#' Use Ionicons in visNetwork \code{graph}
#'
#' Add \href{http://ionicons.com/}{Ionicons} for styling
#' our \code{graph} with beautiful, professional icons.
#' See \href{http://ionicons.com/cheatsheet.html}{Cheatsheet} to get CSS content code.
#' Can also use \link{addFontAwesome}
#'   
#' @param  graph : a visNetwork object
#' @param  name  : name of dependency
#' 
#' @return \code{graph} htmlwidget with Ionicons dependencies attached.
#' 
#' @examples
#' 
#' nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#' edges <- data.frame(from = c(1,2), to = c(2,3))
#' 
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", shape = "icon", 
#'    icon = list(face ='Ionicons', code = "f101", size = 75)) %>%
#'   visGroups(groupname = "B", shape = "icon", 
#'    icon = list(face ='Ionicons', code = "f100", color = "red")) %>%
#'   addIonicons()
#' 
#' @import htmltools
#'
#' @export
addIonicons <- function(graph, name = "ionicons"){
  if(!inherits(graph,"htmlwidget")){
    stop("graph should be a htmlwidget.", call.=F)
  } 
  
  font_dep <- htmltools::htmlDependency(
    name = name,
    version = "2.0.1",
    src = c(file=system.file("htmlwidgets/lib/ionicons", package="visNetwork")),
    stylesheet = "css/ionicons.min.css"
  )
  
  if(length(graph$dependencies) == 0){
    graph$dependencies = list()
  }
  
  graph$dependencies[[length(graph$dependencies)+1]] <- font_dep
  graph$x$iconsRedraw <- TRUE
  graph
}


#' Add libraries dependencies used in export \link{visExport}
#'
#'   
#' @param  graph : a visNetwork object
#' @param  pdf : boolean. Add jsPDF or not ?
#' 
#' @return \code{graph} htmlwidget with dependencies attached.
#' 
#' @import htmltools
#'
#' @export
#
addExport <- function(graph, pdf = TRUE){
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
  
  if(pdf){
    jspdf_dep <- htmltools::htmlDependency(
      name = "jspdf",
      version = "1.3.2",
      src = c(file=system.file("htmlwidgets/lib/export/jsPDF", package="visNetwork")),
      script = "jspdf.debug.js"
    )
    
    graph$dependencies[[length(graph$dependencies)+1]] <- jspdf_dep
  }
  
  graph
}