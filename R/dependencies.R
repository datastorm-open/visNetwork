#' Use fontAwesome icons in visNetwork \code{graph}
#'
#' Add \href{https://fontawesome.com}{Font-Awesome} for styling
#' our \code{graph} with beautiful, professional icons.  Please note
#' that you'll already have these icons if using Shiny.
#' Can also use  \link[visNetwork]{addIonicons}   
#' 
#' @param  graph : a visNetwork object
#' @param  name  : name of dependency
#' @param  version : fontawesome version.  "4.7.0" (default) or "5.13.0"
#' 
#' @return \code{graph} htmlwidget with Font-Awesome dependencies attached.
#' 
#' @examples
#' 
#' # use fontAwesome icons using groups or nodes options 
#' # font-awesome is not part of dependencies. use addFontAwesome() if needed.
#' # Versions in package (and compatible with vis.js) : 4.7.0 & 5.13.0
#' # https://fontawesome.com/v4.7.0/
#' # https://fontawesome.com/
#' # cheatsheet available in package: 
#' # system.file("fontAwesome/Font_Awesome_Cheatsheet_4_7_0.pdf", package = "visNetwork")
#' 
#' # definition in groups
#' nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#' edges <- data.frame(from = c(1,2), to = c(2,3))
#' 
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
#'   visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
#'   addFontAwesome(version = "4.7.0")
#' 
#' # use 5.13.0
#' # set face = "'Font Awesome 5 Free'"
#' # weight is automatically set to "bold"
#' nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#' edges <- data.frame(from = c(1,2), to = c(2,3))
#' 
#' visNetwork(nodes, edges) %>%
#'   visGroups(groupname = "A", shape = "icon", 
#'       icon = list(face = "'Font Awesome 5 Free'", code = "f0c0", size = 75)) %>%
#'   visGroups(groupname = "B", shape = "icon", 
#'       icon = list(face = "'Font Awesome 5 Free'", code = "f007", color = "red")) %>%
#'   addFontAwesome(version = "5.13.0")
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
addFontAwesome <- function(graph, name = "font-awesome", version = c("4.7.0", "5.13.0")){
  if(!inherits(graph,"htmlwidget")){
    stop("graph should be a htmlwidget.", call.=F)
  } 
  
  version <- match.arg(version)
  
  font_dep <- htmltools::htmlDependency(
    name = name,
    version = version,
    src = c(file=system.file(paste0("htmlwidgets/lib/fontawesome_", version), package="visNetwork")),
    stylesheet = switch(version, 
                        "4.7.0" = "css/font-awesome.min.css",
                        "5.13.0" = "css/all.min.css"
    )
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
#' Add \href{https://ionic.io/ionicons}{Ionicons} for styling
#' our \code{graph} with beautiful, professional icons.
#' Can also use  \link[visNetwork]{addFontAwesome}
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


#' Add libraries dependencies used in export  \link[visNetwork]{visExport}
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
