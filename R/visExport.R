#' Network export configuration
#'
#' Network export configuration. This function only work within shiny or a web browser (not in RStudio)
#' 
#'@param graph : a visNetwork object
#'@param type : Type of export. One of "png" (default) or "jpeg"
#'@param name : name of imgage, default to "network"
#'@param label : Label on button, default to "Export as png/jpeg"
#'@param background : background color, default to white (#fff)
#'@param float : button postion, default to "right" 
#'@param style : button css style.
#'@param loadDependencies / Boolean. TRUE by default. Load libraries for export (fileSaver, Blob, canvastoBlob,html2canvas)
#'
#'
#'@examples
#'
#'nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#'edges <- data.frame(from = c(1,2), to = c(2,3))
#'
#'visNetwork(nodes, edges) %>%
#'  visGroups(groupname = "A", color = "red") %>%
#'  visGroups(groupname = "B", color = "lightblue") %>%
#'  visLegend()%>% visExport() 
#'  
#'visNetwork(nodes, edges) %>%
#'  visGroups(groupname = "A", color = "red") %>%
#'  visGroups(groupname = "B", color = "lightblue") %>%
#'  visLegend()%>% visExport(type = "jpeg", name = "export-network", 
#'    float = "left", label = "Save network", background = "purple", style= "") 
#'  
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visExport <- function(graph, type = "png", name = "network",
                         label = paste0("Export as ", type),
                         background = "#fff", float = "right", 
                         style = NULL, loadDependencies = TRUE){
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visSetExport with visNetworkProxy object")
  }
  
  if(!any(class(graph) %in% "visNetwork")){
    stop("graph must be a visNetwork object")
  }
  
  stopifnot(type%in%c("png", "jpeg"))
  
  if(is.null(style)){
    css <- paste0("float:", float, 
                  ";background: #3498db;background-image: -webkit-linear-gradient(top, #3498db, #2980b9);
                  background-image: -moz-linear-gradient(top, #3498db, #2980b9);
                  background-image: -ms-linear-gradient(top, #3498db, #2980b9);
                  background-image: -o-linear-gradient(top, #3498db, #2980b9);
                  background-image: linear-gradient(to bottom, #3498db, #2980b9);
                  -webkit-border-radius: 28;-moz-border-radius: 28;border-radius: 28px;
                  font-family: Arial;color: #ffffff;font-size: 10px;padding: 1px 3px 1px 3px;text-decoration: none;")
  }else{
    css <- paste0("float:", float, ";", style)
  }
  
  export <- list()
  export$type <- type
  export$css <- css
  export$background <- background
  export$name <- paste0(name, ".", type)
  export$label <- label
  
  graph$x$export <- export
  
  if(loadDependencies){
    graph <- addExport(graph)
  }
  
  graph
}