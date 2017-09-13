#' Network export configuration
#'
#' Network export configuration. This function only work within shiny or a web browser.
#' 
#'@param graph : a visNetwork object
#'@param type : Type of export. One of "png" (default), "jpeg" or "pdf"
#'@param name : name of imgage, default to "network"
#'@param label : Label on button, default to "Export as png/jpeg/pdf"
#'@param background : background color, default to white (#fff). Work only if network background is transparent.
#'@param float : button postion, default to "right" 
#'@param style : button css style.
#'@param loadDependencies / Boolean. TRUE by default. Load libraries for export (fileSaver, Blob, canvastoBlob, html2canvas, jsPDF)
#'@param ... : arguments for \link{addExport}
#'
#'@examples
#'
#'\dontrun{
#'
#'nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
#'edges <- data.frame(from = c(1,2), to = c(2,3))
#'
#'visNetwork(nodes, edges) %>%
#'  visGroups(groupname = "A", color = "red") %>%
#'  visGroups(groupname = "B", color = "lightblue") %>%
#'  visLegend() %>% visExport() 
#'  
#'visNetwork(nodes, edges) %>%
#'  visGroups(groupname = "A", color = "red") %>%
#'  visGroups(groupname = "B", color = "lightblue") %>%
#'  visLegend() %>% visExport(type = "jpeg", name = "export-network", 
#'    float = "left", label = "Save network", background = "purple", style= "") 
#'  
#'}
#'@seealso \link{visSave}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visExport <- function(graph, type = "png", name = "network",
                         label = paste0("Export as ", type),
                         background = "#fff", float = "right", 
                         style = NULL, loadDependencies = TRUE, ...){
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visExport with visNetworkProxy object")
  }
  
  if(!any(class(graph) %in% "visNetwork")){
    stop("graph must be a visNetwork object")
  }
  
  stopifnot(type%in%c("png", "jpeg", "pdf"))
  
  if(is.null(style)){
    css <- paste0("float:", float, 
                  ";-webkit-border-radius: 10;
                  -moz-border-radius: 10;
                  border-radius: 10px;
                  font-family: Arial;
                  color: #ffffff;
                  font-size: 12px;
                  background: #090a0a;
                  padding: 4px 8px 4px 4px;
                  text-decoration: none;")
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
    graph <- addExport(graph, ...)
  }
  
  graph
}