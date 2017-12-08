#' Visualize, customize and get back a \code{visNetwork} object. Need shiny package
#' 
#' @param  object : a \code{visNetwork} object
#' @param  filter : see \link{visConfigure}
#' @param  showButton : see \link{visConfigure}
#' 
#' @return a \code{visNetwork} object
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' nodes <- data.frame(id = 1:3, label = paste("Node", 1:3))
#' edges <- data.frame(from = c(1,2), to = c(1,3), label = paste("Edge", 1:2))
#' network <- visNetwork(nodes, edges)
#' 
#' custom_network <- visNetworkEditor(object = network)
#' custom_network
#' 
#' custom_network <- visNetworkEditor(object = network, filter = "nodes,edges")
#' custom_network
#' 
#' }
#' 
#' @export
#' 
#' @importFrom  utils packageVersion
#' 
#' @seealso \link{visConfigure}, \link{visTree}, \link{visNetworkEditorServer}
#'
#' @references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
#'
visNetworkEditor <- function(object, filter = NULL, showButton = NULL){
  
  if(!requireNamespace("shiny")){
    stop("visNetworkEditor require 'shiny' package")
  } else {
    if(packageVersion("shiny") < '1.0.0'){
      stop("visNetworkEditor require 'shiny' 1.0.0 or more")
    }
  }
  
  # server
  server <- function(input, output, session) {
    shiny::callModule(visNetworkEditorServer, id = "editor" , 
               object = shiny::reactive(object), 
               filter = shiny::reactive(filter), 
               showButton = shiny::reactive(showButton))
  }
  
  ui <- shiny::fluidPage(
    visNetworkEditorUI(id = "editor", quitButton = TRUE)
  )
  
  return(shiny::runApp(shiny::shinyApp(ui = ui, server = server)))
}

#' Module shiny for visualize and customize and get back a \code{visNetwork} object.
#' Using the javascript interface \link{visConfigure}.
#'
#' @param  input  \code{list} shiny input
#' @param  output \code{list}, shiny output
#' @param  session  \code{list}, shiny session
#' @param  id \code{character} id of module, linked to  \link{visNetworkEditorUI}
#' @param  object a \code{visNetwork} object. Must be a reactive.
#' @param  filter : see \link{visConfigure}. Must be a reactive.
#' @param  showButton : see \link{visConfigure}. Must be a reactive.
#' @param  quitButton : logical. Add a button for quit shiny and get back network in R ?
#' @param  height : height of the configuration div. Defaut to "700px"
#' 
#' @examples
#' \dontrun{
#' 
#' nodes <- data.frame(id = 1:3, label = paste("Node", 1:3))
#' edges <- data.frame(from = c(1,2), to = c(1,3), label = paste("Edge", 1:2))
#' network <- visNetwork(nodes, edges)
#' 
#' shiny::shinyApp(ui = shiny::fluidPage(
#'  visNetworkEditorUI(id = "id1")), 
#'  server = function(input, output, session) {
#'  shiny::callModule(visNetworkEditorServer, "id1", object = shiny::reactive(network))
#' })
#' 
#' }
#' @name visNetworkEditor-module
#' 
#' @export
#' 
#' @seealso \link{visConfigure}, \link{visTree}, \link{visNetworkEditor}
#' 
#' @references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
#'
#'
visNetworkEditorServer <- function(input, output, session, object, 
                                   filter = shiny::reactive(NULL), showButton = shiny::reactive(NULL)) {
  
  ns <- session$ns
  
  # renderNetwork
  output$network <- renderVisNetwork({
    object() %>% 
      visConfigure(enabled = TRUE, container = ns("configure"), 
                   filter = filter(), showButton = showButton())
  })
  
  output$network_ui <- shiny::renderUI({
    height <- "700px"
    if(!is.null(object()$height)){
      height <- object()$height
    }
    visNetworkOutput(ns("network"), height = height)
  })
  
  # retrieve new options on quit
  shiny::observe({
    if(!is.null(input$quit_btn)){
      if(input$quit_btn > 0){
        session$sendCustomMessage(
          type='visShinyGetOptionsFromConfigurator', 
          message=list(id = ns("network"), input = ns(paste0("network", "_", "configurator")))
        )
      }
    }
  })
  
  # quit and get back new network
  shiny::observe({
    if(!is.null(input$network_configurator)){
      update_network <- object() %>% 
        visSetOptions(options = input$network_configurator) %>%
        visConfigure(enabled = FALSE)
      shiny::stopApp(update_network)
    }
  })
}


#' @rdname visNetworkEditor-module
#' 
#' @export
visNetworkEditorUI <- function(id, quitButton = FALSE, height = "700px") {
  ns <- shiny::NS(id)
  
  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(HTML("input.vis-configuration.vis-config-rangeinput {visibility : hidden;}"))
    ),
    
    shiny::fluidRow(
      shiny::column(4,
                    shiny::div(id = ns("configure"), 
                               style = paste0("overflow: auto;overflow-x: hidden; height:", height, ";")),
                    if(quitButton){
                      shiny::div(hr(), shiny::actionButton(ns("quit_btn"), "Quit and get back network in R"), align = "center")
                    }
      ), 
      shiny::column(8, shiny::uiOutput(ns("network_ui")))
    )
  )
}