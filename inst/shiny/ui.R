require(shiny)
require(visNetwork)

shinyUI(fluidPage(
  
     sliderInput("nb", "number of nodes : ", min = 2, max = 1000, value = 10),
     actionButton("goButton", "Go!"),
     checkboxInput("legend2", "legend", value = TRUE),
     h4("input$network_selected"),verbatimTextOutput("test"),
     h4("output$network"),
     visNetworkOutput("network",height = "600px"),
     hr(),
     div(h3("Just to test two networks on same apps"), align = "center"),
     visNetworkOutput("network2")
    
))