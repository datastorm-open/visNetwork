require(shiny)
require(visNetwork)

shinyUI(fluidPage(
  
     sliderInput("nb", "number of nodes : ", min = 2, max = 1000, value = 10),
     actionButton("goButton", "Go!"),
     checkboxInput("legend2", "legend", value = TRUE),
     visNetworkOutput("network")
    
))