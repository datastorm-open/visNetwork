require(shiny)
require(visNetwork)

shinyUI(fluidPage(
  
     sliderInput("nb", "number of nodes : ", min = 2, max = 1000, value = 10),
     visNetworkOutput("network", height = "600px")
    
))