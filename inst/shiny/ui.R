require(shiny)
require(visNetwork)

shinyUI(fluidPage(
  
     visNetworkOutput("network", height = "600px")
    
))