require(shiny)
require(visNetwork)

shinyServer(function(input, output) {
  
  output$network <- renderVisNetwork({
    nb = input$nb
    nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
                        group = sample(1:nb, nb, replace = TRUE), value = 1:nb,
                        title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)
    
    edges <- data.frame(from = trunc(runif(nb)*(nb-1))+1,
                        to = trunc(runif(nb)*(nb-1))+1,
                        value = rnorm(nb, 10), label = paste("Edge", 1:nb),
                        title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))
    
    # custom navigation
    visNetwork(nodes, edges, legend = TRUE) %>% visOptions(navigation = TRUE, dataManipulation = TRUE, stabilize = FALSE)
  })
  
})