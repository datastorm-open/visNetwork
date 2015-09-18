require(shiny)
require(visNetwork)

shinyServer(function(input, output) {
  
  output$network <- renderVisNetwork({
    input$goButton
    isolate({
      nb = input$nb
      nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
                          group = sample(1:10, nb, replace = TRUE), value = 1:nb,
                          title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)
      
      edges <- data.frame(from = trunc(runif(nb)*(nb-1))+1,
                          to = trunc(runif(nb)*(nb-1))+1,
                          value = rnorm(nb, 10), label = paste("Edge", 1:nb),
                          title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))
      
      # custom navigation
      visNetwork(nodes, edges, legend = input$legend2) %>% 
        visPhysics(stabilization = FALSE) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visOptions(manipulation = TRUE,
                   highlightNearest = TRUE, nodesIdSelection = TRUE)
    })
    
  })
  
  output$network2 <- renderVisNetwork({
    
#     nb = 5
#     nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
#                         group = sample(1:10, nb, replace = TRUE), value = 1:nb,
#                         title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)
#     
#     edges <- data.frame(from = trunc(runif(nb)*(nb-1))+1,
#                         to = trunc(runif(nb)*(nb-1))+1,
#                         value = rnorm(nb, 10), label = paste("Edge", 1:nb),
#                         title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))
#     
#     # custom navigation
#     visNetwork(nodes, edges) %>% visPhysics(stabilization = FALSE) %>%
#       visInteraction(navigationButtons = TRUE) %>%
#       visOptions(manipulation = TRUE,
#                highlightNearest = TRUE, nodesIdSelection = TRUE)
    
    nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
    edges <- data.frame(from = c(1,2), to = c(2,3))
    
    visNetwork(nodes, edges, legend = TRUE) %>%
      visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
      visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
      addFontAwesome()
    
  })
  
  output$test <- renderText({
    input$network_selected
  })
  
})