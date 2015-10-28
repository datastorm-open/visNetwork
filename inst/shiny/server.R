require(shiny)
require(visNetwork)

shinyServer(function(input, output) {
  
  output$network <- renderVisNetwork({
    input$goButton
    isolate({
      nb = input$nb
      nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
                          enjeux = sample(c("Faible", "Moyen","Fort"), 10, replace = TRUE),
                          group = sample(1:10, nb, replace = TRUE), value = 1:nb,
                          title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)
      
      edges <- data.frame(from = trunc(runif(nb)*(nb-1))+1,
                          to = trunc(runif(nb)*(nb-1))+1,
                          value = rnorm(nb, 10), label = paste("Edge", 1:nb),
                          title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))
      
      # custom navigation
      visNetwork(nodes, edges) %>% 
        visPhysics(stabilization = FALSE) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visOptions(manipulation = TRUE,
                   highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "enjeux") %>%
        visLegend(enabled =  input$legend2)
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
    
    visNetwork(nodes, edges) %>%
      visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
      visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>%
      addFontAwesome() %>%
      visLegend(addNodes = list(
        list(label = "A", shape = "icon", icon = list(code = "f0c0", size = 25)),
        list(label = "B", shape = "icon", icon = list(code = "f007", size = 50, color = "red"))
        ),
        addEdges = data.frame(label = "link"), useGroups = FALSE)
    
  })
  
  output$test <- renderText({
    paste(input$network_selected, input$network_selectedBy)
  })
  
})