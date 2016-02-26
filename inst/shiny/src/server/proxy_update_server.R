data <- reactive({
  set.seed(2)
  nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
                      group = sample(LETTERS[1:3], 15, replace = TRUE))
  
  edges <- data.frame(id = 1:15, from = trunc(runif(15)*(15-1))+1,
                      to = trunc(runif(15)*(15-1))+1)
  list(nodes = nodes, edges = edges)
})

output$network_proxy_update <- renderVisNetwork({
  visNetwork(data()$nodes, data()$edges) %>% visLegend()
})

observe({
  input$goButton
  nodes <- data.frame(id = 1:15, 
                      group = sample(LETTERS[1:3], 15, replace = TRUE))
  
  edges <- data.frame(id = 1:15, color = sample(c("red", "blue"), 15, replace = TRUE))
  
  visNetworkProxy("network_proxy_update") %>%
    visUpdateNodes(nodes = nodes) %>%
    visUpdateEdges(edges = edges)
})


output$code_proxy_update  <- renderText({
  '
observe({
  input$goButton
  nodes <- data.frame(id = 1:15, 
                      group = sample(LETTERS[1:3], 15, replace = TRUE))
  
  edges <- data.frame(id = 1:15, color = sample(c("red", "blue"), 15, replace = TRUE))
  
  visNetworkProxy("network_proxy_update") %>%
    visUpdateNodes(nodes = nodes) %>%
    visUpdateEdges(edges = edges)
})
 '
})