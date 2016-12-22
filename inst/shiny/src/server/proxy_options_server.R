dataos <- reactive({
  set.seed(2)
  nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
                      group = sample(LETTERS[1:3], 15, replace = TRUE), 
                      group2 = paste(sample(LETTERS[1:3], 15, replace = TRUE), sample(LETTERS[1:3], 15, replace = TRUE), sep= ","))
  
  edges <- data.frame(id = 1:15, from = trunc(runif(15)*(15-1))+1,
                      to = trunc(runif(15)*(15-1))+1)
  list(nodes = nodes, edges = edges)
})

output$network_proxy_options <- renderVisNetwork({
  visNetwork(dataos()$nodes, dataos()$edges) %>% visEdges(arrows = "to") %>% 
    visLegend()
})

observe({
  col <- paste0('rgba(200,200,200,', input$opahigh, ')')
  visNetworkProxy("network_proxy_options") %>%
    visOptions(highlightNearest = list(enabled = input$highlightNearest, hover = input$hover,
                                       algorithm = input$algorithm, degree = input$deg, hideColor = col))
})

observe({
  visNetworkProxy("network_proxy_options") %>%
    visOptions(nodesIdSelection = list(enabled = input$nodesIdSelection, selected = 5))
})

observe({
  if(input$selectedby){
    col <- paste0('rgba(200,200,200,', input$opasel, ')')
    visNetworkProxy("network_proxy_options") %>%
      visOptions(selectedBy = list(variable = "group", hideColor = col))
  }else{
    visNetworkProxy("network_proxy_options") %>%
      visOptions(selectedBy = NULL)
  }
})

# observe({
#   if(length(input$selectedby) > 0){
#     visNetworkProxy("network_proxy_options") %>%
#       visOptions(selectedBy = list(variable = "group", values = input$selectedby, selected = input$selectedby[1]))
#   }else{
#     visNetworkProxy("network_proxy_options") %>%
#       visOptions(selectedBy = NULL)
#   }
# 
# })

output$code_proxy_options  <- renderText({
  '
observe({
  col <- paste0("rgba(200,200,200,", input$opahigh, ")")
  visNetworkProxy("network_proxy_options") %>%
    visOptions(highlightNearest = list(enabled = input$highlightNearest, hover = input$hover,
                                       algorithm = input$algorithm, degree = input$deg, hideColor = col))
})

observe({
  visNetworkProxy("network_proxy_options") %>%
    visOptions(nodesIdSelection = list(enabled = input$nodesIdSelection, selected = 5))
})

observe({
  if(input$selectedby){
    col <- paste0("rgba(200,200,200,", input$opasel, ")")
    visNetworkProxy("network_proxy_options") %>%
      visOptions(selectedBy = list(variable = "group", hideColor = col))
  }else{
    visNetworkProxy("network_proxy_options") %>%
      visOptions(selectedBy = NULL)
  }
})
 '
})