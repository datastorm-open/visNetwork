data <- diamonds
shinyApp(ui = fluidPage(AllAppUI("id1", FALSE)), 
         server = function(input, output, session) {
           callModule(treeFromrpart, "id1", data = reactive(rpart(data)))
           })



data <- iris
shinyApp(ui = fluidPage(AllAppUI("id1", TRUE)), 
         server = function(input, output, session) {
           callModule(AllApp, "id1", data = reactive(data))
         })
