require(shiny)
require(visNetwork)

data <- iris
shinyApp(ui = fluidPage(visTreeModuleUI("id1", FALSE)), 
         server = function(input, output, session) {
           callModule(treeServLigth, "id1", data = reactive(rpart(data)), legend = reactive(FALSE))
           })
data("solder")
data <- solder
shinyApp(ui = fluidPage(visTreeModuleUI("id1", TRUE)), 
         server = function(input, output, session) {
           callModule(treeServ, "id1", data = reactive(data))
         })

library(ggplot2)
data(diamonds)
shinyApp(ui = 
           navbarPage("Menu",tabPanel(
             "tt1",fluidPage(visTreeModuleUI("id1", TRUE))
             ),
                      tabPanel(
                        "tt2",fluidPage(visTreeModuleUI("id2", TRUE)))
             ), 
         server = function(input, output, session) {
           callModule(treeServ, "id1", data = reactive(diamonds))
           callModule(treeServ, "id2", data = reactive(iris))
         })

data <- diamonds
shinyApp(ui = fluidPage(visTreeModuleUI("id1", FALSE, FALSE)), 
         server = function(input, output, session) {
           callModule(treeServLigth, "id1", data = reactive(rpart(data)))
         })


data <- diamonds
shinyApp(ui = fluidPage(treeUi("id1", FALSE, FALSE)), 
         server = function(input, output, session) {
           callModule(treeServ, "id1", data = reactive(data))
         })

