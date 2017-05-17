






data <- iris
shinyApp(ui = fluidPage(treeUi("id1", FALSE)), 
         server = function(input, output, session) {
           callModule(treeServLigth, "id1", data = reactive(rpart(data)))
           })



data <- iris
shinyApp(ui = fluidPage(treeUi("id1", TRUE)), 
         server = function(input, output, session) {
           callModule(treeServ, "id1", data = reactive(data))
         })

library(ggplot2)
data(diamonds)
shinyApp(ui = 
           navbarPage("Menu",tabPanel(
             "tt1",fluidPage(treeUi("id1", TRUE))
             ),
                      tabPanel(
                        "tt2",fluidPage(treeUi("id2", TRUE)))
             ), 
         server = function(input, output, session) {
           callModule(treeServ, "id1", data = reactive(diamonds))
           callModule(treeServ, "id2", data = reactive(iris))
         })

data <- diamonds
shinyApp(ui = fluidPage(treeUi("id1", FALSE, FALSE)), 
         server = function(input, output, session) {
           callModule(treeServLigth, "id1", data = reactive(rpart(data)))
         })


data <- diamonds
shinyApp(ui = fluidPage(treeUi("id1", FALSE, FALSE)), 
         server = function(input, output, session) {
           callModule(treeServ, "id1", data = reactive(data))
         })

