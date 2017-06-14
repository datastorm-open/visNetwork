require(shiny)
require(visNetwork)

data <- iris
shinyApp(ui = fluidPage(visTreeModuleUI("id1", FALSE)), 
         server = function(input, output, session) {
           callModule(visTreeModuleServerRpart, "id1", data = reactive(rpart(data)))
         })

data("solder")
res <- rpart(Opening~., data = solder, control = rpart.control(cp = 0.00005))
shinyApp(ui = fluidPage(visTreeModuleUI("id1", FALSE)), 
         server = function(input, output, session) {
           callModule(visTreeModuleServerRpart, "id1", data = reactive(res))
         })

data <- iris
shinyApp(ui = fluidPage(visTreeModuleUI("id1", FALSE)), 
         server = function(input, output, session) {
           callModule(visTreeModuleServerRpart, "id1", data = reactive(rpart(data)), legend = reactive(TRUE), 
                      colorY = c("orange", "red"),
                      updateShape = FALSE, rules = reactive(FALSE),
                      export = FALSE, digits = 0, height = 700, tooltipDelay = 100, 
                      legendPosition = "right", legendNcol = 2, legendWidth = 0.2,
                      maxNodeSize = 50, shapeY = "diamond", direction = "DU",
                      colorEdges = "black", edgesFontAlign = "middle", edgesFontSize = 20,
                      collapse = FALSE, highlightNearest = FALSE, colorVar = c("red", "blue"), 
                      main = list(text = "visNetwork minimal example",
                                         style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;"),
                      footer  = list(text = "visNetwork minimal example",
                                           style = "font-family:Comic Sans MS;color:#ff0000;font-size:5px;text-align:center;"), 
                      submain = list(text = "visNetwork minimal example",
                                  style = "font-family:Comic Sans MS;color:#ff0000;font-size:5px;text-align:center;"))
           })

data("solder")
data <- solder
shinyApp(ui = fluidPage(visTreeModuleUI("id1", TRUE)), 
         server = function(input, output, session) {
           callModule(visTreeModuleServerData, "id1", data = reactive(data))
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
           callModule(visTreeModuleServerData, "id1", data = reactive(diamonds))
           callModule(visTreeModuleServerData, "id2", data = reactive(iris))
         })

data <- diamonds
shinyApp(ui = fluidPage(visTreeModuleUI("id1", FALSE, FALSE)), 
         server = function(input, output, session) {
           callModule(visTreeModuleServerRpart, "id1", data = reactive(rpart(data)))
         })


data <- diamonds
shinyApp(ui = fluidPage(treeUi("id1", FALSE, FALSE)), 
         server = function(input, output, session) {
           callModule(visTreeModuleServerData, "id1", data = reactive(data))
         })

