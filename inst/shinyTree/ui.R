#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shinyWidgets)
library(shiny)
library(shinydashboard)
#library(shinyjs)
dta <- ggplot2::diamonds
library(visNetwork)
# Define UI for application that draws a histogram
dashboardPage(
 
    dashboardHeader(title = "Tree visualisation", disable = TRUE),
    dashboardSidebar( disable = TRUE),

    dashboardBody(
      fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
      #useShinyjs(),
          
      

      fluidRow(
        
        fluidRow(
          div(#align = "center",
              
      box(id = "boxparam",width = 12,
          collapsible = TRUE,
          tabBox(width = 12,
                 tabPanel("rpart params",
                          fluidRow(
                          div(align = "center",
                              # column(12, div(class = "hrdivide")),
                          #h3(class="treetitle","Rpart params"),
                          
                            column(1),
                            column(2,
                                   selectInput("y", "Y :", names(dta))),
                            column(4,
                                   selectInput("x", "X :", NULL, multiple = TRUE, selected = NULL)),
                            column(2,
                                   numericInput("complexity", 
                                                "Complexity :",
                                                value = 0.005,
                                                step = 0.001)),
                            column(2,
                                   numericInput("minsplit", "Min split", value = 20, min = 2))
                            
                          )
                            
                      
                 )),
                 
              
                 tabPanel("General params",
                          fluidRow(
                            # column(12, div(class = "hrdivide")),
                            # column(12, h3(class="treetitle","General")),
                
                            column(2, numericInput("heigth", "Heigth :",650)),
                          column(2,textInput("main",
                                    "Main :", "")),
                          column(2,numericInput("digits", "Digits",
                                                value = 3, min = 0)),
                          
                          column(2,selectInput("direction", "Direction :", 
                                               choices = c(
                                                 "up-down" = "UD",
                                                 "down-up" = "DU",
                                                 "left-right" = "LR",
                                                 "right-left" = "RL"),
                                               selected = "UD")),
                          
                          
                          column(2,br(),awesomeCheckbox("fallenLeaves", "Fallen leaves")),
                          column(2,br(),awesomeCheckbox("updateShape", "Update shape",value = TRUE)),
                          
                          column(2,br(),materialSwitch("nodesPopSize", "Nodes size use population",status = "info")),
                          conditionalPanel("input.nodesPopSize == true",
                                           column(2,numericInput("minNodeSize", "Min nodes size",
                                                                 value = 15, min = 1)),                          
                                           column(2,numericInput("maxNodeSize", "Max nodes size",
                                                                 value = 30, min = 1)))
                          )),
                
                          ##Edges##
                 
                          ##End Edges##
                 tabPanel("Tooltips params",
                          fluidRow(
                          ##Tooltips##
                          # column(12, div(class = "hrdivide")),
                          # column(12,h3(class="treetitle","Tooltips")),
                          column(3),
                          column(2,numericInput("tooltipDelay", "Tooltip delay :",
                                                value = 500, min = 0, step = 100)),
                          column(2,br(),materialSwitch("rules","Display rules", value = TRUE, status = "info")),
                          conditionalPanel("input.rules == true",
                          column(2,br(),awesomeCheckbox("simpRules","Simplify rules", value = TRUE))))),
                          ##End Tooltips##
                 tabPanel("Legend params",
                          fluidRow(
                          ##Legend##
                          # column(12, div(class = "hrdivide")),
                          # column(12, h3(class="treetitle","Legend")),
                          column(1),
                          column(2, br(),materialSwitch("legend","Display legend",
                                                  value = TRUE, status = "info")),
                          conditionalPanel("input.legend == true",
                                           column(2,numericInput("legendWidth",
                                                        "Legend width",
                                                        value = 0.1, 
                                                        min = 0,
                                                        max = 1,
                                                        step = 0.01)),
                                           column(2,numericInput("legendNcol",
                                                                 "Legend number of column",
                                                                 value = 1, 
                                                                 min = 1,
                                                                 max = 50,
                                                                 step = 1)),
                                           column(2,numericInput("legendNodesSize", "Legend nodes size :",
                                                                 value = 22, min = 1)),
                                           column(2,numericInput("legendFontSize", "Legend font size :",
                                                                 value = 16, min = 1))
                                           
                                           
                          )))
                          ##End Legend##
                 ),
          div(actionButton("runTree",
                           "Run tree", class = "btnruntree"),align = "center")))
                 
                 ),
      
 
      # Show a plot of the generated distribution
      box(width = 12,
          # uiOutput("treeUI"),
          column(1, dropdownButton(icon = "Nodes",status = "danger",width = 400,
            fluidRow(
              ##Nodes##
              # column(12, div(class = "hrdivide")),
              # column(12, h3(class="treetitle","Nodes")),
              
              column(6,selectInput("shapeY", "shape Y :", 
                                   choices = c("diamond",
                                               "dot",
                                               "star",
                                               "triangle",
                                               "triangleDown",
                                               "square"),
                                   selected = "square"
              )),
              column(6,selectInput("shapeX", "shape X :", 
                                   choices = c("diamond",
                                               "dot",
                                               "star",
                                               "triangle",
                                               "triangleDown",
                                               "square"),
                                   selected = "dot"
              )),
              column(6,numericInput("nodesFontSize", "Nodes font size :",
                                    value = 16, min = 1)),
              
              
              column(12,br(), materialSwitch("usecolornodes","Color nodes", status = "info")),
              uiOutput("colornodes"),
              
              column(12,br(), materialSwitch("usecolorY","Color Y", status = "info")),
              
              uiOutput("colorY"))
          )),
          
          column(1,
          dropdownButton(icon = "Edges",status = "warning",width = 400,
                         
          fluidRow(
            column(6,colourpicker::colourInput("colorEdges", "Color edges :",
                                               value = "#8181F7")),
            column(6,numericInput("edgesFontSize", "Edges font size :",
                                  value = 14, min = 1)),
            column(6,selectInput("edgesFontAlign","Edges font align",choices = c(
              "horizontal", "top", "middle", "bottom"
            )))))),
          column(12),
          
          
         
      uiOutput("treeUI"),
          downloadLink('downloadNetwork', 'Download Tree')
      )
      )
    
  )
    )
)
