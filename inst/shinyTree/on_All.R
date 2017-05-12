#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
library(rpart)
library(colourpicker)
library(ggplot2)
# dta <- diamonds

# Define server logic required to draw a histogram
AllApp <- function(input, output, session, data) {
  
  dta <- reactive({data()})
  
  rPart <- reactive({
    input$runTree
    input$downloadNetwork
    req(input$runTree > 0)
    isolate({
      formul <- paste(input$y, "~", paste0(input$x, collapse = "+"))%>%as.formula()
      rpart(formul, data=dta() , control = rpart.control(cp = input$complexity,
                                                         minsplit = input$minsplit))
    })
  })
  observe({
    updateSelectInput(session, inputId = "y", choices = names(dta()))
  })
  
  ns <- session$ns
  
  observe({
    updateSelectInput(session, inputId = "x", choices = names(dta())[names(dta())!=input$y],
                      selected = names(dta())[names(dta())!=input$y])
  })
  
  observe({
    treeFromrpart(input = input, output = output, session = session, data = rPart)
  })
}

treeFromrpart <- function(input, output, session, data) {
  
  ns <- session$ns
  
  rPart <- reactive({
    input$runTree
    input$downloadNetwork
    data()
  })
  
  
  observe({
    input$y
    input$x
    input$complexity
    input$minsplit
    updateCheckboxInput(session,"usecolornodes", value = FALSE )
    updateCheckboxInput(session,"usecolorY", value = FALSE )
    
  })
  
  
  updateColorVar <- reactive({
    colorVar <- colorVarData()
    dest <- gsub(" ", "", paste0(sortLabels(), "var"))
    if(!is.null(input[[dest[1]]])){
      colorVect <- sapply(dest, function(X)input[[X]])
      colorVar$color <- colorVect
    }
    colorVar
  })
  updateColorY<- reactive({
    colorY <- colorY()
    clas <- attributes(rPart())$ylevels
    if(!is.null(clas)){
      dest <- paste0(gsub(" ", "", paste0(colorY$modality, "Y")))
      if(!is.null(input[[dest[1]]])){
        colorVect <- sapply(dest, function(X)input[[X]])
        colorY$color <- colorVect
      }
    }else{
      if(!is.null(input$minY))
      {
        colorY[1] <- input$minY
        colorY[2] <- input$maxY
      }
    }
    colorY
  })
  
  
  treeBuild <- reactive({
    input$downloadNetwork
    res <- rPart()
    isolate({
      
      colorVar <- updateColorVar()
      colorY <- updateColorY()
      
      simpRules <- input$simpRules
      simpRules <- ifelse(is.null(simpRules),FALSE,simpRules)
      
      legendWidth <- input$legendWidth
      legendWidth <- ifelse(is.null(legendWidth),0.1,legendWidth)
      
      legendNcol <- input$legendNcol
      legendNcol <- ifelse(is.null(legendNcol),1,legendNcol)
      
      legendNodesSize <- input$legendNodesSize
      legendNodesSize <- ifelse(is.null(legendNodesSize),22,legendNodesSize)
      
      legendFontSize <- input$legendFontSize
      legendFontSize <- ifelse(is.null(legendFontSize),16,legendFontSize)
      
      minNodeSize <- input$minNodeSize
      minNodeSize <- ifelse(is.null(minNodeSize),15,minNodeSize)
      
      maxNodeSize <- input$maxNodeSize
      maxNodeSize <- ifelse(is.null(maxNodeSize),30,maxNodeSize)
      
      
      visTree(res,
              main = getMain(),
              submain = getSubMain(),
              footer = getFooter(),
              shapeY = input$shapeY,
              shapeVar = input$shapeX,
              colorVar = colorVar,
              colorY = colorY,
              rules = input$rules,
              simplifyRules = simpRules,
              legend = input$legend,
              legendWidth = legendWidth,
              legendNodesSize = legendNodesSize,
              legendFontSize = legendFontSize,
              legendNcol = legendNcol,
              edgesFontSize = input$edgesFontSize,
              edgesFontAlign = input$edgesFontAlign,
              nodesFontSize = input$nodesFontSize,
              nodesPopSize = input$nodesPopSize,
              minNodeSize = minNodeSize,
              maxNodeSize = maxNodeSize,
              tooltipDelay = input$tooltipDelay,
              digits = input$digits,
              direction = input$direction,
              fallenLeaves = input$fallenLeaves,
              updateShape = input$updateShape,
              colorEdges = input$colorEdges,
              height = paste0(input$heigth, "px"))
    })
  })
  
  
  output$tree <- renderVisNetwork({
    # Basic classification tree
    treeBuild()
  })
  
  
  varNodes <- reactive({
    res <- rPart()
    var <- levels(res$frame$var)[levels(res$frame$var)!="<leaf>"]
    var
  })
  
  sortLabels <- reactive({
    sort(unique(varNodes()))
  })
  
  colorVarData <- reactive({
    color <- visNetwork:::.generateVarColor(vardecided = varNodes(), SortLabel = sortLabels())
    colNod <- color[match(varNodes(),  sortLabels())]
    data.frame(variable = varNodes(), color = colNod)
  })
  
  
  
  colorY <- reactive({
    clas <- attributes(rPart())$ylevels
    if(!is.null(clas)){
      nbClas <- length(clas)
      VAL <- rPart()$frame[,"yval2"]
      effectif <- VAL[,2:(nbClas+1)]
      effectif <- data.frame(effectif)
      probs <- VAL[,(nbClas+2):(ncol(VAL)-1)]
      probs <- data.frame(probs)
      probs2 <- data.frame(probs)
      vardecidedClust <- clas[apply(probs2, 1, which.max)]
      colorTerm <- visNetwork:::.generateYColor(clas)
      data.frame(modality = clas, color = colorTerm)
    }else{
      c("#E6E0F8", "#8904B1")
    }
  })
  
  
  
  output$colorY <- renderUI({
    if(!input$usecolorY){
      return(NULL)
    }else{
      corY <- updateColorY()
      clas <- attributes(rPart())$ylevels
      if(!is.null(clas)){
        res <- apply(corY, 1, function(x){
          as.character(column(6,colourInput(ns(gsub(" ", "", paste0(x[1],"Y"))), paste0(x[1]," :"), x[2])))
        })%>%
          paste0(collapse = "")%>%
          HTML()
      }else{
        res <- list()
        res[[1]] <- as.character(column(6,
                                        colourInput(ns("minY"), "Min y : ", corY[1])))
        res[[2]] <- as.character(column(6,
                                        colourInput(ns("maxY"), "Max y : ", corY[2])))
        res <- paste0(res, collapse = "")%>%
          HTML()
      }
    }
    res
  })
  
  # observe({
  #   m <- treeBuild()
  #   
  # 
  #   visNetworkProxy(ns("tree")) %>%
  #     visEdges(font = list(size = input$edgesFontSize,
  #                          align = input$edgesFontAlign)) %>%
  #     visNodes(font = list(size = input$nodesFontSize))
  # })
  # 
  # 
  
  ##Update color VarNodes
  observe({
    m <- treeBuild()
    oldId <- m$x$nodes$id
    
    legOldId <- m$x$legend$nodes$id
    colorIn <- m$x$nodes$color
    legColorIn <- m$x$legend$nodes$color
    outColor <- unlist(sapply(m$x$nodes$label, function(x){
      if(is.null(input[[gsub(" ", "", paste0(x[1],"var"))]]))
      {
        "NoChange"
      }else{
        input[[gsub(" ", "", paste0(x[1],"var"))]]
      }
      
    }))
    
    legOutColor <- unlist(sapply(m$x$legend$nodes$label, function(x){
      if(is.null(input[[gsub(" ", "", paste0(x[1],"var"))]]))
      {
        "NoChange"
      }else{
        input[[gsub(" ", "", paste0(x[1],"var"))]]
      }
      
    }))
    
    newColor <- data.frame(id = oldId[outColor != "NoChange" &
                                        ifelse(outColor == colorIn, FALSE, TRUE)],
                           color = outColor[outColor != "NoChange" &
                                              ifelse(outColor == colorIn, FALSE, TRUE)])
    legNewColor <- data.frame(id = legOldId[legOutColor != "NoChange" &
                                              ifelse(legOutColor == legColorIn, FALSE, TRUE)],
                              color = legOutColor[legOutColor != "NoChange" &
                                                    ifelse(legOutColor == legColorIn, FALSE, TRUE)])
    
    
    
    
    
    clas <- attributes(rPart())$ylevels
    
    if(!is.null(input$minY) & is.null(clas))
    {
      coloramp <- visNetwork:::.creatColorRampY(c(input$minY, input$maxY))
      colorTerm <- rgb(coloramp((m$x$nodes$labelClust-min(m$x$nodes$labelClust))/(max(m$x$nodes$labelClust-min(m$x$nodes$labelClust)))), maxColorValue=255)
      endf <- data.frame(id = m$x$nodes$id, colorClust = colorTerm, color = "")
      endf$color <- as.character(endf$color)
      endf$color[m$x$nodes$Leaf == 1] <- as.character(colorTerm[m$x$nodes$Leaf == 1])
      endf$color[m$x$nodes$Leaf == 0] <- as.character(m$x$nodes$color[m$x$nodes$Leaf == 0])
      
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(endf)
    }
    if(!is.null(input[[gsub(" ", "", paste0(m$x$nodes[1,]$label,"var"))]]))
    {
      oldId <- m$x$nodes$id
      legId <- m$x$legend$nodes$id
      colorIn <- m$x$nodes$color
      colorInLeg <- as.character(m$x$legend$nodes$color)
      outColor <- unlist(sapply(m$x$nodes$label, function(x){
        if(is.null(input[[gsub(" ", "", paste0(x[1],"Y"))]]))
        {
          "NoChange"
        }else{
          input[[gsub(" ", "", paste0(x[1],"Y"))]]
        }
      }))
      
      outColorLeg <- unlist(sapply(m$x$legend$nodes$label, function(x){
        if(is.null(input[[gsub(" ", "", paste0(x[1],"Y"))]]))
        {
          "NoChange"
        }else{
          input[[gsub(" ", "", paste0(x[1],"Y"))]]
        }
        
      }))
      
      newColorGraph <- data.frame(id = oldId[outColor != "NoChange" &
                                               ifelse(outColor == colorIn, FALSE, TRUE)],
                                  color = outColor[outColor != "NoChange" &
                                                     ifelse(outColor == colorIn, FALSE, TRUE)])
      newColorLegend <- data.frame(id = legId[outColorLeg != "NoChange" &
                                                ifelse(outColorLeg == colorInLeg, FALSE, TRUE)],
                                   color = outColorLeg[outColorLeg != "NoChange" &
                                                         ifelse(outColorLeg == colorInLeg, FALSE, TRUE)])
      
      
      
      
    }
    
    if(exists("newColor") & exists("newColorGraph") )
    {
      Gcol <- rbind(newColorGraph, newColor)
    }else{
      if(exists("newColor")){
        Gcol <- newColor
      }else{
        if(exists("newColorGraph")){
          Gcol <- newColorGraph
        }
      }
    }
    
    if(exists("legNewColor") & exists("newColorLegend"))
    {
      Lcol <- rbind(newColorLegend, legNewColor)
    }else{
      if(exists("legNewColor")){
        Lcol <- legNewColor
      }else{
        if(exists("newColorLegend")){
          Lcol <- newColorLegend
        }
      }
    }
    
    if(exists("Gcol"))
    {
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(Gcol)%>%
        visUpdateNodes(Lcol, legend = TRUE)
    }
    
    
  })
  
  observe({
    m <- treeBuild()
    idEdges <- m$x$edges$id
    
    newEdges <- data.frame(id = m$x$edges$id, color = input$colorEdges)
    visNetworkProxy(ns("tree")) %>%
      visUpdateEdges(newEdges)
  })
  
  observe({
    m <- treeBuild()
    idEdges <- m$x$edges$id
    newEdges <- data.frame(id = m$x$edges$id, font.size = input$edgesFontSize)
    visNetworkProxy(ns("tree")) %>%
      visUpdateEdges(newEdges)
  })
  
  observe({
    m <- treeBuild()
    idEdges <- m$x$edges$id
    newEdges <- data.frame(id = m$x$edges$id, font.align = input$edgesFontAlign)
    visNetworkProxy(ns("tree")) %>%
      visUpdateEdges(newEdges)
  })
  
  observe({
    m <- treeBuild()
    newNodes <- data.frame(id = m$x$nodes$id, font.size = input$nodesFontSize)
    visNetworkProxy(ns("tree")) %>%
      visUpdateNodes(newNodes)
  })
  
  
  getMain <- reactive({
    text <- input$main
    style <- paste0("font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:",input$mainsize,"px;text-align:center;color:", input$colourMain, ";")
    list(text = text, style = style)
  })
  
  ##Update titles
  observe({
    visNetworkProxy(ns("tree")) %>%
      visSetTitle(main = getMain())
  })
  
  
  getSubMain <- reactive({
    text <- input$submain
      style <- paste0("font-family:Georgia, Times New Roman, Times, serif;font-size:",input$Submainsize,"px;text-align:center;color:", input$colourSubMain, ";")
    list(text = text, style = style)
  })
  
  observe({
    visNetworkProxy(ns("tree")) %>%
      visSetTitle(submain =  getSubMain())
  })
  
  
  getFooter <- reactive({
    text <- input$footer
    style <- paste0("font-family:Georgia, Times New Roman, Times, serif;font-size:",input$Footermainsize,"px;text-align:center;color:", input$colourFooterMain, ";")
    list(text = text, style = style)
  })
  
  observe({
    visNetworkProxy(ns("tree")) %>%
      visSetTitle(footer = getFooter())
  })
  
  
  ##Update shape Ynodes
  observe({
    m <- treeBuild()
    ret <- data.frame(id = m$x$nodes$id,
                      shape = ifelse(m$x$nodes$Leaf == 0, input$shapeX,input$shapeY ))
    visNetworkProxy(ns("tree")) %>%
      visUpdateNodes(ret)
  })
  
  highlightNearest <- reactive({
    list(enabled = input$highlightNearest, degree = list(from = 50000, to = 0),
         hover = input$highlightHover, algorithm = "hierarchical")
    
  })
  observe({
    visNetworkProxy(ns("tree")) %>%
      visOptions(highlightNearest = highlightNearest())
  })
  
  collapse <- reactive({
    list(enabled = input$collapse,fit = TRUE, resetHighlight = TRUE, clusterOptions = list(fixed = TRUE, physics= FALSE))
  })
  observe({
    visNetworkProxy(ns("tree")) %>%
      visOptions(collapse = collapse())
  })
  
  output$treeUI <- renderUI({
    res <- rPart()
    isolate({
      heigth <- ifelse(is.null(input$heigth), 650, input$heigth)
      visNetworkOutput(ns("tree"), height = paste0(heigth, "px"))
    })
  })
  
  output$downloadNetwork <- downloadHandler(
    filename = function() {
      paste('tree-', Sys.Date(), '.html', sep='')
    },
    content = function(con) {
      
      res <- rPart()
      
      colorVar <- updateColorVar()
      colorY <- updateColorY()
      
      simpRules <- input$simpRules
      simpRules <- ifelse(is.null(simpRules),FALSE,simpRules)
      
      legendWidth <- input$legendWidth
      legendWidth <- ifelse(is.null(legendWidth),0.1,legendWidth)
      
      legendNcol <- input$legendNcol
      legendNcol <- ifelse(is.null(legendNcol),1,legendNcol)
      
      legendNodesSize <- input$legendNodesSize
      legendNodesSize <- ifelse(is.null(legendNodesSize),22,legendNodesSize)
      
      legendFontSize <- input$legendFontSize
      legendFontSize <- ifelse(is.null(legendFontSize),16,legendFontSize)
      
      minNodeSize <- input$minNodeSize
      minNodeSize <- ifelse(is.null(minNodeSize),15,minNodeSize)
      
      maxNodeSize <- input$maxNodeSize
      maxNodeSize <- ifelse(is.null(maxNodeSize),30,maxNodeSize)
      
      
      out <- visTree(res,
                     main = getMain(),
                     submain = getSubMain(),
                     footer = getFooter(),
                     shapeY = input$shapeY,
                     shapeVar = input$shapeX,
                     colorVar = colorVar,
                     colorY = colorY,
                     rules = input$rules,
                     simplifyRules = simpRules,
                     legend = input$legend,
                     legendWidth = legendWidth,
                     legendNodesSize = legendNodesSize,
                     legendFontSize = legendFontSize,
                     legendNcol = legendNcol,
                     edgesFontSize = input$edgesFontSize,
                     edgesFontAlign = input$edgesFontAlign,
                     nodesFontSize = input$nodesFontSize,
                     nodesPopSize = input$nodesPopSize,
                     minNodeSize = minNodeSize,
                     maxNodeSize = maxNodeSize,
                     tooltipDelay = input$tooltipDelay,
                     digits = input$digits,
                     direction = input$direction,
                     fallenLeaves = input$fallenLeaves,
                     updateShape = input$updateShape,
                     colorEdges = input$colorEdges,
                     height =  "900px")
      
      out %>% visExport() %>% visSave(con)
      
    }
  )
  
  
  output$colornodes <- renderUI({
    if(!input$usecolornodes){
      return(NULL)
    }else{
      corNodes <- updateColorVar()
      res <- apply(corNodes, 1, function(x){
        as.character(column(6,colourpicker::colourInput(ns(gsub(" ", "", paste0(x[1],"var"))), paste0(x[1]," :"), x[2])))
      })%>%
        paste0(collapse = "")%>%
        HTML()
    }
    res
  })
  
  
}

library(shinyWidgets)
library(shiny)
library(ggplot2)
dta <- ggplot2::diamonds
library(visNetwork)
library(colourpicker)



AllAppUI <- function(id, rpartParam = TRUE) {
  ns <- NS(id)
  fluidPage(
    
    mainPanel(  width = 12,
                
                fluidRow(
                  tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                  ),
                  fluidRow(
                    div(class = "writecolor",
                        tabsetPanel(
                          id = ns('boxparam'),
                          if(rpartParam)
                          {
                            tabPanel("rpart params",
                                     fluidRow(
                                       div(align = "center",
                                           column(1),
                                           column(2,
                                                  selectInput(ns("y"), "Y :",NULL)),
                                           column(4,
                                                  selectInput(ns("x"), "X :", NULL, multiple = TRUE, selected = NULL)),
                                           column(2,
                                                  numericInput(ns("complexity"), 
                                                               "Complexity :",
                                                               value = 0.005,
                                                               step = 0.001)),
                                           column(2,
                                                  numericInput(ns("minsplit"), "Min split", value = 20, min = 2))
                                           
                                       )
                                       
                                       
                                     ))}else(div()),
                          
                          
                          tabPanel("General params",
                                   fluidRow(
                                     column(2, numericInput(ns("heigth"), "Heigth :",650)),
                                     
                                     column(2,numericInput(ns("digits"), "Digits",
                                                           value = 3, min = 0)),
                                     
                                     column(2,selectInput(ns("direction"), "Direction :", 
                                                          choices = c(
                                                            "up-down" = "UD",
                                                            "down-up" = "DU",
                                                            "left-right" = "LR",
                                                            "right-left" = "RL"),
                                                          selected = "UD")),
                                     
                                     
                                     column(2,br(),awesomeCheckbox(ns("fallenLeaves"), "Fallen leaves")),
                                     column(2,br(),awesomeCheckbox(ns("updateShape"), "Update shape",value = TRUE)),
                                     column(12),
                                     column(2,br(),materialSwitch(ns("nodesPopSize"), "Nodes size use population",status = "info")),
                                     conditionalPanel(paste0("input['", ns("nodesPopSize"),"'] == true"),
                                                      column(2,numericInput(ns("minNodeSize"), "Min nodes size",
                                                                            value = 15, min = 1)),                          
                                                      column(2,numericInput(ns("maxNodeSize"), "Max nodes size",
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
                                     column(2,numericInput(ns("tooltipDelay"), "Tooltip delay :",
                                                           value = 500, min = 0, step = 100)),
                                     column(2,br(),materialSwitch(ns("rules"),"Display rules", value = TRUE, status = "info")),
                                     conditionalPanel(paste0("input['",ns("rules"),"'] == true"),
                                                      column(2,br(),awesomeCheckbox(ns("simpRules"),"Simplify rules", value = TRUE))))),
                          ##End Tooltips##
                          tabPanel("Legend params",
                                   fluidRow(
                                     ##Legend##
                                     # column(12, div(class = "hrdivide")),
                                     # column(12, h3(class="treetitle","Legend")),
                                     column(1),
                                     column(2, br(),materialSwitch(ns("legend"),"Display legend",
                                                                   value = TRUE, status = "info")),
                                     conditionalPanel(paste0("input['",ns("legend"),"'] == true"),
                                                      column(2,numericInput(ns("legendWidth"),
                                                                            "Legend width",
                                                                            value = 0.1, 
                                                                            min = 0,
                                                                            max = 1,
                                                                            step = 0.01)),
                                                      column(2,numericInput(ns("legendNcol"),
                                                                            "Legend number of column",
                                                                            value = 1, 
                                                                            min = 1,
                                                                            max = 50,
                                                                            step = 1)),
                                                      column(2,numericInput(ns("legendNodesSize"), "Legend nodes size :",
                                                                            value = 22, min = 1)),
                                                      column(2,numericInput(ns("legendFontSize"), "Legend font size :",
                                                                            value = 16, min = 1))
                                                      
                                                      
                                     )))
                          ##End Legend##
                          ,
                          div(actionButton(ns("runTree"),
                                           "Run tree", class = "btnruntree"),align = "center")))
                    
                  ),
                  br(),
                  
                  # Show a plot of the generated distribution
                  fluidPage(
                    # uiOutput("treeUI"),
                    column(1, dropdownButton(icon = "Node",status = "danger",width = 400,
                                             fluidRow(
                                               ##Nodes##
                                               # column(12, div(class = "hrdivide")),
                                               # column(12, h3(class="treetitle","Nodes")),
                                               
                                               column(6,selectInput(ns("shapeY"), "shape Y :", 
                                                                    choices = c("diamond",
                                                                                "dot",
                                                                                "star",
                                                                                "triangle",
                                                                                "triangleDown",
                                                                                "square"),
                                                                    selected = "square"
                                               )),
                                               column(6,selectInput(ns("shapeX"), "shape X :", 
                                                                    choices = c("diamond",
                                                                                "dot",
                                                                                "star",
                                                                                "triangle",
                                                                                "triangleDown",
                                                                                "square"),
                                                                    selected = "dot"
                                               )),
                                               column(6,numericInput(ns("nodesFontSize"), "Nodes font size :",
                                                                     value = 16, min = 1)),
                                               
                                               
                                               column(12,br(), materialSwitch(ns("usecolornodes"),"Color nodes", status = "info")),
                                               uiOutput(ns("colornodes")),
                                               
                                               column(12,br(), materialSwitch(ns("usecolorY"),"Color Y", status = "info")),
                                               
                                               uiOutput(ns("colorY")))
                    )),
                    
                    column(1,
                           dropdownButton(icon = "Edge",status = "warning",width = 200,
                                          
                                          fluidRow(
                                            column(12,colourInput(ns("colorEdges"), "Color edges :",
                                                                  value = "#8181F7")),
                                            column(12,numericInput(ns("edgesFontSize"), "Edges font size :",
                                                                   value = 14, min = 1)),
                                            column(12,selectInput(ns("edgesFontAlign"),"Edges font align",choices = c(
                                              "horizontal", "top", "middle", "bottom"
                                            )))))),
                    
                    column(1, dropdownButton(icon = "Title",status = "info",width = 400,
                                             fluidRow( column(4,textInput(ns("main"),
                                                                          "Main :", "")),
                                                       column(4,
                                                              colourpicker::colourInput(ns("colourMain"),"Main color :", value = "black")),
                                                       column(4,numericInput(ns("mainsize"), "Main size :",
                                                                             value = 20, min = 1)),
                                                       
                                                       column(4,textInput(ns("submain"),
                                                                          "Subtitle :", "")),
                                                       column(4,colourpicker::colourInput(ns("colourSubMain"),"subtitle color :", value = "black")),
                                             column(4,numericInput(ns("Submainsize"), "subtitle size :",
                                                                   value = 12, min = 1)),
                                             column(4,textInput(ns("footer"),
                                                                "Footer :", "")),
                                             column(4,colourpicker::colourInput(ns("colourFooterMain"),"subtitle color :", value = "black")),
                                             column(4,numericInput(ns("Footermainsize"), "subtitle size :",
                                                                   value = 12, min = 1)))
                    )),
                    column(1, dropdownButton(icon = "Other",status = "success",width = 400,
                                             column(6,
                                             materialSwitch(ns("highlightNearest"),"highlight nearest", status = "info", value = TRUE)  ),
                                             column(6,
                                                    materialSwitch(ns("highlightHover"),"highlight hover", status = "info", value = TRUE)  ),  
                                             column(6,
                                                    materialSwitch(ns("collapse"),"Collapse", status = "info", value = TRUE)  )
                    )),
                  column(12),
                  
                  
                  
                  uiOutput(ns("treeUI")),
                  downloadLink(ns('downloadNetwork'), 'Download Tree')
                )
    )
    
  )
  )
# )
}
