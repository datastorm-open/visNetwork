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
library(ggplot2)
library(pipeR)
library(shinydashboard)
library(colourpicker)
source("function.R")

dta <- diamonds

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  
  observe({
    input$y
    input$x
    input$complexity
    input$minsplit
    updateCheckboxInput(session,"usecolornodes", value = FALSE )
    updateCheckboxInput(session,"usecolorY", value = FALSE )
    
  })
  
  rPart <- reactive({
    input$runTree
    req(input$runTree > 0)
    isolate({
      formul <- paste(input$y, "~", paste0(input$x, collapse = "+"))%>>%as.formula()
      rpart(formul, data=diamonds , control = rpart.control(cp = input$complexity,
                                                            minsplit = input$minsplit))
    })
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
              main = input$main,
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
  
  observe({
    updateSelectInput(session, inputId = "x", choices = names(dta)[names(dta)!=input$y],
                      selected = names(dta)[names(dta)!=input$y])
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
        as.character(column(6,colourpicker::colourInput(gsub(" ", "", paste0(x[1],"Y")), paste0(x[1]," :"), x[2])))
      })%>>%
        paste0(collapse = "")%>>%
        HTML()
      }else{
        res <- list()
        res[[1]] <- as.character(column(6,
                                        colourpicker::colourInput("minY", "Min y : ", corY[1])))
        res[[2]] <- as.character(column(6,
                                        colourpicker::colourInput("maxY", "Max y : ", corY[2])))
        res <- paste0(res, collapse = "")%>>%
          HTML()
      }
    }
    res
  })
  
  observe({
    m <- treeBuild()
    

    visNetworkProxy("tree") %>%
      visEdges(font = list(size = input$edgesFontSize,
                           align = input$edgesFontAlign)) %>%
      visNodes(font = list(size = input$nodesFontSize))
  })
  
  
  
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
        
        visNetworkProxy("tree") %>%
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
      visNetworkProxy("tree") %>%
        visUpdateNodes(Gcol)%>%
        visUpdateNodes(Lcol, legend = TRUE)
      }
      
      
  })
  
  observe({
    m <- treeBuild()
    idEdges <- m$x$edges$id
    
    newEdges <- data.frame(id = m$x$edges$id, color = input$colorEdges)
    visNetworkProxy("tree") %>%
      visUpdateEdges(newEdges)
  })
  
  
  ##Update shape Ynodes
  observe({
    m <- treeBuild()
  ret <- data.frame(id = m$x$nodes$id,
                    shape = ifelse(m$x$nodes$Leaf == 0, input$shapeX,input$shapeY ))
  visNetworkProxy("tree") %>%
    visUpdateNodes(ret)
  })
  
  
  output$treeUI <- renderUI({
    res <- rPart()
    isolate({
      heigth <- ifelse(is.null(input$heigth), 650, input$heigth)
    visNetworkOutput("tree", height = paste0(heigth, "px"))
    })
  })

  output$downloadNetwork <- downloadHandler(
             filename = function() {
                   paste('tree-', Sys.Date(), '.html', sep='')
               },
             content = function(con) {
              
                   out <- treeBuild()
                     out %>% visExport() %>% visSave(con)
                    
               }
         )

  
  output$colornodes <- renderUI({
    if(!input$usecolornodes){
      return(NULL)
    }else{
      corNodes <- updateColorVar()
     res <- apply(corNodes, 1, function(x){
        as.character(column(6,colourpicker::colourInput(gsub(" ", "", paste0(x[1],"var")), paste0(x[1]," :"), x[2])))
      })%>>%
       paste0(collapse = "")%>>%
       HTML()
    }
    res
  })
  
  
})
