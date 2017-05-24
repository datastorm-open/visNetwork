library(shiny)
library(visNetwork)
library(rpart)
library(colourpicker)
library(shinyWidgets)

#' Generate server of shiny module from data.frame reactive
#'
#' @param  input  \code{list} shiny input
#' @param  output \code{list}, shiny output
#' @param  session  \code{list}, shiny session
#' @param data \code{reactive}, reactive of a data.frame
#' 
#' @seealso \link{visTreeModuleUI}
#' 
#' @export
treeServ <- function(input, output, session, data) {
  
  ns <- session$ns
  
  data_network <- shiny::reactive({data()})
  
  # Get reactive rpart
  rPart <- shiny::reactive({
    input$runTree
    input$downloadNetwork
    shiny::req(input$runTree > 0)
    shiny::isolate({
      formule <- paste(input$y, "~", paste0(input$x, collapse = "+")) %>% as.formula()
      rpart(formule, data = data_network(), 
            control = rpart.control(cp = input$complexity, minsplit = input$minsplit))
    })
  })
  
  shiny::observe({
    shiny::updateSelectInput(session, inputId = "y", choices = names(data_network()))
  })
  
  shiny::observe({
    shiny::updateSelectInput(session, inputId = "x", choices = names(data_network())[names(data_network())!=input$y],
                             selected = names(data_network())[names(data_network())!=input$y])
  })
  
  # Get all default param if tabs of params are display
  paramTop <- shiny::reactive({
    heigth <- input$heigth
    if(is.null(heigth)){
      heigth <- 650
    }
    
    digits <- input$digits
    if(is.null(input$digits)){
      digits <- 3
    }
    
    fallenLeaves <- input$fallenLeaves
    if(is.null(fallenLeaves)){
      fallenLeaves <- FALSE
    }
    
    updateShape <- input$updateShape
    if(is.null(updateShape)){
      updateShape <- TRUE
    }
    
    tooltipDelay <- input$tooltipDelay
    if(is.null(tooltipDelay)){
      tooltipDelay <- 500
    }
    
    rules <- input$rules
    if(is.null(rules)){
      rules <- TRUE
    }
    
    legend <- input$legend
    
    if(is.null(legend)){
      legend <- TRUE
    }
    nodesPopSize <- input$nodesPopSize
    
    if(is.null(nodesPopSize)){
      nodesPopSize <- FALSE
    }
    
    export <- input$export
    if(is.null(export)){
      export <- TRUE
    }
    list(heigth = heigth, digits = digits,
         fallenLeaves = fallenLeaves, tooltipDelay = tooltipDelay,
         rules = rules, legend = legend, nodesPopSize = nodesPopSize,
         updateShape = updateShape, export = export)
  })
  
  # Run shiny module on rpart
  shiny::observe({
    treeServLigth(input = input, output = output, session = session, data = rPart,
                  heigth = shiny::reactive(paramTop()$heigth),
                  digits = shiny::reactive(paramTop()$digits),
                  fallenLeaves = shiny::reactive(paramTop()$fallenLeaves),
                  tooltipDelay = shiny::reactive(paramTop()$tooltipDelay),
                  rules = shiny::reactive(paramTop()$rules),
                  legend = shiny::reactive(paramTop()$legend),
                  nodesPopSize = shiny::reactive(paramTop()$nodesPopSize),
                  updateShape = shiny::reactive(paramTop()$updateShape),
                  export = shiny::reactive(paramTop()$export))
  })
}



#' Generate server of shiny module from rpart reactive
#'
#' @param  input  \code{list} shiny input
#' @param  output \code{list}, shiny output
#' @param  session  \code{list}, shiny session
#' @param data \code{reactive}, reactive of a rpart
#' 
#' @seealso \link{visTreeModuleUI}
#' 
#' @export
treeServLigth <- function(input, output, session, data,
                          heigth = shiny::reactive(650),
                          digits = shiny::reactive(3),
                          fallenLeaves = shiny::reactive(FALSE),
                          tooltipDelay = shiny::reactive(500),
                          rules = shiny::reactive(TRUE),
                          legend = shiny::reactive(TRUE),
                          nodesPopSize = shiny::reactive(FALSE),
                          updateShape = shiny::reactive(TRUE),
                          export = shiny::reactive(FALSE)) {
  
  ns <- session$ns
  
  # Get rpart from reactive data
  rPart <- shiny::reactive({
    input$runTree
    input$downloadNetwork
    data()
  })
  
  
  shiny::observe({
    input$y
    input$x
    input$complexity
    input$minsplit
    shiny::updateCheckboxInput(session,"usecolornodes", value = FALSE )
    shiny::updateCheckboxInput(session,"usecolorY", value = FALSE )
  })
  
  # Get color
  updateColorVar <- shiny::reactive({
    colorVar <- colorVarData()
    dest <- gsub(" ", "", paste0(sortLabels(), "var"))
    if(!is.null(input[[dest[1]]])){
      colorVect <- sapply(dest, function(X)input[[X]])
      colorVar$color <- colorVect
    }
    colorVar
  })
  
  updateColorY<- shiny::reactive({
    colorY <- colorY()
    clas <- attributes(rPart())$ylevels
    if(!is.null(clas)){
      dest <- paste0(gsub(" ", "", paste0(colorY$modality, "Y")))
      if(!is.null(input[[dest[1]]])){
        colorVect <- sapply(dest, function(X)input[[X]])
        colorY$color <- colorVect
      }
    }else{
      if(!is.null(input$minY)){
        colorY[1] <- input$minY
        colorY[2] <- input$maxY
      }
    }
    colorY
  })
  
  # Build tree (visTree)
  treeBuild <- shiny::reactive({
    input$downloadNetwork
    res <- rPart()
    shiny::isolate({
      
      colorVar <- updateColorVar()
      colorY <- updateColorY()
      
      simpRules <- input$simpRules
      simpRules <- ifelse(is.null(simpRules),FALSE,simpRules)
      
      legendWidth <- input$legendWidth
      legendWidth <- ifelse(is.null(legendWidth), 0.1, legendWidth)
      
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
      
      direction <- input$direction
      direction <- ifelse(is.null(direction), "UD",direction)
      
      legend <- legend()
      rules <- rules()
      tooltipDelay <- tooltipDelay()
      updateShape <- updateShape()
      fallenLeaves <- fallenLeaves()
      digits <- digits()
      heigth <- heigth()
      nodesPopSize <- nodesPopSize()
      export <- export()
      if("runTree" %in% names(input)){
        if(input$runTree > 0){
          legend <- input$legend
          rules <- input$rules
          tooltipDelay <- input$tooltipDelay
          updateShape <- input$updateShape
          fallenLeaves <- input$fallenLeaves
          digits <- input$digits
          heigth <- input$heigth
          nodesPopSize <- input$nodesPopSize
          export <- input$export
        }
      }
      
      visTree(res,
              main = getMain(),
              submain = getSubMain(),
              footer = getFooter(),
              shapeY = input$shapeY,
              shapeVar = input$shapeX,
              colorVar = colorVar,
              colorY = colorY,
              rules = rules,
              simplifyRules = simpRules,
              legend = legend,
              legendWidth = legendWidth,
              legendNodesSize = legendNodesSize,
              legendFontSize = legendFontSize,
              legendNcol = legendNcol,
              edgesFontSize = input$edgesFontSize,
              edgesFontAlign = input$edgesFontAlign,
              nodesFontSize = input$nodesFontSize,
              nodesPopSize = nodesPopSize,
              minNodeSize = minNodeSize,
              maxNodeSize = maxNodeSize,
              tooltipDelay = tooltipDelay,
              digits = digits,
              direction = direction,
              fallenLeaves = fallenLeaves,
              updateShape = updateShape,
              colorEdges = input$colorEdges,
              height = paste0(heigth, "px"),
              export = export)
    })
  })
  
  output$tree <- renderVisNetwork({
    treeBuild()
  })
  
  # Give names of variables
  varNodes <- shiny::reactive({
    res <- rPart()
    var <- levels(res$frame$var)[levels(res$frame$var)!="<leaf>"]
    var
  })
  
  sortLabels <- shiny::reactive({
    sort(unique(varNodes()))
  })
  
  colorVarData <- shiny::reactive({
    color <- visNetwork:::.generateVarColor(nodes_var = varNodes(), SortLabel = sortLabels())
    colNod <- color[match(varNodes(),  sortLabels())]
    data.frame(variable = varNodes(), color = colNod)
  })
  
  
  # Color for Y
  colorY <- shiny::reactive({
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
  
  
  # UI for color
  output$colorY <- shiny::renderUI({
    if(!input$usecolorY){
      return(NULL)
    }else{
      corY <- updateColorY()
      clas <- attributes(rPart())$ylevels
      if(!is.null(clas)){
        res <- apply(corY, 1, function(x){
          as.character(shiny::column(6,colourpicker::colourInput(ns(gsub(" ", "", paste0(x[1],"Y"))), paste0(x[1]," :"), x[2])))
        })%>%
          paste0(collapse = "")%>%
          HTML()
      }else{
        res <- list()
        res[[1]] <- as.character(shiny::column(6,
                                               colourpicker::colourInput(ns("minY"), "Min y : ", corY[1])))
        res[[2]] <- as.character(shiny::column(6,
                                               colourpicker::colourInput(ns("maxY"), "Max y : ", corY[2])))
        res <- paste0(res, collapse = "")%>%
          HTML()
      }
    }
    res
  })
  
  
  # Update color VarNodes
  shiny::observe({
    m <- treeBuild()
    oldId <- m$x$nodes$id
    
    legOldId <- m$x$legend$nodes$id
    colorIn <- m$x$nodes$color
    legColorIn <- m$x$legend$nodes$color
    outColor <- unlist(sapply(m$x$nodes$label, function(x){
      if(is.null(input[[gsub(" ", "", paste0(x[1],"var"))]])){
        "NoChange"
      }else{
        input[[gsub(" ", "", paste0(x[1],"var"))]]
      }
    }))
    
    legOutColor <- unlist(sapply(m$x$legend$nodes$label, function(x){
      if(is.null(input[[gsub(" ", "", paste0(x[1], "var"))]])){
        "NoChange"
      }else{
        input[[gsub(" ", "", paste0(x[1], "var"))]]
      }
    }))
    
    newColor <- data.frame(
      id = oldId[outColor != "NoChange" & ifelse(outColor == colorIn, FALSE, TRUE)],
      color = outColor[outColor != "NoChange" & ifelse(outColor == colorIn, FALSE, TRUE)]
    )
    
    legNewColor <- data.frame(
      id = legOldId[legOutColor != "NoChange" & ifelse(legOutColor == legColorIn, FALSE, TRUE)],
      color = legOutColor[legOutColor != "NoChange" & ifelse(legOutColor == legColorIn, FALSE, TRUE)]
    )
    
    clas <- attributes(rPart())$ylevels
    
    if(!is.null(input$minY) & is.null(clas))
    {
      coloramp <- visNetwork:::.creatColorRampY(c(input$minY, input$maxY))
      if(nrow(m$x$nodes) > 1){
        meanV <- (m$x$nodes$labelClust-min(m$x$nodes$labelClust))/(max(m$x$nodes$labelClust-min(m$x$nodes$labelClust)))
      } else {
        meanV <- 1
      }
      colorTerm <- grDevices::rgb(coloramp(meanV), maxColorValue=255)
      endf <- data.frame(id = m$x$nodes$id, colorClust = colorTerm, color = "")
      endf$color <- as.character(endf$color)
      endf$color[m$x$nodes$Leaf == 1] <- as.character(colorTerm[m$x$nodes$Leaf == 1])
      endf$color[m$x$nodes$Leaf == 0] <- as.character(m$x$nodes$color[m$x$nodes$Leaf == 0])
      
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(endf)
    }
    if(!is.null(input[[gsub(" ", "", paste0(m$x$nodes[1,]$label,"var"))]])){
      oldId <- m$x$nodes$id
      legId <- m$x$legend$nodes$id
      colorIn <- m$x$nodes$color
      colorInLeg <- as.character(m$x$legend$nodes$color)
      outColor <- unlist(sapply(m$x$nodes$label, function(x){
        if(is.null(input[[gsub(" ", "", paste0(x[1],"Y"))]])){
          "NoChange"
        }else{
          input[[gsub(" ", "", paste0(x[1],"Y"))]]
        }
      }))
      
      outColorLeg <- unlist(sapply(m$x$legend$nodes$label, function(x){
        if(is.null(input[[gsub(" ", "", paste0(x[1],"Y"))]])){
          "NoChange"
        }else{
          input[[gsub(" ", "", paste0(x[1],"Y"))]]
        }
      }))
      
      newColorGraph <- data.frame(
        id = oldId[outColor != "NoChange" & ifelse(outColor == colorIn, FALSE, TRUE)],
        color = outColor[outColor != "NoChange" & ifelse(outColor == colorIn, FALSE, TRUE)]
      )
      
      newColorLegend <- data.frame(
        id = legId[outColorLeg != "NoChange" & ifelse(outColorLeg == colorInLeg, FALSE, TRUE)],
        color = outColorLeg[outColorLeg != "NoChange" & ifelse(outColorLeg == colorInLeg, FALSE, TRUE)]
      )
    }
    
    if(exists("newColor") & exists("newColorGraph")){
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
    
    if(exists("legNewColor") & exists("newColorLegend")){
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
    
    if(exists("Gcol")){
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(Gcol) %>%
        visUpdateNodes(Lcol, legend = TRUE)
    }
  })
  
  # Update direction
  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visHierarchicalLayout(direction = input$direction)
  })
  
  # Update node size
  shiny::observe({
    m <- treeBuild()
    if(input$nodesPopSize){
      newNodes <- data.frame(id = m$x$nodes$id, scaling.min = input$minNodeSize, scaling.max = input$maxNodeSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(newNodes)
    }
  })
  
  # Update node size
  shiny::observe({
    m <- treeBuild()
    if(!input$nodesPopSize){
      newNodes <- data.frame(id = m$x$nodes$id, scaling.min = input$nodesSize, scaling.max = input$nodesSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(newNodes)
    }
  })
  
  # Update edges color
  shiny::observe({
    m <- treeBuild()
    if(!is.null(m$x$edges)){
      idEdges <- m$x$edges$id
      newEdges <- data.frame(id = m$x$edges$id, color = input$colorEdges)
      visNetworkProxy(ns("tree")) %>%
        visUpdateEdges(newEdges)
    }
  })
  
  # Update edges font size
  shiny::observe({
    m <- treeBuild()
    if(!is.null(m$x$edges)){
      idEdges <- m$x$edges$id
      newEdges <- data.frame(id = m$x$edges$id, font.size = input$edgesFontSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateEdges(newEdges)
    }
  })
  
  # Update edges font align
  shiny::observe({
    m <- treeBuild()
    if(!is.null(m$x$edges)){
      idEdges <- m$x$edges$id
      newEdges <- data.frame(id = m$x$edges$id, font.align = input$edgesFontAlign)
      visNetworkProxy(ns("tree")) %>%
        visUpdateEdges(newEdges)
    }
  })
  
  # Update nodes font size
  shiny::observe({
    m <- treeBuild()
    newNodes <- data.frame(id = m$x$nodes$id, font.size = input$nodesFontSize)
    visNetworkProxy(ns("tree")) %>%
      visUpdateNodes(newNodes)
  })
  
  # reactive for title
  getMain <- shiny::reactive({
    text <- input$main
    style <- paste0("font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:",input$mainsize,"px;text-align:center;color:", input$colourMain, ";")
    list(text = text, style = style)
  })
  
  # Update title
  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visSetTitle(main = getMain())
  })
  
  
  getSubMain <- shiny::reactive({
    text <- input$submain
    style <- paste0("font-family:Georgia, Times New Roman, Times, serif;font-size:",input$Submainsize,"px;text-align:center;color:", input$colourSubMain, ";")
    list(text = text, style = style)
  })
  
  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visSetTitle(submain =  getSubMain())
  })
  
  getFooter <- shiny::reactive({
    text <- input$footer
    style <- paste0("font-family:Georgia, Times New Roman, Times, serif;font-size:",input$Footermainsize,"px;text-align:center;color:", input$colourFooterMain, ";")
    list(text = text, style = style)
  })
  
  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visSetTitle(footer = getFooter())
  })
  
  # Update shape Ynodes
  shiny::observe({
    m <- treeBuild()
    ret <- data.frame(id = m$x$nodes$id, shape = ifelse(m$x$nodes$Leaf == 0, input$shapeX,input$shapeY))
    visNetworkProxy(ns("tree")) %>%
      visUpdateNodes(ret)
  })
  
  # Update highlightNearest
  highlightNearest <- shiny::reactive({
    list(enabled = input$highlightNearest, degree = list(from = 50000, to = 0),
         hover = input$highlightHover, algorithm = "hierarchical")
    
  })
  
  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visOptions(highlightNearest = highlightNearest())
  })
  
  # Update collapse
  collapse <- shiny::reactive({
    list(enabled = input$collapse,fit = TRUE, resetHighlight = TRUE, clusterOptions = list(fixed = TRUE, physics= FALSE))
  })
  
  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visOptions(collapse = collapse())
  })
  
  # Update heigth
  output$treeUI <- shiny::renderUI({
    res <- rPart()
    shiny::isolate({
      heigth <- ifelse(is.null(input$heigth), 650, input$heigth)
      visNetworkOutput(ns("tree"), height = paste0(heigth, "px"))
    })
  })
  
  # download tree
  output$downloadNetwork <- shiny::downloadHandler(
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
                     rules = rules(),
                     simplifyRules = simpRules,
                     legend = legend(),
                     legendWidth = legendWidth,
                     legendNodesSize = legendNodesSize,
                     legendFontSize = legendFontSize,
                     legendNcol = legendNcol,
                     edgesFontSize = input$edgesFontSize,
                     edgesFontAlign = input$edgesFontAlign,
                     nodesFontSize = input$nodesFontSize,
                     nodesPopSize = nodesPopSize(),
                     minNodeSize = minNodeSize,
                     maxNodeSize = maxNodeSize,
                     tooltipDelay = tooltipDelay(),
                     digits = digits(),
                     direction = direction(),
                     fallenLeaves = fallenLeaves(),
                     updateShape = updateShape(),
                     colorEdges = input$colorEdges,
                     height = paste0(900, "px"),
                     export = export())
      
      out %>% visExport() %>% visSave(con)
      
    }
  )
  
  output$colornodes <- shiny::renderUI({
    if(!input$usecolornodes){
      return(NULL)
    }else{
      corNodes <- updateColorVar()
      if(nrow(corNodes) > 0){
        res <- apply(corNodes, 1, function(x){
          as.character(shiny::column(6,colourpicker::colourInput(ns(gsub(" ", "", paste0(x[1],"var"))), paste0(x[1]," :"), x[2])))
        }) %>% paste0(collapse = "") %>% htmltools::HTML()
      }
    }
  })
}


#' Generate ui for visTree shiny module. 
#'
#' @param  id \code{character} id of module, refear to id in \link{treeServLigth} or \link{treeServ}
#' @param  rpartParam \code{boolean}, add tab of rpartParam
#' @param  generalParam \code{boolean}, add tabs of generalParam
#' 
#' @return \code{html} html of shiny module
#' 
#' @examples
#' \dontrun{
#' data <- iris
#' shiny::shinyApp(ui = shiny::fluidPage(visTreeModuleUI(id = "id1",
#'  rpartParam = FALSE,
#'  generalParam = FALSE)), 
#'  server = function(input, output, session) {
#'  shiny::callModule(treeServLigth, "id1", data = shiny::reactive(rpart(data)))
#' })
#' 
#' shiny::shinyApp(ui = shiny::fluidPage(visTreeModuleUI(id = "id1",
#'  rpartParam = TRUE,
#'  generalParam = FALSE)), 
#'  server = function(input, output, session) {
#'  shiny::callModule(treeServ, "id1", data = shiny::reactive(data))
#' })
#' 
#' 
#' shiny::shinyApp(ui = shiny::fluidPage(visTreeModuleUI(id = "id1",
#'  rpartParam = TRUE,
#'  generalParam = TRUE)), 
#'  server = function(input, output, session) {
#'  shiny::callModule(treeServ, "id1", data = shiny::reactive(data))
#' })
#' 
#' shiny::shinyApp(ui = 
#' navbarPage("Menu",shiny::tabPanel(
#'   "tt1",shiny::fluidPage(visTreeModuleUI(id = "id1", 
#'   rpartParam = TRUE,
#'   generalParam = TRUE))
#' ),
#' shiny::tabPanel(
#'   "tt2",shiny::fluidPage(visTreeModuleUI(id = "id2", 
#'   rpartParam = FALSE,
#'   generalParam = FALSE)))
#' ), 
#' server = function(input, output, session) {
#'   shiny::callModule(treeServ, "id1", data = shiny::reactive(iris))
#'   shiny::callModule(treeServLigth, "id2", data = shiny::reactive(rpart(iris)))
#' })
#' }
#' 
#' @export
visTreeModuleUI <- function(id, rpartParam = TRUE, generalParam = TRUE) {
  ns <- shiny::NS(id)
  selectedTabs <- ifelse(rpartParam, "rpart", "visTree options")
  
  shiny::fluidPage(
    if(generalParam | rpartParam){
      shiny::tabsetPanel(
        id = ns('boxparam'),
        selected = selectedTabs ,
        if(rpartParam){
          shiny::tabPanel("rpart",
                          #Params for rparts
                          shiny::fluidRow(
                            shiny::div(align = "center",
                                       shiny::column(2, 
                                                     shiny::selectInput(ns("y"), "Y :",NULL)
                                       ),
                                       shiny::column(6,
                                                     shiny::selectInput(ns("x"), "X :", NULL, multiple = TRUE, selected = NULL, width = "100%")
                                       ),
                                       shiny::column(2,
                                                     shiny::numericInput(ns("complexity"), "Complexity :", value = 0.005, step = 0.001)
                                       ),
                                       shiny::column(2,
                                                     shiny::numericInput(ns("minsplit"), "Minsplit", value = 20, min = 2)
                                       )
                                       
                            )
                          )
          )
        }else{""},
        
        if(generalParam){
          shiny::tabPanel("visTree options",
                          #Params graph
                          shiny::fluidRow(
                            shiny::column(1, 
                                          shiny::numericInput(ns("heigth"), "Height :",650)
                            ),
                            shiny::column(1,
                                          shiny::numericInput(ns("digits"), "Digits : ", value = 3, min = 0)
                            ),
                            shiny::column(1, 
                                          shiny::numericInput(ns("tooltipDelay"), "Tooltip delay :", value = 500, min = 0, step = 100)
                            ),
                            shiny::column(2,
                                          shiny::br(), shinyWidgets::awesomeCheckbox(ns("fallenLeaves"), "Fallen leaves")
                            ),
                            shiny::column(2,
                                          shiny::br(), shinyWidgets::awesomeCheckbox(ns("updateShape"), "Update shape",value = TRUE)
                            ),
                            shiny::column(2,
                                          shiny::br(), shinyWidgets::awesomeCheckbox(ns("rules"),"Display rules", value = TRUE)
                            ),
                            shiny::conditionalPanel(paste0("input['",ns("rules"),"'] == true"),
                                                    shiny::column(2,
                                                                  shiny::br(), shinyWidgets::awesomeCheckbox(ns("simpRules"),"Simplify rules", value = TRUE))
                            ),
                            shiny::column(1,
                                          shiny::br(), shinyWidgets::awesomeCheckbox(ns("export"), "Export", value = TRUE)
                            )
                          )
          )
        }else{""},

        if(generalParam){
          shiny::tabPanel("Legend",
                          shiny::fluidRow(
                            shiny::column(2, offset = 1, 
                                          shiny::br(),
                                          shinyWidgets::materialSwitch(ns("legend"),"Display legend", value = TRUE, status = "info")
                            ),
                            shiny::conditionalPanel(paste0("input['",ns("legend"),"'] == true"),
                                                    shiny::column(2,
                                                                  shiny::numericInput(ns("legendWidth"), "Width :",
                                                                                      value = 0.1, min = 0, max = 1, step = 0.01)
                                                    ),
                                                    shiny::column(2,
                                                                  shiny::numericInput(ns("legendNcol"), "Number of columns :",
                                                                                      value = 1, min = 1, max = 50, step = 1)
                                                    ),
                                                    shiny::column(2,
                                                                  shiny::numericInput(ns("legendNodesSize"), "Nodes size :",
                                                                                      value = 22, min = 1)
                                                    ),
                                                    shiny::column(2,
                                                                  shiny::numericInput(ns("legendFontSize"), "Font size :",
                                                                                      value = 16, min = 1)
                                                    )
                                                    
                                                    
                            )
                          )
          )
        }else{""}
        
      )
    },
    
    if(generalParam | rpartParam){
      shiny::div(
        shiny::actionButton(ns("runTree"), "Run tree", class = "btnruntree"), align = "center"
      )
    }else{""},
    
    shiny::br(),
    
    # Show a plot of the generated distribution
    # shiny::uiOutput("treeUI"),
    shiny::fluidRow(
      shiny::column(1, 
                    shinyWidgets::dropdownButton(icon = "Node",status = "danger",width = 400,
                                                 shiny::fluidRow(
                                                   shiny::column(6,
                                                                 shiny::selectInput(ns("shapeY"), "shape Y :", 
                                                                                    choices = c("diamond",
                                                                                                "dot",
                                                                                                "star",
                                                                                                "triangle",
                                                                                                "triangleDown",
                                                                                                "square",
                                                                                                "ellipse", 
                                                                                                "circle", 
                                                                                                "database", 
                                                                                                "box", 
                                                                                                "text"),
                                                                                    selected = "square"),
                                                                 shiny::numericInput(ns("nodesFontSize"), "Nodes font size :",
                                                                                     value = 16, min = 1)
                                                   ),
                                                   shiny::column(6,
                                                                 shiny::selectInput(ns("shapeX"), "shape X :", 
                                                                                    choices = c("diamond",
                                                                                                "dot",
                                                                                                "star",
                                                                                                "triangle",
                                                                                                "triangleDown",
                                                                                                "square",
                                                                                                "ellipse", 
                                                                                                "circle", 
                                                                                                "database", 
                                                                                                "box", 
                                                                                                "text"),
                                                                                    selected = "dot"),
                                                                 shiny::numericInput(ns("nodesSize"), "Nodes size :",
                                                                                     value = 22, min = 1)),
                                                   shiny::column(12,
                                                                 shiny::br(),
                                                                 shinyWidgets::materialSwitch(ns("nodesPopSize"), 
                                                                                              "Nodes size use population", status = "info")
                                                   ),
                                                   shiny::column(12,             
                                                                 shiny::conditionalPanel(paste0("input['", ns("nodesPopSize"),"'] == true"),
                                                                                         shiny::column(6,
                                                                                                       shiny::numericInput(ns("minNodeSize"), "Min nodes size",
                                                                                                                           value = 15, min = 1)
                                                                                         ),                          
                                                                                         shiny::column(6,
                                                                                                       shiny::numericInput(ns("maxNodeSize"), "Max nodes size",
                                                                                                                           value = 30, min = 1))
                                                                 )
                                                                 
                                                   ),
                                                   shiny::column(12,
                                                                 shiny::br(), 
                                                                 shinyWidgets::materialSwitch(ns("usecolornodes"),"Color nodes", status = "info")
                                                   ),
                                                   shiny::uiOutput(ns("colornodes")),
                                                   
                                                   shiny::column(12, 
                                                                 shiny::br(), 
                                                                 shinyWidgets::materialSwitch(ns("usecolorY"),"Color Y", status = "info")
                                                   ),
                                                   shiny::uiOutput(ns("colorY")))
                    )
      ),
      
      shiny::column(1,
                    shinyWidgets::dropdownButton(icon = "Edge", status = "warning", width = 300,
                                                 shiny::fluidRow(
                                                   shiny::column(12,
                                                                 colourpicker::colourInput(ns("colorEdges"), "Color edges :",
                                                                                           value = "#8181F7")
                                                   ),
                                                   shiny::column(12,
                                                                 shiny::numericInput(ns("edgesFontSize"), "Edges font size :",
                                                                                     value = 14, min = 1)
                                                   ),
                                                   shiny::column(12,
                                                                 shiny::selectInput(ns("edgesFontAlign"),"Edges font align",choices = c(
                                                                   "horizontal", "top", "middle", "bottom"))
                                                   )
                                                 )
                    )
      ),
      
      shiny::column(1, 
                    shinyWidgets::dropdownButton(icon = "Title",status = "info", width = 400,
                                                 shiny::fluidRow(
                                                   shiny::column(4,
                                                                 textInput(ns("main"), "Main :", "")
                                                   ),
                                                   shiny::column(4,
                                                                 colourpicker::colourInput(ns("colourMain"), 
                                                                                           "Main color :", value = "black")
                                                   ),
                                                   shiny::column(4,
                                                                 shiny::numericInput(ns("mainsize"), "Main size :",
                                                                                     value = 20, min = 1)
                                                   ),
                                                   shiny::column(4,
                                                                 textInput(ns("submain"), "Subtitle :", "")
                                                   ),
                                                   shiny::column(4,
                                                                 colourpicker::colourInput(ns("colourSubMain"),
                                                                                           "subtitle color :", value = "black")
                                                   ),
                                                   shiny::column(4,
                                                                 shiny::numericInput(ns("Submainsize"), "subtitle size :",
                                                                                     value = 12, min = 1)
                                                   ),
                                                   shiny::column(4,
                                                                 textInput(ns("footer"), "Footer :", "")
                                                   ),
                                                   shiny::column(4,
                                                                 colourpicker::colourInput(ns("colourFooterMain"),
                                                                                           "subtitle color :", value = "black")
                                                   ),
                                                   shiny::column(4,
                                                                 shiny::numericInput(ns("Footermainsize"), "subtitle size :",
                                                                                     value = 12, min = 1))
                                                 )
                    )
      ),
      shiny::column(1, 
                    shinyWidgets::dropdownButton(icon = "Other",status = "success",width = 300,
                                                 shiny:: fluidRow(
                                                   shiny::column(12,
                                                                 shinyWidgets::materialSwitch(ns("highlightNearest"),
                                                                                              "highlight nearest", status = "info", value = TRUE),
                                                                 shinyWidgets::materialSwitch(ns("highlightHover"),
                                                                                              "highlight hover", status = "info", value = FALSE),  
                                                                 shinyWidgets::materialSwitch(ns("collapse"),
                                                                                              "Collapse", status = "info", value = TRUE),
                                                                 shiny::selectInput(ns("direction"), "Direction :", 
                                                                                    choices = c("up-down" = "UD", "down-up" = "DU",
                                                                                                "left-right" = "LR", "right-left" = "RL"), 
                                                                                    selected = "UD")
                                                   )
                                                 )
                    )
      )
    ),
    
    shiny::uiOutput(ns("treeUI")),
    shiny::downloadLink(ns('downloadNetwork'), 'Download Tree')
  )
}


#' run tree app
#'
#' @param  data  \code{rpart or data.drame} data in shiny app
#' @param  rpartParam \code{boolean}, add tab of rpartParam
#' @param  generalParam \code{boolean}, add tabs of generalParam
#' @param  id \code{character} id of module
#' @examples
#' \dontrun{
#' treeApp(data = iris)
#' treeApp(data = iris, generalParam = FALSE)
#' treeApp(data = rpart(iris), rpartParam = FALSE)
#' treeApp(data = rpart(iris), rpartParam = FALSE, generalParam = FALSE)
#' }
#' 
#' @export
treeApp <- function(data, id = "id1", rpartParam = TRUE, generalParam = TRUE){
  if("rpart" %in% class(data)){
    if(rpartParam == TRUE){
      stop("data is of class rpart and rpartParam is TRUE, you must pass a data.frame or rpartParam to FALSE")
    }
    return(shiny::shinyApp(ui = shiny::fluidPage(visTreeModuleUI(id = id,
                                                                 rpartParam = rpartParam,
                                                                 generalParam = generalParam)), 
                           server = function(input, output, session) {
                             shiny::callModule(treeServLigth, id ,data = shiny::reactive(data))
                           }))
  }
  
  if("data.frame" %in% class(data)){
    if(rpartParam == FALSE){
      stop("data is of class data.frame and rpartParam is FALSE, you must pass a rpart or rpartParam to TRUE")
    }
    
    return( shiny::shinyApp(ui = shiny::fluidPage(visTreeModuleUI(id = id,
                                                                  rpartParam = rpartParam,
                                                                  generalParam = generalParam)), 
                            server = function(input, output, session) {
                              shiny::callModule(treeServ, id ,data = shiny::reactive(data))
                            }))
  }
}
