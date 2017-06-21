library(shiny)
library(visNetwork)
library(rpart)
library(colourpicker)
library(shinyWidgets)


#' Generate server of shiny module from rpart reactive
#'
#' @param  input  \code{list} shiny input
#' @param  output \code{list}, shiny output
#' @param  session  \code{list}, shiny session
#' @param  data \code{reactive}, a \code{data.frame} or a \code{rpart} result. Must be a reactive object
#' 
#' @seealso \link{visTreeModuleUI}
#' 
#' @export
visTreeModuleServer <- function(input, output, session, data,
                                main = "",
                                submain = "",
                                footer = "",
                                direction = "UD",
                                fallenLeaves = FALSE,
                                rules = TRUE,
                                simplifyRules = TRUE,
                                shapeVar = "dot",
                                shapeY = "square",
                                colorVar = NULL,
                                colorY = NULL,
                                colorEdges = "#8181F7",
                                nodesFontSize = 16,
                                edgesFontSize = 14,
                                edgesFontAlign = "horizontal",
                                legend = TRUE,
                                legendNodesSize = 22,
                                legendFontSize = 16,
                                legendWidth = 0.1,
                                legendNcol = 1,
                                legendPosition = "left",
                                nodesPopSize = FALSE,
                                minNodeSize = 15,
                                maxNodeSize = 30,
                                highlightNearest =  list(enabled = TRUE,
                                                         degree = list(from = 50000, to = 0), hover = FALSE,
                                                         algorithm = "hierarchical"),
                                collapse = list(enabled = TRUE, fit = TRUE, resetHighlight = TRUE, 
                                                clusterOptions = list(fixed = TRUE, physics = FALSE)),
                                updateShape = TRUE,
                                tooltipDelay = 500,
                                digits = 3, 
                                height = 650,
                                width = "100%",
                                export = TRUE) {
  
  ns <- session$ns
  
  # reactive controls
  if (!shiny::is.reactive(main)){
    get_main <- shiny::reactive({main})
  } else {
    get_main <- main
  }
  
  if (!shiny::is.reactive(submain)){
    get_submain <- shiny::reactive({submain})
  } else {
    get_submain <- submain
  }
  
  if (!shiny::is.reactive(footer)){
    get_footer <- shiny::reactive({footer})
  } else {
    get_footer <- footer
  }
  
  if (!shiny::is.reactive(direction)){
    get_direction <- shiny::reactive({direction})
  } else {
    get_direction <- direction
  }
  
  if (!shiny::is.reactive(fallenLeaves)){
    get_fallenLeaves <- shiny::reactive({fallenLeaves})
  } else {
    get_fallenLeaves <- fallenLeaves
  }
  
  if (!shiny::is.reactive(rules)){
    get_rules <- shiny::reactive({rules})
  } else {
    get_rules <- rules
  }
  
  if (!shiny::is.reactive(simplifyRules)){
    get_simplifyRules <- shiny::reactive({simplifyRules})
  } else {
    get_simplifyRules <- simplifyRules
  }
  
  if (!shiny::is.reactive(shapeVar)){
    get_shapeVar <- shiny::reactive({shapeVar})
  } else {
    get_shapeVar <- shapeVar
  }
  
  if (!shiny::is.reactive(shapeY)){
    get_shapeY <- shiny::reactive({shapeY})
  } else {
    get_shapeY <- shapeY
  }
  
  if (!shiny::is.reactive(colorVar)){
    get_colorVar <- shiny::reactive({colorVar})
  } else {
    get_colorVar <- colorVar
  }
  
  if (!shiny::is.reactive(colorY)){
    get_colorY <- shiny::reactive({colorY})
  } else {
    get_colorY <- colorY
  }
  
  if (!shiny::is.reactive(colorEdges)){
    get_colorEdges <- shiny::reactive({colorEdges})
  } else {
    get_colorEdges <- colorEdges
  }
  
  if (!shiny::is.reactive(nodesFontSize)){
    get_nodesFontSize <- shiny::reactive({nodesFontSize})
  } else {
    get_nodesFontSize <- nodesFontSize
  }
  
  if (!shiny::is.reactive(edgesFontSize)){
    get_edgesFontSize <- shiny::reactive({edgesFontSize})
  } else {
    get_edgesFontSize <- edgesFontSize
  }
  
  if (!shiny::is.reactive(edgesFontAlign)){
    get_edgesFontAlign <- shiny::reactive({edgesFontAlign})
  } else {
    get_edgesFontAlign <- edgesFontAlign
  }
  
  if (!shiny::is.reactive(legend)){
    get_legend <- shiny::reactive({legend})
  } else {
    get_legend <- legend
  }
  
  if (!shiny::is.reactive(legendNodesSize)){
    get_legendNodesSize <- shiny::reactive({legendNodesSize})
  } else {
    get_legendNodesSize <- legendNodesSize
  }
  
  if (!shiny::is.reactive(legendFontSize)){
    get_legendFontSize <- shiny::reactive({legendFontSize})
  } else {
    get_legendFontSize <- legendFontSize
  }
  
  if (!shiny::is.reactive(legendWidth)){
    get_legendWidth <- shiny::reactive({legendWidth})
  } else {
    get_legendWidth <- legendWidth
  }
  
  if (!shiny::is.reactive(legendNcol)){
    get_legendNcol <- shiny::reactive({legendNcol})
  } else {
    get_legendNcol <- legendNcol
  }
  
  if (!shiny::is.reactive(legendPosition)){
    get_legendPosition <- shiny::reactive({legendPosition})
  } else {
    get_legendPosition <- legendPosition
  }
  
  if (!shiny::is.reactive(nodesPopSize)){
    get_nodesPopSize <- shiny::reactive({nodesPopSize})
  } else {
    get_nodesPopSize <- nodesPopSize
  }
  
  if (!shiny::is.reactive(minNodeSize)){
    get_minNodeSize <- shiny::reactive({minNodeSize})
  } else {
    get_minNodeSize <- minNodeSize
  }
  
  if (!shiny::is.reactive(maxNodeSize)){
    get_maxNodeSize <- shiny::reactive({maxNodeSize})
  } else {
    get_maxNodeSize <- maxNodeSize
  }
  
  if (!shiny::is.reactive(highlightNearest)){
    get_highlightNearest <- shiny::reactive({highlightNearest})
  } else {
    get_highlightNearest <- highlightNearest
  }
  
  if (!shiny::is.reactive(collapse)){
    get_collapse <- shiny::reactive({collapse})
  } else {
    get_collapse <- collapse
  }
  
  if (!shiny::is.reactive(updateShape)){
    get_updateShape <- shiny::reactive({updateShape})
  } else {
    get_updateShape <- updateShape
  }
  
  if (!shiny::is.reactive(tooltipDelay)){
    get_tooltipDelay <- shiny::reactive({tooltipDelay})
  } else {
    get_tooltipDelay <- tooltipDelay
  }
  
  if (!shiny::is.reactive(digits)){
    get_digits <- shiny::reactive({digits})
  } else {
    get_digits <- digits
  }
  
  if (!shiny::is.reactive(height)){
    get_height <- shiny::reactive({height})
  } else {
    get_height <- height
  }
  
  if (!shiny::is.reactive(width)){
    get_width <- shiny::reactive({width})
  } else {
    get_width <- width
  }
  
  if (!shiny::is.reactive(export)){
    get_export <- shiny::reactive({export})
  } else {
    get_export <- export
  }
  
  # update input
  shiny::observe({
    legend <- get_legend()
    shiny::isolate({
      if("legend" %in% names(input)){
        shiny::updateCheckboxInput(session, "legend", "Display legend", value = legend)
      }
    })
  })
  
  shiny::observe({
    fallenLeaves <- get_fallenLeaves()
    shiny::isolate({
      if("fallenLeaves" %in% names(input)){
        shiny::updateCheckboxInput(session, "fallenLeaves", "Fallen leaves", fallenLeaves)
      }
    })
  })
  
  shiny::observe({
    rules <- get_rules()
    shiny::isolate({
      if("rules" %in% names(input)){
        shiny::updateCheckboxInput(session, "rules","Display rules", value = rules)
      }
    })
  })
  
  shiny::observe({
    simplifyRules <- get_simplifyRules()
    shiny::isolate({
      if("simpRules" %in% names(input)){
        shiny::updateCheckboxInput(session, "simpRules", "Simplify rules", value = simplifyRules)
      }
    })
  })
  
  shiny::observe({
    updateShape <- get_updateShape()
    isolate({
      if("updateShape" %in% names(input)){
        shiny::updateCheckboxInput(session, "updateShape", "Update shape", value = updateShape)
      }
    })
  })
  
  shiny::observe({
    export <- get_export()
    shiny::isolate({
      if("export" %in% names(input)){
        shiny::updateCheckboxInput(session, "export", "Export", value = export)
      }
    })
  })
  
  shiny::observe({
    height <- get_height()
    shiny::isolate({
      if("height" %in% names(input)){
        shiny::updateNumericInput(session, "height", "Height :", value = height)
      }
    })
  })
  
  shiny::observe({
    digits <- get_digits()
    shiny::isolate({
      if("digits" %in% names(input)){
        shiny::updateNumericInput(session, "digits", "Digits : ", value = digits, min = 0)
      }
    })
  })
  
  shiny::observe({
    tooltipDelay <- get_tooltipDelay()
    shiny::isolate({
      if("tooltipDelay" %in% names(input)){
        shiny::updateNumericInput(session, "tooltipDelay", "Tooltip delay :", value = tooltipDelay, min = 0, step = 100)
      }
    })
  })
  
  shiny::observe({
    legendWidth <- get_legendWidth()
    shiny::isolate({
      if("legendWidth" %in% names(input)){
        shiny::updateNumericInput(session, "legendWidth", "Width :", value = legendWidth, min = 0, max = 0.5, step = 0.05)
      }
    })
  })
  
  shiny::observe({
    legendNcol <- get_legendNcol()
    shiny::isolate({
      if("legendNcol" %in% names(input)){
        shiny::updateNumericInput(session, "legendNcol", "Number of columns :", value = legendNcol, min = 1, max = 50, step = 1)
      }
    })
  })
  
  shiny::observe({
    legendPosition <- get_legendPosition()
    shiny::isolate({
      if("legendPosition" %in% names(input)){
        shiny::updateSelectInput(session, "legendPosition", "Position", choices = c("left", "right"), selected = legendPosition)
      }
    })
  })
  
  shiny::observe({
    shapeY <- get_shapeY()
    shiny::isolate({
      if("shapeY" %in% names(input)){
        shiny::updateSelectInput(session, "shapeY", "shape Y", choices = c("diamond",
                                                                           "dot",
                                                                           "star",
                                                                           "triangle",
                                                                           "triangleDown",
                                                                           "square",
                                                                           "ellipse", 
                                                                           "circle", 
                                                                           "database", 
                                                                           "box", 
                                                                           "text"), selected = shapeY)
      }
    })
  })
  
  shiny::observe({
    shapeVar <- get_shapeVar()
    shiny::isolate({
      if("shapeX" %in% names(input)){
        shiny::updateSelectInput(session, "shapeX", "shape X", choices = c("diamond",
                                                                           "dot",
                                                                           "star",
                                                                           "triangle",
                                                                           "triangleDown",
                                                                           "square",
                                                                           "ellipse", 
                                                                           "circle", 
                                                                           "database", 
                                                                           "box", 
                                                                           "text"), selected = shapeVar)
      }
    })
  })
  
  shiny::observe({
    nodesSize <- (get_maxNodeSize() + get_minNodeSize()) / 2
    shiny::isolate({
      if("nodesSize" %in% names(input)){
        shiny::updateNumericInput(session, "nodesSize", "Nodes size", value = nodesSize, min = 1)
      }
      if("maxNodeSize" %in% names(input)){
        shiny::updateNumericInput(session, "maxNodeSize", "Max nodes size", value = get_maxNodeSize(), min = 1)
      }
      if("minNodeSize" %in% names(input)){
        shiny::updateNumericInput(session, "minNodeSize", "Min nodes size", value = get_minNodeSize(), min = 1)
      }
    })
  })
  
  shiny::observe({
    legendNodesSize <- get_legendNodesSize()
    shiny::isolate({
      if("legendNodesSize" %in% names(input)){
        shiny::updateNumericInput(session, "legendNodesSize", "Legend size", value = legendNodesSize, min = 1)
      }
    })
  })
  
  shiny::observe({
    nodesFontSize <- get_nodesFontSize()
    shiny::isolate({
      if("nodesFontSize" %in% names(input)){
        shiny::updateNumericInput(session, "nodesFontSize", "Nodes font", value = nodesFontSize, min = 1)
      }
    })
  })
  
  shiny::observe({
    legendFontSize <- get_legendFontSize()
    shiny::isolate({
      if("legendFontSize" %in% names(input)){
        shiny::updateNumericInput(session, "legendFontSize", "Legend font", value = legendFontSize, min = 1)
      }
    })
  })
  
  shiny::observe({
    colorEdges <- get_colorEdges()
    shiny::isolate({
      if("colorEdges" %in% names(input)){
        colourpicker::updateColourInput(session, "colorEdges", "Edges color", value = colorEdges)
      }
    })
  })
  
  shiny::observe({
    edgesFontSize <- get_edgesFontSize()
    shiny::isolate({
      if("edgesFontSize" %in% names(input)){
        shiny::updateNumericInput(session, "edgesFontSize", "Edges font", value = edgesFontSize, min = 1)
      }
    })
  })
  
  shiny::observe({
    edgesFontAlign <- get_edgesFontAlign()
    shiny::isolate({
      if("edgesFontAlign" %in% names(input)){
        shiny::updateSelectInput(session, "edgesFontAlign", "Edges font align",
                                 choices = c("horizontal", "top", "middle", "bottom"), 
                                 selected = edgesFontAlign)
      }
    })
  })
  
  shiny::observe({
    direction <- get_direction()
    shiny::isolate({
      if("direction" %in% names(input)){
        shiny::updateSelectInput(session, "direction", "Direction :",
                                 choices = c("up-down" = "UD", "down-up" = "DU",
                                             "left-right" = "LR", "right-left" = "RL"), 
                                 selected = direction)
      }
    })
  })
  
  shiny::observe({
    highlightNearest <- get_highlightNearest()
    shiny::isolate({
      nearest <- TRUE
      hover <- FALSE
      
      if(is.logical(highlightNearest)){
        nearest <- highlightNearest
        hover <- FALSE
      } else {
        if(!is.null(highlightNearest$enabled)){
          nearest <- highlightNearest$enabled
        }
        if(!is.null(highlightNearest$hover)){
          hover <- highlightNearest$hover
        }
      }
      
      if("highlightNearest" %in% names(input)){
        shinyWidgets::updateMaterialSwitch(session, "highlightNearest", value = nearest)
      }
      if("highlightHover" %in% names(input)){
        shinyWidgets::updateMaterialSwitch(session, "highlightHover", value = hover)
      }
    })
  })
  
  shiny::observe({
    collapse <- get_collapse()
    shiny::isolate({
      value <- TRUE
      if(is.logical(collapse)){
        value <- collapse
      } else {
        if(!is.null(collapse$enabled)){
          value <- collapse$enabled
        }
      }
      if("collapse" %in% names(input)){
        shinyWidgets::updateMaterialSwitch(session, "collapse", value = value)
      }
    })
  })
  
  # update main
  shiny::observe({
    main <- get_main()
    shiny::isolate({
      if(any(c("character", "factor") %in% class(main))){
        shiny::updateTextInput(session, "main", value = main)
      } else {
        if(is.list(main)){
          if(!is.null(main$text)){
            shiny::updateTextInput(session, "main", value = main$text)
          }
          
          if(!is.null(main$style)){
            find_size <- gregexpr("(font-size:)[[:digit:]]*", main$style)
            size <- as.numeric(gsub("font-size:", "", unlist(regmatches(main$style, find_size))))[1]
            if(!is.na(size)){
              shiny::updateNumericInput(session, "mainsize", value = size)
            }
            find_color <- gregexpr("(color:#)[[:alnum:]]*", main$style)
            color <- gsub("color:", "", unlist(regmatches(main$style, find_color)))[1]
            if(!is.na(color)){
              colourpicker::updateColourInput(session, "colourMain", value = color)
            }
          }
        }
      }
    })
  })
  
  # update submain
  shiny::observe({
    submain <- get_submain()
    shiny::isolate({
      if(any(c("character", "factor") %in% class(submain))){
        shiny::updateTextInput(session, "submain", value = submain)
      } else {
        if(is.list(submain)){
          if(!is.null(submain$text)){
            shiny::updateTextInput(session, "submain", value = submain$text)
          }
          
          if(!is.null(submain$style)){
            find_size <- gregexpr("(font-size:)[[:digit:]]*", submain$style)
            size <- as.numeric(gsub("font-size:", "", unlist(regmatches(submain$style, find_size))))[1]
            if(!is.na(size)){
              shiny::updateNumericInput(session, "Submainsize", value = size)
            }
            find_color <- gregexpr("(color:#)[[:alnum:]]*", submain$style)
            color <- gsub("color:", "", unlist(regmatches(submain$style, find_color)))[1]
            if(!is.na(color)){
              colourpicker::updateColourInput(session, "colourSubMain", value = color)
            }
          }
        }
      }
    })
  })
  
  # update footer
  shiny::observe({
    footer <- get_footer()
    shiny::isolate({
      if(any(c("character", "factor") %in% class(footer))){
        shiny::updateTextInput(session, "footer", value = footer)
      } else {
        if(is.list(footer)){
          if(!is.null(footer$text)){
            shiny::updateTextInput(session, "footer", value = footer$text)
          }
          
          if(!is.null(footer$style)){
            find_size <- gregexpr("(font-size:)[[:digit:]]*", footer$style)
            size <- as.numeric(gsub("font-size:", "", unlist(regmatches(footer$style, find_size))))[1]
            if(!is.na(size)){
              shiny::updateNumericInput(session, "Footermainsize", value = size)
            }
            find_color <- gregexpr("(color:#)[[:alnum:]]*", footer$style)
            color <- gsub("color:", "", unlist(regmatches(footer$style, find_color)))[1]
            if(!is.na(color)){
              colourpicker::updateColourInput(session, "colourFooterMain", value = color)
            }
          }
        }
      }
    })
  })
  
  shiny::observe({
    if("data.frame" %in% class(data())){
      shiny::updateSelectInput(session, inputId = "y", choices = names(data()))
    }
  })
  
  shiny::observe({
    if("data.frame" %in% class(data())){
      shiny::updateSelectInput(session, inputId = "x", choices = names(data())[names(data())!=input$y],
                               selected = names(data())[names(data())!=input$y])
    }
  })
  
  # Get rpart from reactive data
  rpart_tree <- shiny::reactive({
    input$runTree
    if("rpart" %in% class(data())){
      data()
    } else {
      if(input$runTree > 0){
        shiny::isolate({
          formule <- paste(input$y, "~", paste0(input$x, collapse = "+")) %>% as.formula()
          rpart(formule, data = data(), 
                control = rpart.control(cp = input$complexity, minsplit = input$minsplit))
        })
      } else {
        NULL
      }
    }
  })
  
  shiny::observe({
    input$y
    input$x
    input$complexity
    input$minsplit
    shiny::updateCheckboxInput(session,"usecolornodes", value = FALSE )
    shiny::updateCheckboxInput(session,"usecolorY", value = FALSE )
  })
  
  # data.frame with variables colors
  infoColors <- shiny::reactiveValues(colorVar = NULL, colorY = NULL)
  
  # Build tree (visTree)
  treeBuild <- shiny::reactive({
    res <- rpart_tree()
    if(!is.null(res)){
      shiny::isolate({
        main <- get_main()
        submain <- get_submain()
        footer <- get_footer()
        direction <- get_direction()
        fallenLeaves <- get_fallenLeaves()
        rules <- get_rules()
        simplifyRules <- get_simplifyRules()
        shapeVar <- get_shapeVar()
        shapeY <- get_shapeY()
        colorVar <- get_colorVar()
        colorY <- get_colorY()
        colorEdges <- get_colorEdges()
        nodesFontSize <- get_nodesFontSize()
        edgesFontSize <- get_edgesFontSize()
        edgesFontAlign <- get_edgesFontAlign()
        legend <- get_legend()
        legendNodesSize <- get_legendNodesSize()
        legendFontSize <- get_legendFontSize()
        legendWidth <- get_legendWidth()
        legendNcol <- get_legendNcol()
        legendPosition <- get_legendPosition()
        nodesPopSize <- get_nodesPopSize()
        minNodeSize <- get_minNodeSize()
        maxNodeSize <- get_maxNodeSize()
        highlightNearest <- get_highlightNearest()
        collapse <- get_collapse()
        updateShape <- get_updateShape()
        tooltipDelay <- get_tooltipDelay()
        digits <- get_digits()
        height <- get_height()
        width <- get_width()
        export <- get_export()
        
        if("runTree" %in% names(input)){
          if(input$runTree > 0){
            legend <- input$legend
            rules <- input$rules
            tooltipDelay <- input$tooltipDelay
            updateShape <- input$updateShape
            fallenLeaves <- input$fallenLeaves
            digits <- input$digits
            height <- input$height
            nodesPopSize <- input$nodesPopSize
            export <- input$export
            
            colorVar <- updateColorVar()
            colorY <- updateColorY()
            # 
            main = build_main()
            submain = build_submain()
            footer = build_footer()
            
            simplifyRules <- ifelse(is.null(input$simpRules), simplifyRules, input$simpRules)
            legendWidth <- ifelse(is.null(input$legendWidth), legendWidth, input$legendWidth)
            legendNcol <- ifelse(is.null(input$legendNcol), legendNcol, input$legendNcol)
            legendNodesSize <- ifelse(is.null(input$legendNodesSize), legendNodesSize, input$legendNodesSize)
            legendFontSize <- ifelse(is.null(input$legendFontSize), legendFontSize, input$legendFontSize)
            legendPosition <- ifelse(is.null(input$legendPosition), legendPosition, input$legendPosition)
            minNodeSize <- ifelse(is.null(input$minNodeSize), minNodeSize, input$minNodeSize)
            maxNodeSize <- ifelse(is.null(input$maxNodeSize), maxNodeSize, input$maxNodeSize)
            direction <- ifelse(is.null(input$direction), direction, input$direction)
            shapeY <- ifelse(is.null(input$shapeY), shapeY, input$shapeY)
            shapeVar <- ifelse(is.null(input$shapeX), shapeVar, input$shapeX)
            edgesFontSize <- ifelse(is.null(input$edgesFontSize), edgesFontSize, input$edgesFontSize)
            edgesFontAlign <- ifelse(is.null(input$edgesFontAlign), edgesFontAlign, input$edgesFontAlign)
            nodesFontSize <- ifelse(is.null(input$nodesFontSize), nodesFontSize, input$nodesFontSize)
            colorEdges <- ifelse(is.null(input$colorEdges), colorEdges, input$colorEdges)
          }
        }
        
        tree <- visTree(res,
                        main = main,
                        submain = submain,
                        footer = footer,
                        shapeY = shapeY,
                        shapeVar = shapeVar,
                        colorVar = colorVar,
                        colorY = colorY,
                        rules = rules,
                        simplifyRules = simplifyRules,
                        legend = legend,
                        legendWidth = legendWidth,
                        legendNodesSize = legendNodesSize,
                        legendFontSize = legendFontSize,
                        legendPosition = legendPosition,
                        legendNcol = legendNcol,
                        edgesFontSize = edgesFontSize,
                        edgesFontAlign = edgesFontAlign,
                        nodesFontSize = nodesFontSize,
                        nodesPopSize = nodesPopSize,
                        minNodeSize = minNodeSize,
                        maxNodeSize = maxNodeSize,
                        tooltipDelay = tooltipDelay,
                        digits = digits,
                        direction = direction,
                        fallenLeaves = fallenLeaves,
                        updateShape = updateShape,
                        colorEdges = colorEdges,
                        highlightNearest = highlightNearest,
                        collapse = collapse,
                        height = paste0(height, "px"),
                        export = export)  %>%
          visEvents(type = "on", doubleClick = "networkOpenCluster")
        
        # save color information
        infoColors$colorVar <- tree$x$tree$colorVar
        infoColors$colorY <- tree$x$tree$colorY
        
        tree
      })
    } else {
      NULL
    }
  })
  
  output$is_tree <- shiny::reactive({
    !is.null(treeBuild())
  })
  
  shiny::outputOptions(output, "is_tree", suspendWhenHidden = FALSE)
  
  # the tree
  output$tree <- visNetwork::renderVisNetwork({
    treeBuild()
  })
  
  # Give names of variables
  infoRpartNodes <- shiny::reactive({
    res <- rpart_tree()
    if(!is.null(res)){
      nodes_var <- as.character(res$frame$var)
      nodes_var_x <- nodes_var[nodes_var != "<leaf>"]
      sortLabels <- unique(nodes_var_x)
      
      if(!is.null(attributes(res)$ylevels)){
        infoClass <- attributes(res)$ylevels
        nlevelsClass <- length(infoClass)
        probaClass <- res$frame[,"yval2"]
        effectif <- data.frame(probaClass[,2:(nlevelsClass+1), drop = F])
        probs <- data.frame(probaClass[,(nlevelsClass+2):(ncol(probaClass)-1), drop = F])
      } else {
        infoClass <- NULL
        probs <- NULL
      }
      
      list(nodes_var = nodes_var, nodes_var_x = nodes_var_x, sortLabels = sortLabels, 
           infoClass = infoClass, probs = probs)
    }
  })
  
  # Get color
  updateColorVar <- shiny::reactive({
    newColorVar <- infoColors$colorVar
    if(!is.null(newColorVar)){
      input_name <- gsub(" ", "", paste0(newColorVar$variable, "var"))
      newColorVar$color <- as.character(newColorVar$color)
      if(!is.null(input[[input_name[1]]])){
        colorVect <- sapply(input_name, function(X){
          tmp <- input[[X]]
          ifelse(is.null(tmp), "", tmp)
        })
        newColorVar$color[colorVect != ""] <- colorVect[colorVect != ""]
      }
      newColorVar
    } else {
      NULL
    }
  })
  
  # Color for Y
  # data.frame with variables colors
  colorYData <- shiny::reactive({
    visNetwork:::.generateYColor(object = shiny::isolate(rpart_tree()), colorY = get_colorY(),
                                 nodes_var = infoRpartNodes()$nodes_var_x, 
                                 infoClass = infoRpartNodes()$infoClass, probs = infoRpartNodes()$probs)
  })
  
  updateColorY <- shiny::reactive({
    colorY <- colorYData()$colorY
    clas <- attributes(rpart_tree())$ylevels
    if(!is.null(clas)){
      dest <- paste0(gsub(" ", "", paste0(colorY$modality, "Y")))
      if(!is.null(shiny::isolate(input[[dest[1]]]))){
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
  
  # UI for color
  output$colorY <- shiny::renderUI({
    if(!input$usecolorY){
      return(NULL)
    }else{
      corY <- updateColorY()
      clas <- attributes(rpart_tree())$ylevels
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
    if(!is.null(m)){
      oldId <- m$x$nodes$id
      colorIn <- m$x$nodes$color

      legOldId <- m$x$legend$nodes$id
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

      clas <- attributes(isolate(rpart_tree()))$ylevels

      if(!is.null(input$minY) & is.null(clas)){
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
      # if(!is.null(input[[gsub(" ", "", paste0(m$x$nodes[1,]$label, "Y"))]])){
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
      # }

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
    }
  })

  # Update direction
  shiny::observe({
    print("udpate direction")
    visNetworkProxy(ns("tree")) %>%
      visHierarchicalLayout(direction = input$direction)
  })

  # Update node size
  shiny::observe({
    minNodeSize <- input$minNodeSize
    maxNodeSize <- input$maxNodeSize
    m <- shiny::isolate(treeBuild())
    if(input$nodesPopSize & !is.null(m)){
      newNodes <- data.frame(id = m$x$nodes$id, scaling.min = minNodeSize, scaling.max = maxNodeSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(newNodes)
    }
  })

  # Update node size
  shiny::observe({
    nodesSize <- input$nodesSize
    m <- shiny::isolate(treeBuild())
    if(!input$nodesPopSize & !is.null(m)){
      newNodes <- data.frame(id = m$x$nodes$id, scaling.min = nodesSize, scaling.max = nodesSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(newNodes)
    }
  })

  # Update legend node size
  shiny::observe({
    legendNodesSize <- input$legendNodesSize
    m <- shiny::isolate(treeBuild())
    if(!is.null(m$x$legend)){
      change_legend_nodes <- data.frame(id = m$x$legend$nodes$id, size = legendNodesSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(change_legend_nodes, legend = TRUE)
    }
  })

  # Update legend font size
  shiny::observe({
    legendFontSize <- input$legendFontSize
    m <- shiny::isolate(treeBuild())
    if(!is.null(m$x$legend)){
      change_legend_nodes <- data.frame(id = m$x$legend$nodes$id, font.size = legendFontSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(change_legend_nodes, legend = TRUE)
    }
  })

  # Update edges color
  shiny::observe({
    colorEdges <- input$colorEdges
    m <- shiny::isolate(treeBuild())
    if(!is.null(m$x$edges)){
      idEdges <- m$x$edges$id
      newEdges <- data.frame(id = m$x$edges$id, color = colorEdges)
      visNetworkProxy(ns("tree")) %>%
        visUpdateEdges(newEdges)
    }
  })

  # Update edges font size
  shiny::observe({
    edgesFontSize <- input$edgesFontSize
    m <- shiny::isolate(treeBuild())
    if(!is.null(m$x$edges)){
      idEdges <- m$x$edges$id
      newEdges <- data.frame(id = m$x$edges$id, font.size = edgesFontSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateEdges(newEdges)
    }
  })

  # Update edges font align
  shiny::observe({
    edgesFontAlign <- input$edgesFontAlign
    m <- shiny::isolate(treeBuild())
    if(!is.null(m$x$edges)){
      idEdges <- m$x$edges$id
      newEdges <- data.frame(id = m$x$edges$id, font.align = edgesFontAlign)
      visNetworkProxy(ns("tree")) %>%
        visUpdateEdges(newEdges)
    }
  })

  # Update nodes font size
  shiny::observe({
    nodesFontSize <- input$nodesFontSize
    m <- shiny::isolate(treeBuild())
    if(!is.null(m)){
      newNodes <- data.frame(id = m$x$nodes$id, font.size = nodesFontSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(newNodes)
    }
  })

  # reactive for title
  build_main <- shiny::reactive({
    text <- input$main
    style <- paste0("font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:", 
                    input$mainsize,"px;text-align:center;color:", input$colourMain, ";")
    list(text = text, style = style)
  })

  # Update title
  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visSetTitle(main = build_main())
  })


  build_submain <- shiny::reactive({
    text <- input$submain
    style <- paste0("font-family:Georgia, Times New Roman, Times, serif;font-size:", 
                    input$Submainsize,"px;text-align:center;color:", input$colourSubMain, ";")
    list(text = text, style = style)
  })

  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visSetTitle(submain =  build_submain())
  })

  build_footer <- shiny::reactive({
    text <- input$footer
    style <- paste0("font-family:Georgia, Times New Roman, Times, serif;font-size:", 
                    input$Footermainsize,"px;text-align:center;color:", input$colourFooterMain, ";")
    list(text = text, style = style)
  })

  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visSetTitle(footer = build_footer())
  })
  
  # Update shape Ynodes
  shiny::observe({
    shapeX <- input$shapeX
    shapeY <- input$shapeY
    m <- shiny::isolate(treeBuild())
    if(!is.null(m)){
    # nodes
    change_nodes <- data.frame(id = m$x$nodes$id, shape = ifelse(m$x$nodes$Leaf == 0, shapeX, shapeY))
    visNetworkProxy(ns("tree")) %>%
      visUpdateNodes(change_nodes)
    # legend
    change_legend_nodes <- data.frame(id = m$x$legend$nodes$id, shape = ifelse(m$x$legend$nodes$Leaf == 0, shapeX, shapeY))
    visNetworkProxy(ns("tree")) %>%
      visUpdateNodes(change_legend_nodes, legend = TRUE)
    }
  })
  
  # Update highlightNearest
  update_highlightNearest <- shiny::reactive({
    list(enabled = input$highlightNearest, degree = list(from = 50000, to = 0),
         hover = input$highlightHover, algorithm = "hierarchical")

  })

  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visOptions(highlightNearest = update_highlightNearest())
  })

  # Update collapse
  update_collapse <- shiny::reactive({
    list(enabled = input$collapse, fit = TRUE, resetHighlight = TRUE, 
         clusterOptions = list(fixed = TRUE, physics= FALSE))
  })

  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visOptions(collapse = update_collapse())
  })
  
  # Update highlightNearest
  update_highlightNearest <- shiny::reactive({
    list(enabled = input$highlightNearest, degree = list(from = 50000, to = 0),
         hover = input$highlightHover, algorithm = "hierarchical")

  })

  shiny::observe({
    visNetworkProxy(ns("tree")) %>%
      visOptions(highlightNearest = update_highlightNearest())
  })

  # Update height
  output$treeUI <- shiny::renderUI({
    res <- rpart_tree()
    shiny::isolate({
      height <- get_height()
      if("runTree" %in% names(input)){
        if(input$runTree > 0){
          height <- ifelse(is.null(input$height), height, input$height)
        }
      }
      visNetworkOutput(ns("tree"), height = paste0(height, "px"))
    })
  })
  
  build_export_tree <- shiny::reactive({
    res <- rpart_tree()
    
    legend <- ifelse(is.null(input$legend), get_legend(), input$legend)
    rules <- ifelse(is.null(input$rules), get_rules(), input$rules)
    tooltipDelay <- ifelse(is.null(input$tooltipDelay), get_tooltipDelay(), input$tooltipDelay)
    updateShape <- ifelse(is.null(input$updateShape), get_updateShape(), input$updateShape)
    fallenLeaves <- ifelse(is.null(input$fallenLeaves), get_fallenLeaves(), input$fallenLeaves)
    digits <- ifelse(is.null(input$digits), get_digits(), input$digits)
    nodesPopSize <- ifelse(is.null(input$nodesPopSize), get_nodesPopSize(), input$nodesPopSize)
    export <- ifelse(is.null(input$export), get_export(), input$export)
    simplifyRules <- ifelse(is.null(input$simpRules), get_simplifyRules(), input$simpRules)
    legendWidth <- ifelse(is.null(input$legendWidth), get_legendWidth(), input$legendWidth)
    legendNcol <- ifelse(is.null(input$legendNcol), get_legendNcol(), input$legendNcol)
    legendNodesSize <- ifelse(is.null(input$legendNodesSize), get_legendNodesSize(), input$legendNodesSize)
    legendFontSize <- ifelse(is.null(input$legendFontSize), get_legendFontSize(), input$legendFontSize)
    legendPosition <- ifelse(is.null(input$legendPosition), get_legendPosition(), input$legendPosition)
    minNodeSize <- ifelse(is.null(input$minNodeSize),  get_minNodeSize(), input$minNodeSize)
    maxNodeSize <- ifelse(is.null(input$maxNodeSize), get_maxNodeSize(), input$maxNodeSize)
    direction <- ifelse(is.null(input$direction), get_direction(), input$direction)
    shapeY <- ifelse(is.null(input$shapeY), get_shapeY(), input$shapeY)
    shapeVar <- ifelse(is.null(input$shapeX), get_shapeVar(), input$shapeX)
    edgesFontSize <- ifelse(is.null(input$edgesFontSize), get_edgesFontSize(), input$edgesFontSize)
    edgesFontAlign <- ifelse(is.null(input$edgesFontAlign), get_edgesFontAlign(), input$edgesFontAlign)
    nodesFontSize <- ifelse(is.null(input$nodesFontSize), get_nodesFontSize(), input$nodesFontSize)
    colorEdges <- ifelse(is.null(input$colorEdges), get_colorEdges(), input$colorEdges)
    colorVar <- updateColorVar()
    colorY <- updateColorY()
    
    out <- visTree(res,
                   main = build_main(),
                   submain = build_submain(),
                   footer = build_footer(),
                   shapeY = shapeY,
                   shapeVar = shapeVar,
                   colorVar = colorVar,
                   colorY = colorY,
                   rules = rules,
                   simplifyRules = simplifyRules,
                   legend = legend,
                   legendWidth = legendWidth,
                   legendNodesSize = legendNodesSize,
                   legendFontSize = legendFontSize,
                   legendPosition = legendPosition,
                   legendNcol = legendNcol,
                   edgesFontSize = edgesFontSize,
                   edgesFontAlign = edgesFontAlign,
                   nodesFontSize = nodesFontSize,
                   nodesPopSize = nodesPopSize,
                   minNodeSize = minNodeSize,
                   maxNodeSize = maxNodeSize,
                   tooltipDelay = tooltipDelay,
                   digits = digits,
                   direction = direction,
                   fallenLeaves = fallenLeaves,
                   updateShape = updateShape,
                   colorEdges = colorEdges,
                   highlightNearest = update_highlightNearest(),
                   collapse = update_collapse(),
                   height = paste0(input$export_height, "px"),
                   export = export)
  })
  
  # download tree
  output$downloadNetwork <- shiny::downloadHandler(
    filename = function() {
      paste('tree-', Sys.Date(), '.html', sep='')
    },
    content = function(con) {
      out <- build_export_tree()
      out  %>% visSave(con, TRUE, input$export_background)
    }
  )
}


#' Generate ui for visTree shiny module. 
#'
#' @param  id \code{character} id of module, refear to id in \link{treeServLigth} or \link{treeServ}
#' @param  visTreeParams \code{boolean}, add tabs for visTree parameters
#' 
#' @return \code{html} html of shiny module
#' 
#' @examples
#' \dontrun{
#' data <- iris
#' shiny::shinyApp(ui = shiny::fluidPage(visTreeModuleUI(id = "id1",
#'  rpartParams = FALSE,
#'  visTreeParams = FALSE)), 
#'  server = function(input, output, session) {
#'  shiny::callModule(treeServLigth, "id1", data = shiny::reactive(rpart(data)))
#' })
#' 
#' shiny::shinyApp(ui = shiny::fluidPage(visTreeModuleUI(id = "id1",
#'  rpartParams = TRUE,
#'  visTreeParams = FALSE)), 
#'  server = function(input, output, session) {
#'  shiny::callModule(treeServ, "id1", data = shiny::reactive(data))
#' })
#' 
#' 
#' shiny::shinyApp(ui = shiny::fluidPage(visTreeModuleUI(id = "id1",
#'  rpartParams = TRUE,
#'  visTreeParams = TRUE)), 
#'  server = function(input, output, session) {
#'  shiny::callModule(treeServ, "id1", data = shiny::reactive(data))
#' })
#' 
#' shiny::shinyApp(ui = 
#' navbarPage("Menu",shiny::tabPanel(
#'   "tt1",shiny::fluidPage(visTreeModuleUI(id = "id1", 
#'   rpartParams = TRUE,
#'   visTreeParams = TRUE))
#' ),
#' shiny::tabPanel(
#'   "tt2",shiny::fluidPage(visTreeModuleUI(id = "id2", 
#'   rpartParams = FALSE,
#'   visTreeParams = FALSE)))
#' ), 
#' server = function(input, output, session) {
#'   shiny::callModule(treeServ, "id1", data = shiny::reactive(iris))
#'   shiny::callModule(treeServLigth, "id2", data = shiny::reactive(rpart(iris)))
#' })
#' }
#' 
#' @export
visTreeModuleUI <- function(id, rpartParams = TRUE, visTreeParams = TRUE) {
  ns <- shiny::NS(id)
  selectedTabs <- ifelse(rpartParams, "rpart", "visTree options")
  
  shiny::fluidPage(
    if(visTreeParams | rpartParams){
      shiny::tabsetPanel(
        id = ns('boxparam'),
        selected = selectedTabs ,
        if(rpartParams){
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
                                                     shiny::sliderInput(ns("complexity"), "Complexity :",
                                                                        min = 0, max = 1, value = 0.005, step = 0.005)
                                       ),
                                       shiny::column(2,
                                                     shiny::numericInput(ns("minsplit"), "Minsplit : ", value = 20, min = 2)
                                       )
                                       
                            )
                          )
          )
        }else{""},
        
        if(visTreeParams){
          shiny::tabPanel("visTree options",
                          #Params graph
                          shiny::fluidRow(
                            shiny::column(1, 
                                          shiny::numericInput(ns("height"), "Height :",650)
                            ),
                            shiny::column(1,
                                          shiny::numericInput(ns("digits"), "Digits : ", value = 3, min = 0)
                            ),
                            shiny::column(1, 
                                          shiny::numericInput(ns("tooltipDelay"), "Tooltip delay :", value = 500, min = 0, step = 100)
                            ),
                            shiny::column(2,
                                          shiny::br(), shiny::checkboxInput(ns("fallenLeaves"), "Fallen leaves")
                            ),
                            shiny::column(2,
                                          shiny::br(), shiny::checkboxInput(ns("updateShape"), "Update shape",value = TRUE)
                            ),
                            shiny::column(2,
                                          shiny::br(), shiny::checkboxInput(ns("rules"),"Display rules", value = TRUE)
                            ),
                            shiny::conditionalPanel(paste0("input['",ns("rules"),"'] == true"),
                                                    shiny::column(2,
                                                                  shiny::br(), shiny::checkboxInput(ns("simpRules"),"Simplify rules", value = TRUE))
                            ),
                            shiny::column(1,
                                          shiny::br(), shiny::checkboxInput(ns("export"), "Export", value = TRUE)
                            )
                          )
          )
        }else{""},
        
        if(visTreeParams){
          shiny::tabPanel("Legend",
                          shiny::fluidRow(
                            shiny::column(2, offset = 1, 
                                          shiny::br(), shiny::checkboxInput(ns("legend"),"Display legend", value = TRUE)
                            ),
                            # shiny::conditionalPanel(paste0("input['",ns("legend"),"'] == true"),
                            shiny::column(2,
                                          shiny::numericInput(ns("legendWidth"), "Width :",
                                                              value = 0.1, min = 0, max = 0.5, step = 0.05)
                            ),
                            shiny::column(2,
                                          shiny::numericInput(ns("legendNcol"), "Number of columns :",
                                                              value = 1, min = 1, max = 50, step = 1)
                            ),
                            shiny::column(2,
                                          shiny::selectInput(ns("legendPosition"),"Position",choices = c("left", "right"))
                            )
                          )
          )
        }else{""}
        
      )
    },
    
    if(visTreeParams | rpartParams){
      shiny::div(
        shiny::actionButton(ns("runTree"), "Run tree", class = "btnruntree"), align = "center"
      )
    }else{""},
    
    shiny::br(),
    
    # Show a plot of the generated distribution
    # shiny::uiOutput("treeUI"),
    # shiny::conditionalPanel(condition = paste0("output['", ns("is_rpart"), "'] === true"),
    conditionalPanel(condition = paste0("output['", ns("is_tree"), "'] === true"),
    shiny::fluidRow(
      shiny::column(1, 
                    shinyWidgets::dropdownButton(icon = icon("share-alt"),status = "danger",width = 500, circle = T, 
                                                 label = "Update nodes properties", tooltip = TRUE,
                                                 shiny::fluidRow(
                                                   shiny::column(4,
                                                                 shiny::selectInput(ns("shapeY"), "shape Y", 
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
                                                                 
                                                                 shiny::selectInput(ns("shapeX"), "shape X", 
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
                                                                                    selected = "dot")
                                                   ),
                                                   shiny::column(4,
                                                                 shiny::numericInput(ns("nodesSize"), "Nodes size",
                                                                                     value = 22, min = 1),
                                                                 
                                                                 shiny::numericInput(ns("legendNodesSize"), "Legend size",
                                                                                     value = 22, min = 1)
                                                   ),
                                                   shiny::column(4,
                                                                 shiny::numericInput(ns("nodesFontSize"), "Nodes font",
                                                                                     value = 16, min = 1),
                                                                 
                                                                 shiny::numericInput(ns("legendFontSize"), "Legend font",
                                                                                     value = 16, min = 1)
                                                   ),
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
                    shinyWidgets::dropdownButton(icon = icon("exchange"), status = "warning", width = 300, circle = T, 
                                                 label = "Update edges properties", tooltip = TRUE,
                                                 shiny::fluidRow(
                                                   shiny::column(12,
                                                                 colourpicker::colourInput(ns("colorEdges"), "Edges color",
                                                                                           value = "#8181F7")
                                                   ),
                                                   shiny::column(12,
                                                                 shiny::numericInput(ns("edgesFontSize"), "Edges font",
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
                    shinyWidgets::dropdownButton(icon = icon("header"), status = "info", width = 400, circle = T, 
                                                 label = "Set title, subtitle and footer", tooltip = TRUE,
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
                                                                                           "Subtitle color :", value = "black")
                                                   ),
                                                   shiny::column(4,
                                                                 shiny::numericInput(ns("Submainsize"), "Subtitle size :",
                                                                                     value = 12, min = 1)
                                                   ),
                                                   shiny::column(4,
                                                                 textInput(ns("footer"), "Footer :", "")
                                                   ),
                                                   shiny::column(4,
                                                                 colourpicker::colourInput(ns("colourFooterMain"),
                                                                                           "Footer color :", value = "black")
                                                   ),
                                                   shiny::column(4,
                                                                 shiny::numericInput(ns("Footermainsize"), "Footer size :",
                                                                                     value = 12, min = 1))
                                                 )
                    )
      ),
      shiny::column(1, 
                    shinyWidgets::dropdownButton(icon = icon("gear"),status = "success",width = 300,circle = T, 
                                                 label = "Interaction and layout", tooltip = TRUE,
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
      ),
      shiny::column(1, 
                    shinyWidgets::dropdownButton(icon = icon("download"),status = "default", width = 300, circle = T, 
                                                 label = "Download the network as html", tooltip = TRUE,
                                                 shiny:: fluidRow(
                                                   shiny::column(12,
                                                                 shiny::sliderInput(ns("export_height"), "Height:",
                                                                                    min = 200, max = 1400, value = 900),
                                                                 # shinyWidgets::materialSwitch(ns("export_self"),
                                                                 #                              "selfcontained ?", status = "info", value = TRUE),
                                                                 colourpicker::colourInput(ns("export_background"),
                                                                                           "Background color :", value = "white"),
                                                                 shiny::downloadLink(ns('downloadNetwork'), 'Download Tree as html')
                                                   )
                                                 )
                    )
      )
    ),
    
    
                     shiny::uiOutput(ns("treeUI"))
    )
    # )
    
  )
}


#' run tree app
#'
#' @param  data  \code{rpart or data.drame} data in shiny app
#' @param  rpartParams \code{boolean}, add tabs for rpart parameters
#' @param  visTreeParams \code{boolean}, add tabs for visTree parameters
#' @param  id \code{character} id of module
#' @examples
#' \dontrun{
#' treeApp(data = iris)
#' treeApp(data = iris, visTreeParams = FALSE)
#' treeApp(data = rpart(iris), rpartParams = FALSE)
#' treeApp(data = rpart(iris), rpartParams = FALSE, visTreeParams = FALSE)
#' }
#' 
#' @export
treeApp <- function(data, id = "id1", rpartParams = TRUE, visTreeParams = TRUE){
  if("rpart" %in% class(data)){
    if(rpartParams == TRUE){
      stop("data is of class rpart and rpartParams is TRUE, you must pass a data.frame or rpartParams to FALSE")
    }
    return(shiny::shinyApp(ui = shiny::fluidPage(visTreeModuleUI(id = id,
                                                                 rpartParams = rpartParams,
                                                                 visTreeParams = visTreeParams)), 
                           server = function(input, output, session) {
                             shiny::callModule(treeServLigth, id ,data = shiny::reactive(data))
                           }))
  }
  
  if("data.frame" %in% class(data)){
    if(rpartParams == FALSE){
      stop("data is of class data.frame and rpartParams is FALSE, you must pass a rpart or rpartParams to TRUE")
    }
    
    return( shiny::shinyApp(ui = shiny::fluidPage(visTreeModuleUI(id = id,
                                                                  rpartParams = rpartParams,
                                                                  visTreeParams = visTreeParams)), 
                            server = function(input, output, session) {
                              shiny::callModule(treeServ, id ,data = shiny::reactive(data))
                            }))
  }
}
