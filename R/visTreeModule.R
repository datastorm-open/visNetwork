#' Module shiny for visualize and customize a \code{rpart} tree
#'
#' Needed packages : shiny, rpart, colourpicker, shinyWidgets, sparkline
#' 
#' @param  id \code{character} id of module, linked to  \link{visTreeModuleServer}
#' @param  rpartParams \code{boolean}, add tabs for rpart parameters (in case of \code{data.frame} in input)
#' @param  visTreeParams \code{boolean}, add tabs for visTree parameters. Default to TRUE. Force to TRUE if \code{rpartParams}
#' @param  quitButton \code{boolean}, add a button to quit module and get back network in R ?
#' 
#' @param  input  \code{list} shiny input
#' @param  output \code{list}, shiny output
#' @param  session  \code{list}, shiny session
#' @param  data \code{reactive}, a \code{data.frame} or a \code{rpart} result. Must be a reactive object
#' @param  tooltip_data \code{reactive}, a \code{data.frame}. if \code{data} is a \code{rpart}, 
#' data.frame used to build tree in order to plot \code{sparkline}
#' 
#' @inheritParams visTree
#' 
#' 
#' @examples
#' \dontrun{
#' 
#' require(rpart)
#' # simple module editor on rpart
#' data <- iris
#' shiny::shinyApp(ui = shiny::fluidPage(
#' visTreeModuleUI(id = "id1", rpartParams = FALSE, visTreeParams = FALSE)), 
#'  server = function(input, output, session) {
#'  shiny::callModule(visTreeModuleServer, "id1", data = shiny::reactive(rpart(data)))
#' })
#' 
#' # full module editor on rpart + data.frame for sparkline
#' data <- iris
#' shiny::shinyApp(ui = shiny::fluidPage(
#'  visTreeModuleUI(id = "id1", rpartParams = FALSE, visTreeParams = TRUE)), 
#'  server = function(input, output, session) {
#'  shiny::callModule(visTreeModuleServer, "id1", data = shiny::reactive(rpart(data)), 
#'  tooltip_data = data)
#' })
#' 
#' # module on data.frame
#' shiny::shinyApp(ui = shiny::fluidPage(visTreeModuleUI(id = "id1",
#'  rpartParams = TRUE)), 
#'  server = function(input, output, session) {
#'  shiny::callModule(visTreeModuleServer, "id1", data = shiny::reactive(data))
#' })
#' 
#' # multiple modules
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
#'   shiny::callModule(visTreeModuleServer, "id1", data = shiny::reactive(iris))
#'   shiny::callModule(visTreeModuleServer, "id2", data = shiny::reactive(rpart(iris)))
#' })
#' }
#' 
#' @name visNetwork-treeModule
#' 
#' @export
#' 
#' @importFrom  stats as.formula
#' @importFrom grDevices col2rgb
#' 
#' @references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
#'
visTreeModuleServer <- function(input, output, session, data,
                                tooltip_data = NULL,
                                tooltipColumns = "",
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
  if (!shiny::is.reactive(tooltip_data)){
    get_tooltip_data <- shiny::reactive({tooltip_data})
  } else {
    get_tooltip_data <- tooltip_data
  }
  
  if (!shiny::is.reactive(tooltipColumns)){
    get_tooltipColumns <- shiny::reactive({tooltipColumns})
  } else {
    get_tooltipColumns <- tooltipColumns
  }
  
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
  
  # create input
  output$input_legend <- shiny::renderUI({
    legend <- get_legend()
    shiny::isolate({
      shiny::checkboxInput(ns("legend"), "Display legend", value = legend)
    })
  })
  shiny::outputOptions(output, "input_legend", suspendWhenHidden = FALSE)
  
  output$input_fallenLeaves <- shiny::renderUI({
    fallenLeaves <- get_fallenLeaves()
    shiny::isolate({
      shiny::checkboxInput(ns("fallenLeaves"), "Fallen leaves", fallenLeaves)
    })
  })
  shiny::outputOptions(output, "input_fallenLeaves", suspendWhenHidden = FALSE)
  
  output$input_rules <- shiny::renderUI({
    rules <- get_rules()
    shiny::isolate({
      shiny::checkboxInput(ns("rules"),"Display rules", value = rules)
    })
  })
  shiny::outputOptions(output, "input_rules", suspendWhenHidden = FALSE)
  
  output$input_updateShape <- shiny::renderUI({
    updateShape <- get_updateShape()
    shiny::isolate({
      shiny::checkboxInput(ns("updateShape"), "Update shape", value = updateShape)
    })
  })
  shiny::outputOptions(output, "input_updateShape", suspendWhenHidden = FALSE)
  
  output$input_export <- shiny::renderUI({
    export <- get_export()
    shiny::isolate({
      shiny::checkboxInput(ns("export"), "Export", value = export)
    })
  })
  shiny::outputOptions(output, "input_export", suspendWhenHidden = FALSE)
  
  output$input_height <- shiny::renderUI({
    height <- get_height()
    shiny::isolate({
      shiny::numericInput(ns("height"), "Height :", value = height)
    })
  })
  shiny::outputOptions(output, "input_height", suspendWhenHidden = FALSE)
  
  output$input_digits <- shiny::renderUI({
    digits <- get_digits()
    shiny::isolate({
      shiny::numericInput(ns("digits"), "Digits : ", value = digits, min = 0)
    })
  })
  shiny::outputOptions(output, "input_digits", suspendWhenHidden = FALSE)
  
  output$input_tooltipDelay <- shiny::renderUI({
    tooltipDelay <- get_tooltipDelay()
    shiny::isolate({
      shiny::numericInput(ns("tooltipDelay"), "Tooltip delay :", value = tooltipDelay, min = 0, step = 100)
    })
  })
  shiny::outputOptions(output, "input_tooltipDelay", suspendWhenHidden = FALSE)
  
  output$input_legendWidth <- shiny::renderUI({
    legendWidth <- get_legendWidth()
    shiny::isolate({
      shiny::numericInput(ns("legendWidth"), "Width :", value = legendWidth, min = 0, max = 0.5, step = 0.05)
    })
  })
  shiny::outputOptions(output, "input_legendWidth", suspendWhenHidden = FALSE)
  
  output$input_legendNcol <- shiny::renderUI({
    legendNcol <- get_legendNcol()
    shiny::isolate({
      shiny::numericInput(ns("legendNcol"), "Number of columns :", value = legendNcol, min = 1, max = 50, step = 1)
    })
  })
  shiny::outputOptions(output, "input_legendNcol", suspendWhenHidden = FALSE)
  
  output$input_legendPosition <- shiny::renderUI({
    legendPosition <- get_legendPosition()
    shiny::isolate({
      shiny::selectInput(ns("legendPosition"), "Position", choices = c("left", "right"), selected = legendPosition)
    })
  })
  shiny::outputOptions(output, "input_legendPosition", suspendWhenHidden = FALSE)
  
  output$input_shapeY <- shiny::renderUI({
    shapeY <- get_shapeY()
    shiny::isolate({
      shiny::selectInput(ns("shapeY"), "shape Y", choices = c("diamond",
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
    })
  })
  shiny::outputOptions(output, "input_shapeY", suspendWhenHidden = FALSE)
  
  output$input_shapeX <- shiny::renderUI({
    shapeVar <- get_shapeVar()
    shiny::isolate({
      shiny::selectInput(ns("shapeX"), "shape X", choices = c("diamond",
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
    })
  })
  shiny::outputOptions(output, "input_shapeX", suspendWhenHidden = FALSE)
  
  output$input_nodesSize <- shiny::renderUI({
    nodesSize <- (get_maxNodeSize() + get_minNodeSize()) / 2
    shiny::isolate({
      shiny::numericInput(ns("nodesSize"), "Nodes size", value = nodesSize, min = 1)
    })
  })
  shiny::outputOptions(output, "input_nodesSize", suspendWhenHidden = FALSE)
  
  output$input_maxNodeSize <- shiny::renderUI({
    value = get_maxNodeSize()
    shiny::isolate({
      shiny::numericInput(ns("maxNodeSize"), "Max nodes size", value = value, min = 1)
    })
  })
  shiny::outputOptions(output, "input_maxNodeSize", suspendWhenHidden = FALSE)
  
  output$input_minNodeSize <- shiny::renderUI({
    value = get_minNodeSize()
    shiny::isolate({
      shiny::numericInput(ns("minNodeSize"), "Min nodes size", value = value, min = 1)
    })
  })
  shiny::outputOptions(output, "input_minNodeSize", suspendWhenHidden = FALSE)
  
  output$input_legendNodesSize <- shiny::renderUI({
    legendNodesSize <- get_legendNodesSize()
    shiny::isolate({
      shiny::numericInput(ns("legendNodesSize"), "Legend size", value = legendNodesSize, min = 1)
    })
  })
  shiny::outputOptions(output, "input_legendNodesSize", suspendWhenHidden = FALSE)
  
  output$input_nodesFontSize <- shiny::renderUI({
    nodesFontSize <- get_nodesFontSize()
    shiny::isolate({
      shiny::numericInput(ns("nodesFontSize"), "Nodes font", value = nodesFontSize, min = 1)
    })
  })
  shiny::outputOptions(output, "input_nodesFontSize", suspendWhenHidden = FALSE)
  
  output$input_legendFontSize <- shiny::renderUI({
    legendFontSize <- get_legendFontSize()
    shiny::isolate({
      shiny::numericInput(ns("legendFontSize"), "Legend font", value = legendFontSize, min = 1)
    })
  })
  shiny::outputOptions(output, "input_legendFontSize", suspendWhenHidden = FALSE)
  
  output$input_colorEdges <- shiny::renderUI({
    colorEdges <- get_colorEdges()
    shiny::isolate({
      colourpicker::colourInput(ns("colorEdges"), "Edges color", value = colorEdges)
    })
  })
  shiny::outputOptions(output, "input_colorEdges", suspendWhenHidden = FALSE)
  
  output$input_edgesFontSize <- shiny::renderUI({
    edgesFontSize <- get_edgesFontSize()
    shiny::isolate({
      shiny::numericInput(ns("edgesFontSize"), "Edges font", value = edgesFontSize, min = 1)
    })
  })
  shiny::outputOptions(output, "input_edgesFontSize", suspendWhenHidden = FALSE)
  
  output$input_edgesFontAlign <- shiny::renderUI({
    edgesFontAlign <- get_edgesFontAlign()
    shiny::isolate({
      shiny::selectInput(ns("edgesFontAlign"), "Edges font align",
                         choices = c("horizontal", "top", "middle", "bottom"), 
                         selected = edgesFontAlign)
    })
  })
  shiny::outputOptions(output, "input_edgesFontAlign", suspendWhenHidden = FALSE)
  
  output$input_direction <- shiny::renderUI({
    direction <- get_direction()
    shiny::isolate({
      shiny::selectInput(ns("direction"), "Direction :",
                         choices = c("up-down" = "UD", "down-up" = "DU",
                                     "left-right" = "LR", "right-left" = "RL"), 
                         selected = direction)
    })
  })
  shiny::outputOptions(output, "input_direction", suspendWhenHidden = FALSE)
  
  output$input_edgesFontAlign <- shiny::renderUI({
    edgesFontAlign <- get_edgesFontAlign()
    shiny::isolate({
      shiny::selectInput(ns("edgesFontAlign"), "Edges font align",
                         choices = c("horizontal", "top", "middle", "bottom"), 
                         selected = edgesFontAlign)
    })
  })
  shiny::outputOptions(output, "input_edgesFontAlign", suspendWhenHidden = FALSE)
  
  output$input_highlight <- shiny::renderUI({
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
      
      shiny::fluidRow(
        shiny::column(12,
                      shinyWidgets::materialSwitch(ns("highlightNearest"), "highlight nearest", status = "info", value = nearest),
                      shinyWidgets::materialSwitch(ns("highlightHover"), "highlight hover", status = "info", value = hover)
        )
      )
    })
  })
  shiny::outputOptions(output, "input_highlight", suspendWhenHidden = FALSE)
  
  output$input_collapse <- shiny::renderUI({
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
      shinyWidgets::materialSwitch(ns("collapse"), "Collapse", status = "info", value = value)
    })
  })
  shiny::outputOptions(output, "input_collapse", suspendWhenHidden = FALSE)
  
  # update main
  output$input_main <- shiny::renderUI({
    main <- get_main()
    shiny::isolate({
      text <- ""
      color <- "black"
      size <- 20
      if(any(c("character", "factor") %in% class(main))){
        text <- main
      } else {
        if(is.list(main)){
          if(!is.null(main$text)){
            text <- main$text
          }
          
          if(!is.null(main$style)){
            find_size <- gregexpr("(font-size:)[[:digit:]]*", main$style)
            tmp_size <- as.numeric(gsub("font-size:", "", unlist(regmatches(main$style, find_size))))[1]
            if(!is.na(size)){
              size <- tmp_size
            }
            find_color <- gregexpr("(color:#)[[:alnum:]]*", main$style)
            tmp_color <- gsub("color:", "", unlist(regmatches(main$style, find_color)))[1]
            if(!is.na(color)){
              color <- tmp_color
            }
          }
        }
      }
      
      shiny::fluidRow(
        shiny::column(4,
                      shiny::textInput(ns("main"), "Main :", text)
        ),
        shiny::column(4,
                      colourpicker::colourInput(ns("colourMain"),
                                                "Main color :", value = color)
        ),
        shiny::column(4,
                      shiny::numericInput(ns("mainsize"), "Main size :",
                                          value = size, min = 1)
        )
      )
    })
  })
  shiny::outputOptions(output, "input_main", suspendWhenHidden = FALSE)
  
  # update submain
  output$input_submain <- shiny::renderUI({
    submain <- get_submain()
    shiny::isolate({
      text <- ""
      color <- "black"
      size <- 12
      if(any(c("character", "factor") %in% class(submain))){
        text <- submain
      } else {
        if(is.list(submain)){
          if(!is.null(submain$text)){
            text <- submain$text
          }
          
          if(!is.null(submain$style)){
            find_size <- gregexpr("(font-size:)[[:digit:]]*", submain$style)
            tmp_size <- as.numeric(gsub("font-size:", "", unlist(regmatches(submain$style, find_size))))[1]
            if(!is.na(size)){
              size <- tmp_size
            }
            find_color <- gregexpr("(color:#)[[:alnum:]]*", submain$style)
            tmp_color <- gsub("color:", "", unlist(regmatches(submain$style, find_color)))[1]
            if(!is.na(color)){
              color <- tmp_color
            }
          }
        }
      }
      
      shiny::fluidRow(
        shiny::column(4,
                      shiny::textInput(ns("submain"), "Submain :", text)
        ),
        shiny::column(4,
                      colourpicker::colourInput(ns("colourSubMain"),
                                                "Submain color :", value = color)
        ),
        shiny::column(4,
                      shiny::numericInput(ns("Submainsize"), "Submain size :",
                                          value = size, min = 1)
        )
      )
    })
  })
  shiny::outputOptions(output, "input_submain", suspendWhenHidden = FALSE)
  
  # update footer
  output$input_footer <- shiny::renderUI({
    footer <- get_footer()
    shiny::isolate({
      text <- ""
      color <- "black"
      size <- 12
      if(any(c("character", "factor") %in% class(footer))){
        text <- footer
      } else {
        if(is.list(footer)){
          if(!is.null(footer$text)){
            text <- footer$text
          }
          
          if(!is.null(footer$style)){
            find_size <- gregexpr("(font-size:)[[:digit:]]*", footer$style)
            tmp_size <- as.numeric(gsub("font-size:", "", unlist(regmatches(footer$style, find_size))))[1]
            if(!is.na(size)){
              size <- tmp_size
            }
            find_color <- gregexpr("(color:#)[[:alnum:]]*", footer$style)
            tmp_color <- gsub("color:", "", unlist(regmatches(footer$style, find_color)))[1]
            if(!is.na(color)){
              color <- tmp_color
            }
          }
        }
      }
      
      shiny::fluidRow(
        shiny::column(4,
                      shiny::textInput(ns("footer"), "Footer :", text)
        ),
        shiny::column(4,
                      colourpicker::colourInput(ns("colourFooterMain"),
                                                "Footer color :", value = color)
        ),
        shiny::column(4,
                      shiny::numericInput(ns("Footermainsize"), "Footer size :",
                                          value = size, min = 1)
        )
      )
    })
  })
  shiny::outputOptions(output, "input_footer", suspendWhenHidden = FALSE)
  
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
  
  shiny::observe({
    if("data.frame" %in% class(data())){
      choices = 1:ncol(data())
      names(choices) = names(data())
      selected = get_tooltipColumns()
      if(class(selected) %in% c("character", "factor")){
        selected <- which(selected %in% names(data()))
      }
      if(isTRUE(all.equal(selected, ""))){
        selected <- choices
      }
      shiny::isolate({
        shiny::updateSelectInput(session, inputId = "tooltipColumns", choices = choices, selected = selected)
      })
    } else if(!is.null(get_tooltip_data()) && "data.frame" %in% class(get_tooltip_data())){
      choices = 1:ncol(get_tooltip_data())
      names(choices) = names(get_tooltip_data())
      selected = get_tooltipColumns()
      if(class(selected) %in% c("character", "factor")){
        selected <- which(selected %in% names(get_tooltip_data()))
      }
      if(isTRUE(all.equal(selected, ""))){
        selected <- choices
      }
      shiny::isolate({
        shiny::updateSelectInput(session, inputId = "tooltipColumns", choices = choices, selected = selected)
      })
    }
  })
  
  output$is_data_frame <- shiny::reactive({
    "data.frame" %in% class(data()) || (!is.null(get_tooltip_data()) && "data.frame" %in% class(get_tooltip_data()))
  })
  
  shiny::outputOptions(output, "is_data_frame", suspendWhenHidden = FALSE)
  
  # Get rpart from reactive data
  rpart_tree <- shiny::reactive({
    input$runTree
    if("rpart" %in% class(data())){
      data()
    } else {
      if(input$runTree > 0){
        shiny::isolate({
          formule <- paste(input$y, "~", paste0(input$x, collapse = "+")) %>% as.formula()
          rpart::rpart(formule, data = data(), 
                       control = rpart::rpart.control(cp = input$complexity, minsplit = input$minsplit))
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
  
  current_tree_params <- shiny::reactiveValues()
  
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
        
        if("data.frame" %in% class(data())){
          data <- data()
          tooltipColumns <- get_tooltipColumns()
          if(isTRUE(all.equal(tooltipColumns, ""))){
            tooltipColumns <- 1:ncol(data)
          }
        } else {
          if(!is.null(get_tooltip_data()) && "data.frame" %in% class(get_tooltip_data())){
            data <- get_tooltip_data()
            tooltipColumns <- get_tooltipColumns()
            if(isTRUE(all.equal(tooltipColumns, ""))){
              tooltipColumns <- 1:ncol(data)
            }
          } else {
            data <- NULL
            tooltipColumns <- NULL
          }
        }
        
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
            
            tmp_colorY <- updateColorY()
            if("class" %in% res$method & is.data.frame(tmp_colorY)){
              colorY <- tmp_colorY
            } else if("anova" %in% res$method & "character" %in% class(tmp_colorY)){
              colorY <- tmp_colorY
            } 
            
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
            
            if("data.frame" %in% class(data())){
              data <- data()
              
              if(is.null(input$tooltipColumns)){
                tooltipColumns <- NULL
              } else if(length(input$tooltipColumns) == 0){
                tooltipColumns <- NULL
              } else {
                tooltipColumns <- as.numeric(input$tooltipColumns)
              }
            } else {
              if(!is.null(get_tooltip_data()) && "data.frame" %in% class(get_tooltip_data())){
                data <- get_tooltip_data()
                if(is.null(input$tooltipColumns)){
                  tooltipColumns <- NULL
                } else if(length(input$tooltipColumns) == 0){
                  tooltipColumns <- NULL
                } else {
                  tooltipColumns <- as.numeric(input$tooltipColumns)
                }
              } else {
                data <- NULL
                tooltipColumns <- NULL
              }
            }
          }
        }
        
        current_tree_params$rules = rules
        current_tree_params$simplifyRules = simplifyRules
        current_tree_params$legend = legend
        current_tree_params$legendWidth = legendWidth
        current_tree_params$legendPosition = legendPosition
        current_tree_params$legendNcol = legendNcol
        current_tree_params$tooltipDelay = tooltipDelay
        current_tree_params$digits = digits
        current_tree_params$fallenLeaves = fallenLeaves
        current_tree_params$updateShape = updateShape
        current_tree_params$export = export
        current_tree_params$data = data
        current_tree_params$tooltipColumns = tooltipColumns
        
        tree <- visTree(object = res,
                        data = data, 
                        tooltipColumns = tooltipColumns,
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
  updateColorY <- shiny::reactive({
    colorY <- infoColors$colorY$colorY
    if(is.data.frame(colorY)){
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
          paste0(collapse = "") %>%
          HTML()
      }else{
        res <- list()
        res[[1]] <- as.character(shiny::column(6,
                                               colourpicker::colourInput(ns("minY"), "Min y : ", corY[1])))
        res[[2]] <- as.character(shiny::column(6,
                                               colourpicker::colourInput(ns("maxY"), "Max y : ", corY[2])))
        res <- paste0(res, collapse = "") %>%
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
      
      clas <- attributes(shiny::isolate(rpart_tree()))$ylevels
      
      if(!is.null(input$minY) & is.null(clas)){
        coloramp <- .creatColorRampY(c(input$minY, input$maxY))
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
  shiny::observeEvent({!is.null(input$direction)}, {
    # print("update direction")
    visNetworkProxy(ns("tree")) %>%
      visHierarchicalLayout(direction = input$direction)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Update node size
  shiny::observeEvent({
    !is.null(input$minNodeSize) & !is.null(input$maxNodeSize) & input$nodesPopSize
  }, {
    # print("update nodes size")
    minNodeSize <- input$minNodeSize
    maxNodeSize <- input$maxNodeSize
    m <- shiny::isolate(treeBuild())
    if(input$nodesPopSize & !is.null(m)){
      newNodes <- data.frame(id = m$x$nodes$id, scaling.min = minNodeSize, scaling.max = maxNodeSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(newNodes)
    }
  }, ignoreInit = TRUE)
  
  # Update node size
  shiny::observeEvent({!is.null(input$nodesSize) & input$nodesPopSize == FALSE}, {
    nodesSize <- input$nodesSize
    m <- shiny::isolate(treeBuild())
    if(!input$nodesPopSize & !is.null(m)){
      newNodes <- data.frame(id = m$x$nodes$id, scaling.min = nodesSize, scaling.max = nodesSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(newNodes)
    }
  }, ignoreInit = TRUE)
  
  # Update legend node size
  shiny::observeEvent(input$legendNodesSize, {
    legendNodesSize <- input$legendNodesSize
    m <- shiny::isolate(treeBuild())
    if(!is.null(m$x$legend)){
      change_legend_nodes <- data.frame(id = m$x$legend$nodes$id, size = legendNodesSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(change_legend_nodes, legend = TRUE)
    }
  }, ignoreInit = TRUE)
  
  # Update legend font size
  shiny::observeEvent(input$legendFontSize, {
    legendFontSize <- input$legendFontSize
    m <- shiny::isolate(treeBuild())
    if(!is.null(m$x$legend)){
      change_legend_nodes <- data.frame(id = m$x$legend$nodes$id, font.size = legendFontSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(change_legend_nodes, legend = TRUE)
    }
  }, ignoreInit = TRUE)
  
  # Update edges color
  shiny::observeEvent(input$colorEdges, {
    colorEdges <- input$colorEdges
    m <- shiny::isolate(treeBuild())
    if(!is.null(m$x$edges)){
      idEdges <- m$x$edges$id
      newEdges <- data.frame(id = m$x$edges$id, color = colorEdges)
      visNetworkProxy(ns("tree")) %>%
        visUpdateEdges(newEdges)
    }
  }, ignoreInit = TRUE)
  
  # Update edges font size
  shiny::observeEvent(input$edgesFontSize, {
    edgesFontSize <- input$edgesFontSize
    m <- shiny::isolate(treeBuild())
    if(!is.null(m$x$edges)){
      idEdges <- m$x$edges$id
      newEdges <- data.frame(id = m$x$edges$id, font.size = edgesFontSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateEdges(newEdges)
    }
  }, ignoreInit = TRUE)
  
  # Update edges font align
  shiny::observeEvent(input$edgesFontAlign, {
    edgesFontAlign <- input$edgesFontAlign
    m <- shiny::isolate(treeBuild())
    if(!is.null(m$x$edges)){
      idEdges <- m$x$edges$id
      newEdges <- data.frame(id = m$x$edges$id, font.align = edgesFontAlign)
      visNetworkProxy(ns("tree")) %>%
        visUpdateEdges(newEdges)
    }
  }, ignoreInit = TRUE)
  
  # Update nodes font size
  shiny::observeEvent(input$nodesFontSize, {
    nodesFontSize <- input$nodesFontSize
    m <- shiny::isolate(treeBuild())
    if(!is.null(m)){
      newNodes <- data.frame(id = m$x$nodes$id, font.size = nodesFontSize)
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(newNodes)
    }
  }, ignoreInit = TRUE)
  
  # reactive for title
  build_main <- shiny::reactive({
    text <- input$main
    style <- paste0("font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:", 
                    input$mainsize,"px;text-align:center;color:", input$colourMain, ";")
    list(text = text, style = style)
  })
  
  # Update title
  shiny::observeEvent({!is.null(input$main) & !is.null(input$mainsize) & !is.null(input$colourMain)}, {
    # print("update main")
    visNetworkProxy(ns("tree")) %>%
      visSetTitle(main = build_main())
  }, ignoreInit = TRUE)
  
  
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
  shiny::observeEvent({!is.null(input$shapeX) & !is.null(input$shapeY)},{
    shapeX <- input$shapeX
    shapeY <- input$shapeY
    m <- shiny::isolate(treeBuild())
    if(!is.null(m)){
      # nodes
      change_nodes <- data.frame(id = m$x$nodes$id, shape = ifelse(m$x$nodes$Leaf == 0, shapeX, shapeY))
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(change_nodes) %>%
        .visUpdateTree(shapeY = shapeY)
      # legend
      change_legend_nodes <- data.frame(id = m$x$legend$nodes$id, shape = ifelse(m$x$legend$nodes$Leaf == 0, shapeX, shapeY))
      visNetworkProxy(ns("tree")) %>%
        visUpdateNodes(change_legend_nodes, legend = TRUE)
    }
  }, ignoreInit = TRUE)
  
  # Update highlightNearest
  update_highlightNearest <- shiny::reactive({
    list(enabled = input$highlightNearest, degree = list(from = 50000, to = 0),
         hover = input$highlightHover, algorithm = "hierarchical")
    
  })
  
  shiny::observeEvent({!is.null(input$highlightNearest) & !is.null(input$highlightHover)}, {
    visNetworkProxy(ns("tree")) %>%
      visOptions(highlightNearest = list(enabled = input$highlightNearest, degree = list(from = 50000, to = 0),
                                         hover = input$highlightHover, algorithm = "hierarchical"))
  }, ignoreInit = TRUE)
  
  # Update collapse
  update_collapse <- shiny::reactive({
    list(enabled = input$collapse, fit = TRUE, resetHighlight = TRUE, 
         clusterOptions = list(fixed = TRUE, physics= FALSE))
  })
  
  shiny::observeEvent({!is.null(input$collapse)}, {
    visNetworkProxy(ns("tree")) %>%
      visOptions(collapse = list(enabled = input$collapse, fit = TRUE, resetHighlight = TRUE, 
                                 clusterOptions = list(fixed = TRUE, physics= FALSE)))
  }, ignoreInit = TRUE)
  
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
    
    # legend <- ifelse(is.null(input$legend), get_legend(), input$legend)
    # rules <- ifelse(is.null(input$rules), get_rules(), input$rules)
    # tooltipDelay <- ifelse(is.null(input$tooltipDelay), get_tooltipDelay(), input$tooltipDelay)
    # updateShape <- ifelse(is.null(input$updateShape), get_updateShape(), input$updateShape)
    # fallenLeaves <- ifelse(is.null(input$fallenLeaves), get_fallenLeaves(), input$fallenLeaves)
    # digits <- ifelse(is.null(input$digits), get_digits(), input$digits)
    nodesPopSize <- ifelse(is.null(input$nodesPopSize), get_nodesPopSize(), input$nodesPopSize)
    # export <- ifelse(is.null(input$export), get_export(), input$export)
    # simplifyRules <- ifelse(is.null(input$simpRules), get_simplifyRules(), input$simpRules)
    # legendWidth <- ifelse(is.null(input$legendWidth), get_legendWidth(), input$legendWidth)
    # legendNcol <- ifelse(is.null(input$legendNcol), get_legendNcol(), input$legendNcol)
    legendNodesSize <- ifelse(is.null(input$legendNodesSize), get_legendNodesSize(), input$legendNodesSize)
    legendFontSize <- ifelse(is.null(input$legendFontSize), get_legendFontSize(), input$legendFontSize)
    # legendPosition <- ifelse(is.null(input$legendPosition), get_legendPosition(), input$legendPosition)
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
    
    rules <- current_tree_params$rules
    simplifyRules <- current_tree_params$simplifyRules
    legend <- current_tree_params$legend
    legendWidth <- current_tree_params$legendWidth
    legendPosition <- current_tree_params$legendPosition
    legendNcol <- current_tree_params$legendNcol
    tooltipDelay <- current_tree_params$tooltipDelay
    digits <- current_tree_params$digits
    fallenLeaves <- current_tree_params$fallenLeaves
    updateShape <- current_tree_params$updateShape
    export <- current_tree_params$export
    data <- current_tree_params$data
    tooltipColumns <- current_tree_params$tooltipColumns
    
    out <- visTree(object = res,
                   data = data, 
                   tooltipColumns = tooltipColumns,
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
  
  # complexity update
  cp_parameters <- shiny::reactiveValues(min = 0, max = 1,  step = 0.005)
  
  shiny::observeEvent(input$set_cp, {
    shiny::showModal(shiny::modalDialog(
      title = "Complexity parameters",
      shiny::numericInput(ns("cp_min"), "Slider minimum :", shiny::isolate(cp_parameters$min)),
      shiny::numericInput(ns("cp_max"), "Slider maximum :", shiny::isolate(cp_parameters$max)),
      shiny::numericInput(ns("cp_step"), "Slider step :", shiny::isolate(cp_parameters$step)),
      shiny::actionButton(ns("update_cp"), "Update complexity slider"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  shiny::observeEvent(input$update_cp, {
    cp_parameters$min <- input$cp_min
    cp_parameters$max <- input$cp_max
    cp_parameters$step <- input$cp_step
    shiny::updateSliderInput(session, "complexity", min = input$cp_min, max = input$cp_max, step = input$cp_step)
  })
  
  
  # download tree
  output$downloadNetwork <- shiny::downloadHandler(
    filename = function() {
      paste('tree-', Sys.Date(), '.html', sep='')
    },
    content = function(con) {
      out <- build_export_tree()
      
      background <- input$export_background
      #Transform color
      if (grepl("^#", background, perl = TRUE)) {
        bgcol <- col2rgb(background, alpha = TRUE)
        background <- sprintf("rgba(%d,%d,%d,%f)", bgcol[1,1], bgcol[2,1], bgcol[3,1], bgcol[4,1]/255)
      }
      
      
      out  %>% visSave(con, TRUE, background)
    }
  )
  
  # quit and get back network
  shiny::observe({
    if(!is.null(input$quit_btn)){
      if(input$quit_btn > 0){
        shiny::stopApp(build_export_tree())
      }
    }
  })
  
}


#' @rdname visNetwork-treeModule
#' 
#' @export
visTreeModuleUI <- function(id, rpartParams = TRUE, visTreeParams = TRUE, quitButton = FALSE) {
  ns <- shiny::NS(id)
  
  .ctrlPckTree()
  
  if(rpartParams){
    visTreeParams <- TRUE
  }
  
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
                                       shiny::column(5,
                                                     shiny::selectInput(ns("x"), "X :", NULL, multiple = TRUE, selected = NULL, width = "100%")
                                       ),
                                       shiny::column(2,
                                                     shiny::numericInput(ns("minsplit"), "Minsplit : ", value = 20, min = 2)
                                       ),
                                       shiny::column(2,
                                                     shiny::sliderInput(ns("complexity"), "Complexity (cp) :",
                                                                        min = 0, max = 1, value = 0.005, step = 0.005)
                                       ),
                                       shiny::column(1,
                                                     shiny::br(), shiny::br(), shiny::actionButton(ns("set_cp"), "Set cp slider")
                                       )
                                       
                            )
                          )
          )
        }else{shiny::div(style = "visibility: hidden")},
        
        if(visTreeParams){
          shiny::tabPanel("visTree options",
                          #Params graph
                          shiny::fluidRow(
                            shiny::column(1, 
                                          shiny::uiOutput(ns("input_height"))
                            ),
                            shiny::column(1,
                                          shiny::uiOutput(ns("input_digits"))
                            ),
                            shiny::column(1, 
                                          shiny::uiOutput(ns("input_tooltipDelay"))
                            ),
                            shiny::column(2,
                                          shiny::br(), shiny::uiOutput(ns("input_fallenLeaves"))
                            ),
                            shiny::column(2,
                                          shiny::br(), 
                                          shiny::uiOutput(ns("input_updateShape"))
                            ),
                            shiny::column(2,
                                          shiny::br(), 
                                          shiny::uiOutput(ns("input_rules"))
                            ),
                            shiny::conditionalPanel(paste0("input['",ns("rules"),"'] == true"),
                                                    shiny::column(2,
                                                                  shiny::br(), 
                                                                  shiny::checkboxInput(ns("simpRules"),"Simplify rules", value = TRUE))
                            ),
                            shiny::column(1,
                                          shiny::br(), 
                                          shiny::uiOutput(ns("input_export"))
                            )
                          ),
                          
                          shiny::conditionalPanel(paste0("output['",ns("is_data_frame"),"'] === true"),
                                                  shiny::fluidRow(
                                                    shiny::column(12, 
                                                                  shiny::selectInput(ns("tooltipColumns"), "tooltipColumns :", NULL, multiple = TRUE, selected = NULL, width = "100%")
                                                    )
                                                  )
                          )
          )
        }else{shiny::div(style = "visibility: hidden")},
        
        if(visTreeParams){
          shiny::tabPanel("Legend",
                          shiny::fluidRow(
                            shiny::column(2, offset = 1, 
                                          shiny::br(), 
                                          shiny::uiOutput(ns("input_legend"))
                            ),
                            # shiny::conditionalPanel(paste0("input['",ns("legend"),"'] == true"),
                            shiny::column(2,
                                          shiny::uiOutput(ns("input_legendWidth"))
                            ),
                            shiny::column(2,
                                          shiny::uiOutput(ns("input_legendNcol"))
                            ),
                            shiny::column(2,
                                          shiny::uiOutput(ns("input_legendPosition"))
                            )
                          )
          )
        }else{shiny::div(style = "visibility: hidden")}
        
      )
    },
    
    if(visTreeParams | rpartParams){
      if(quitButton){
        shiny::fluidRow(
          shiny::column(width = 3, offset = 3, 
                        shiny::div(
                          shiny::actionButton(ns("runTree"), "Run / update tree", class = "btnruntree"), align = "center"
                        )
          ),
          shiny::column(width = 3,
                        shiny::div(
                          shiny::actionButton(ns("quit_btn"), "Quit and get back network in R"), align = "center"
                        )
          )
        )
      } else {
        shiny::div(
          shiny::actionButton(ns("runTree"), "Run / update tree", class = "btnruntree"), align = "center"
        )
      }
    }else{shiny::div(style = "visibility: hidden")},
    
    shiny::hr(),
    
    shiny::conditionalPanel(condition = paste0("output['", ns("is_tree"), "'] === true"),
                            shiny::fluidRow(
                              shiny::column(1, 
                                            shinyWidgets::dropdownButton(icon = shiny::icon("share-alt"),status = "danger",width = 500, circle = T, 
                                                                         label = "Update nodes properties", tooltip = TRUE,
                                                                         shiny::fluidRow(
                                                                           shiny::column(4,
                                                                                         shiny::uiOutput(ns("input_shapeY")),
                                                                                         shiny::uiOutput(ns("input_shapeX"))
                                                                           ),
                                                                           shiny::column(4,
                                                                                         shiny::uiOutput(ns("input_nodesSize")),
                                                                                         shiny::uiOutput(ns("input_legendNodesSize"))
                                                                           ),
                                                                           shiny::column(4,
                                                                                         shiny::uiOutput(ns("input_nodesFontSize")),
                                                                                         shiny::uiOutput(ns("input_legendFontSize"))
                                                                           )
                                                                         ),
                                                                         shiny::column(12,
                                                                                       shiny::br(),
                                                                                       shinyWidgets::materialSwitch(ns("nodesPopSize"), 
                                                                                                                    "Nodes size use population", status = "info"),
                                                                                       shiny::conditionalPanel(paste0("input['", ns("nodesPopSize"),"'] == true"),
                                                                                                               shiny::column(6,
                                                                                                                             shiny::uiOutput(ns("input_minNodeSize"))
                                                                                                               ),                          
                                                                                                               shiny::column(6,
                                                                                                                             shiny::uiOutput(ns("input_maxNodeSize"))
                                                                                                               )
                                                                                                               
                                                                                       ),
                                                                                       
                                                                                       shiny::br(), 
                                                                                       shinyWidgets::materialSwitch(ns("usecolornodes"),"Color nodes", status = "info"),
                                                                                       
                                                                                       shiny::fluidRow(
                                                                                         shiny::uiOutput(ns("colornodes"))
                                                                                       ),
                                                                                       
                                                                                       shiny::br(), 
                                                                                       shinyWidgets::materialSwitch(ns("usecolorY"),"Color Y", status = "info"),
                                                                                       
                                                                                       shiny::uiOutput(ns("colorY"))
                                                                         )
                                            )
                              ),
                              
                              shiny::column(1,
                                            shinyWidgets::dropdownButton(icon = shiny::icon("exchange"), status = "warning", width = 300, circle = T, 
                                                                         label = "Update edges properties", tooltip = TRUE,
                                                                         shiny::fluidRow(
                                                                           shiny::column(12,
                                                                                         shiny::uiOutput(ns("input_colorEdges")),
                                                                                         shiny::uiOutput(ns("input_edgesFontSize")),
                                                                                         shiny::uiOutput(ns("input_edgesFontAlign"))
                                                                           )
                                                                         )
                                            )
                              ),
                              
                              shiny::column(1, 
                                            shinyWidgets::dropdownButton(icon = shiny::icon("header"), status = "info", width = 400, circle = T, 
                                                                         label = "Set title, subtitle and footer", tooltip = TRUE,
                                                                         shiny::fluidRow(
                                                                           shiny::column(12,
                                                                                         shiny::uiOutput(ns("input_main")),
                                                                                         shiny::uiOutput(ns("input_submain")),
                                                                                         shiny::uiOutput(ns("input_footer"))
                                                                           )
                                                                         )
                                            )
                              ),
                              shiny::column(1, 
                                            shinyWidgets::dropdownButton(icon = shiny::icon("gear"),status = "success",width = 300,circle = T, 
                                                                         label = "Interaction and layout", tooltip = TRUE,
                                                                         shiny:: fluidRow(
                                                                           shiny::column(12,
                                                                                         shiny::uiOutput(ns("input_highlight")),
                                                                                         shiny::uiOutput(ns("input_collapse")),
                                                                                         shiny::uiOutput(ns("input_direction"))
                                                                           )
                                                                         )
                                            )
                              ),
                              shiny::column(1, 
                                            shinyWidgets::dropdownButton(icon = shiny::icon("download"),status = "default", width = 300, circle = T, 
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
  )
}


.ctrlPckTree <- function(){
  miss_packages <- c()
  
  if(!requireNamespace("shiny", quietly = TRUE)){
    miss_packages <- c(miss_packages, "'shiny (>1.0.0)'")
  }
  
  if(!requireNamespace("colourpicker", quietly = TRUE)){
    miss_packages <- c(miss_packages, "'colourpicker'")
  }
  
  if(!requireNamespace("shinyWidgets", quietly = TRUE)){
    miss_packages <- c(miss_packages, "'shinyWidgets'")
  }
  
  if(!requireNamespace("rpart", quietly = TRUE)){
    miss_packages <- c(miss_packages, "'rpart'")
  }
  
  if(!requireNamespace("sparkline", quietly = TRUE)){
    miss_packages <- c(miss_packages, "'sparkline'")
  }
  
  if(length(miss_packages) == 1){
    stop(miss_packages," package is needed for this visTreeModule / visTreeEditor", call. = FALSE)
  } else if(length(miss_packages) > 1){
    stop(paste(miss_packages, collapse = ", ")," packages are needed for visTreeModule / visTreeEditor", call. = FALSE)
  }
  
  invisible(NULL)
}
