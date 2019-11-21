#' Visualize Recursive Partitioning and Regression Trees (rpart object)
#' 
#' Visualize Recursive Partitioning and Regression Trees \code{rpart}. Have a look to \link{visTreeEditor} to edity and get back network, or to \link{visTreeModuleServer} to use custom tree module in R
#' 
#' @param object \code{rpart}, rpart object
#' @param data \code{data.frame}, adding mini-graphics in tooltips using \code{sparkline} and \code{tooltipColumns} ?
#' @param tooltipColumns \code{numeric}, indice of columns used in tooltip. All by default.
#' So, we add boxplot / pie focus on sub-population vs all population using \code{sparkline} package. \code{NULL} to disable.
#' @param main Title. See \link{visNetwork}
#' @param submain Subtitle. See \link{visNetwork}
#' @param footer Footer. See \link{visNetwork}
#' @param direction \code{character}, The direction of the hierarchical layout.
#' The available options are: UD, DU, LR, RL. To simplify:
#' up-down, down-up, left-right, right-left. Default UD. See \link{visHierarchicalLayout} 
#' @param nodesPopSize \code{boolean}, nodes sizes depends on population ? Default to FALSE
#' @param fallenLeaves \code{boolean} leaf nodes at the bottom of the graph ? Default to FALSE
#' @param nodesFontSize \code{numeric}, size of labels of nodes. Default to 16
#' @param edgesFontSize \code{numeric}, size of labels of edges Default to 14
#' @param legendFontSize \code{numeric}, size of labels of nodes in legend. Default to 16
#' @param legendNodesSize \code{numeric}, size of nodes in legend. Default to 22
#' @param edgesFontAlign \code{character}, for edges only. Default tp 'horizontal'. Possible options: 'horizontal' (Default),'top','middle','bottom'. See \link{visEdges}  
#' @param colorVar \code{character}, colors to use or \code{data.frame} To set color of variables. 2 columns :
#' \itemize{
#'   \item{"variable"}{ : names of variables}
#'   \item{"color"}{ : colors (in hexa). See examples}
#' }
#' @param colorY if classification tree : \code{character} colors to use or \code{data.frame} 2 columns :
#' \itemize{
#'   \item{"modality"}{ : levels of Y}
#'   \item{"color"}{ : colors (in hexa)}
#' }
#' if regression tree : \code{character}, 2 colors (min and max, in hexa)
#' @param colorEdges \code{character}, color of edges, in hexa. Default to #8181F7
#' @param legend \code{boolean}, add legend ? Default TRUE. \link{visLegend}
#' @param legendWidth \code{numeric}, legend width, between 0 and 1. Default 0.1
#' @param legendNcol \code{numeric}, number of columns in legend. Default 1
#' @param legendPosition \code{character}, one of "left" (Default) or "right"
#' @param highlightNearest \code{list}, Highlight nearest nodes. See \link{visOptions}
#' @param collapse \code{list}, collapse or not using double click on a node ? See \link{visOptions}
#' @param updateShape \code{boolean}, in case of collapse, udpate cluster node shape as terminal node ? Default to TRUE
#' @param tooltipDelay \code{numeric}, delay for tooltips in millisecond. Default 500
#' @param rules \code{boolean}, add rules in tooltips ? Default to TRUE
#' @param simplifyRules \code{boolean}, simplify rules writing
#' @param digits \code{numeric}, number of digits. Default to 3
#' @param height \code{character}, default to "600px"
#' @param width \code{character}, default to "100\%"
#' @param minNodeSize \code{numeric}, in case of \code{nodesPopSize}, minimum size of a node. Defaut to 15. Else, nodes size is minNodeSize + maxNodeSize / 2 
#' @param maxNodeSize \code{numeric}, in case of \code{nodesPopSize}, maximum size of a node. Defaut to 30. Else, nodes size is minNodeSize + maxNodeSize / 2 
#' @param shapeVar \code{character}, shape for variables nodes See \link{visNodes}
#' @param shapeY \code{character}, shape for terminal nodes See \link{visNodes}
#' @param export \code{boolean}, add export button. Default to TRUE
#' 
#' @return a visNetwork object 
#' 
#' @seealso \link{visTreeEditor}, \link{visTreeModuleServer}, \link{visNetworkEditor}
#'
#' @references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
#'
#' @examples
#' 
#' \dontrun{
#' 
#' library(rpart)
#' 
#' # Basic classification tree
#' res <- rpart(Species~., data=iris)
#' visTree(res, data = iris, main = "Iris classification Tree")
#' 
#' # Basic regression tree
#' res <- rpart(Petal.Length~., data=iris)
#' visTree(res, edgesFontSize = 14, nodesFontSize = 16)
#' 
#' # Complex tree
#' data("solder")
#' res <- rpart(Opening~., data = solder, control = rpart.control(cp = 0.00005))
#' visTree(res, data = solder, nodesPopSize = TRUE, minNodeSize = 10, 
#'   maxNodeSize = 30, height = "800px")
#' 
#' # ----- Options
#' res <- rpart(Opening~., data = solder, control = rpart.control(cp = 0.005))
#' 
#' # fallen leaves + align edges label & size
#' visTree(res, fallenLeaves = TRUE, height = "500px", 
#'  edgesFontAlign = "middle", edgesFontSize = 20)
#' 
#' # disable rules in tooltip, and render tooltip faster
#' # enable hover highlight
#' visTree(res, rules = FALSE, tooltipDelay = 0, 
#'  highlightNearest = list(enabled = TRUE, degree = list(from = 50000, to = 0), 
#'  hover = TRUE, algorithm = "hierarchical"))
#' 
#' # Change color with data.frame
#' colorVar <- data.frame(variable = names(solder), 
#'  color = c("#339933", "#b30000","#4747d1","#88cc00", "#9900ff","#247856"))
#'  
#' colorY <- data.frame(modality = unique(solder$Opening), 
#'  color = c("#AA00AA", "#CDAD15", "#213478"))
#' 
#' visTree(res, colorEdges = "#000099", colorVar = colorVar, colorY = colorY)
#' 
#' # Change color with vector
#' visTree(res, colorEdges = "#000099", 
#'     colorVar = substring(rainbow(6), 1, 7), 
#'     colorY = c("blue", "green", "orange"))
#'     
#'     
#'  # Use visNetwork functions to add more options
#' visTree(res) %>% 
#'     visOptions(highlightNearest = TRUE)
#' 
#' 
#' }
#' 
#' @export
#' 
#' @importFrom grDevices hcl
#' @importFrom grDevices colorRamp
#' @importFrom grDevices rgb
#' 
visTree <- function(object,
                    data = NULL,
                    tooltipColumns = if(!is.null(data)){1:ncol(data)} else {NULL},
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
                    height = "600px",
                    width = "100%",
                    export = TRUE){
  
  # controls
  stopifnot("rpart" %in% class(object))
  stopifnot("character" %in% class(direction))
  stopifnot(direction %in% c("UD", "LR", "RL", "DU"))
  stopifnot(length(direction) == 1)
  stopifnot("logical" %in% class(nodesPopSize))
  stopifnot("numeric" %in% class(minNodeSize) | "integer" %in% class(minNodeSize))
  stopifnot("numeric" %in% class(maxNodeSize) | "integer" %in% class(maxNodeSize))
  stopifnot("logical" %in% class(fallenLeaves))
  stopifnot("logical" %in% class(simplifyRules))
  stopifnot("numeric" %in% class(nodesFontSize) | "integer" %in% class(nodesFontSize))
  stopifnot("numeric" %in% class(edgesFontSize) | "integer" %in% class(edgesFontSize))
  stopifnot("numeric" %in% class(legendFontSize) | "integer" %in% class(legendFontSize))
  stopifnot("character" %in% class(edgesFontAlign))
  
  if(!is.null(colorVar)){
    stopifnot(any(c("data.frame", "character") %in% class(colorVar)))
  }
  
  if(!is.null(colorY)){
    if(object$method == "class"){
      stopifnot(any(c("data.frame", "character") %in% class(colorY)))
    }
    if(object$method == "anova"){
      stopifnot("character"%in%class(colorY))
      stopifnot(length(colorY) <= 2)
    }
  }
  
  if(!is.null(colorEdges)){
    stopifnot("character" %in% class(colorEdges))
  }
  
  
  stopifnot("logical" %in% class(legend))
  stopifnot("numeric" %in% class(legendWidth) | "integer" %in% class(legendWidth))
  stopifnot("numeric" %in% class(legendNcol) | "integer" %in% class(legendNcol))
  stopifnot("character" %in% class(legendPosition))
  stopifnot(any(c("logical", "list") %in% class(highlightNearest)))
  stopifnot(any(c("logical", "list") %in% class(collapse)))
  stopifnot("numeric" %in% class(tooltipDelay)| "integer" %in% class(tooltipDelay))
  stopifnot("logical" %in% class(rules))
  stopifnot("numeric" %in% class(digits)| "integer" %in% class(digits))
  stopifnot("character" %in% class(height))
  stopifnot("character" %in% class(width))
  stopifnot("character" %in% class(shapeVar))
  stopifnot("character" %in% class(shapeY))
  
  if(!is.null(tooltipColumns)){
    
    if(class(tooltipColumns) %in% c("character", "factor")){
      tooltipColumns <- which(tooltipColumns %in% colnames(data))
    }
    stopifnot(class(tooltipColumns)[1] %in% c("numeric", "integer"))
    stopifnot(!is.null(data))
    stopifnot(max(tooltipColumns) <= ncol(data))
    
    which_character <- which(sapply(data[, tooltipColumns, drop = FALSE], 
                                    function(x) class(x)[1]) %in% "character")
    if(length(which_character) > 0){
      for(i in tooltipColumns[which_character]){
        data[, i] <- as.factor(data[, i])
      }
    }
  }
  
  if(!is.null(tooltipColumns) | rules){
    if(!requireNamespace("sparkline", quietly = TRUE)){
      stop("'sparkline' package is needed for this function")
    }
  }
  
  # ------------------------------
  # get information from rpart object
  rpartNodesNames <- row.names(object$frame)
  
  infoClass <- NULL
  parentsDec <- list(lapply(rpartNodesNames, function(X)(.parent(as.numeric(X)))))
  infoVar <- object$frame$var
  infoRules <- .vis_give_rules(object)
  detailRules <- .rpart_lists(object)
  colLabels <- attributes(object$terms)$term.labels
  colClass <- attributes(object$terms)$dataClasses
  colClass <- colClass[names(colClass)%in%colLabels]
  
  if(length(rpartNodesNames) > 1){
    rpartHier <- sapply(as.numeric(rpartNodesNames[2:length(rpartNodesNames)]), function(X){
      info <- .parent(X)
      list(info[length(info)-1], info[length(info)], length(info))
    })
    
    from <- unlist(rpartHier[1,])
    to <- unlist(rpartHier[2,])
    level <- c(1, unlist(rpartHier[3,]))
    
    # ------------------------------
    # build edge info (label + tootip)
    edgesLabels <- character(length(to))
    edgesTooltip <- character(length(to))
    lapply(1:length(to), function(i){
      cur_rule <- strsplit(infoRules[paste0("Node", to[i])], ",")[[1]]
      sens <- substr(cur_rule, 1, 1)
      ind_rule <- as.numeric(substr(cur_rule, 2, nchar(cur_rule)))
      rule <- detailRules[[sens]][[ind_rule]]
      operator <- attributes(rule)$compare
      # if(names(operator) %in% names(colClass[which(colClass %in% c("factor", "character", "ordered"))])){
      if(operator %in% "="){
        operator <- NULL
        edgesLabels[i] <<- paste(rule, collapse = ", ")
      }else{
        rule <- round(rule, digits)
        edgesLabels[i] <<- paste(operator, paste(rule, collapse = ", "))
      }
      
      edgesTooltip[i] <<- paste0('<div style="text-align:center;"><b>', names(attr(rule, "compare")),  "</b></div>", 
                                 paste0('<div style="text-align:center;">', operator, rule, "</div>", collapse = ""))
      invisible()
    })
    
    edgesLabelsFull <- edgesLabels
    formatLabels <- function(x){
      ifelse(nchar(x)  > 10, paste0(substr(x, 1, 7), "..."), x)
    }
    edgesLabels <- sapply(edgesLabels, formatLabels)
    
  } else {
    level <- 1
  }
  
  # ------------------------------
  # nodes
  if(length(rpartNodesNames) > 1){
    nodes_pop <- object$frame$n[match(to, rpartNodesNames)]
  } else {
    nodes_pop <- object$frame$n
  }
  nodes_var <- as.character(infoVar)
  nodes_var_color <- nodes_var[nodes_var != "<leaf>"]
  shape <- ifelse(infoVar != "<leaf>", shapeVar, shapeY)
  SortLabel <-  sort(unique(nodes_var_color))
  colorVar <- .generateVarColor(colorVar, nodes_var_color, SortLabel)
  nodes_color <- as.character(colorVar$color[match(nodes_var, colorVar$variable)])
  
  
  # get stats for nodes (mean / variance / proba)
  statsNodes <- NULL
  # Classification TREE
  if(!is.null(attributes(object)$ylevels)){
    infoClass <- attributes(object)$ylevels
    nlevelsClass <- length(infoClass)
    probaClass <- object$frame[,"yval2"]
    effectif <- data.frame(probaClass[,2:(nlevelsClass+1), drop = F])
    probs <- data.frame(probaClass[,(nlevelsClass+2):(ncol(probaClass)-1), drop = F])
    probsHtml <- probs
    for(i in 1:length(infoClass)){
      probsHtml[,i] <- paste0(infoClass[i], " : <b>",
                              round(probsHtml[,i], digits)*100, "%</b>",
                              " (", effectif[,i], ")")
    }
    statsNodes <- apply(probsHtml, 1, function(x){paste0(x, collapse = "<br>")})
  }else{
    # Regression TREE
    varNodes <- round(object$frame$dev/(object$frame$n - 1),digits)
    varNodes[which(varNodes == Inf)] <- NA
    statsNodes <- paste0("Mean : <b>" , round(object$frame$yval,digits),
                         "</b><br>Variance : <b>",varNodes, "</b>")
  }
  
  # ------------------------------
  # Build rules for tooltip
  tooltipRules <- list(NULL)
  if(length(parentsDec[[1]]) > 1){
    for(i in 2:length(parentsDec[[1]])){
      use <- parentsDec[[1]][[i]]
      varDecisions <- nodes_var[match(as.character(use[-length(use)]), rpartNodesNames)]
      decisionsrules <- edgesLabelsFull[match(as.character(use), rpartNodesNames)-1]
      varDecisionBegin <- unique(varDecisions)
      if(simplifyRules){
        filtre <- ifelse(colClass[varDecisions]%in% c("character", "factor", "ordered"), 
                         varDecisions,
                         paste0(varDecisions, substr(decisionsrules, 1 ,1)))
        tabFiltre <- table(filtre) > 1
        if(length(which(tabFiltre))>0){
          filtres <- names(tabFiltre)[which(tabFiltre)]
          filtreOut <- NULL
          for(j in filtres){
            filtreOut <- c(filtreOut, max(which(j== filtre)))
          }
          keeprules <- sort(c(which(!filtre%in%filtres), filtreOut))
          varDecisions <- varDecisions[keeprules]
          decisionsrules <- decisionsrules[keeprules]
        }
        
        filtre <- varDecisions
        varDecisionsOrder <- varDecisions
        tabFiltre <- table(filtre)>1
        if(length(which(tabFiltre))>0){
          filtres <- names(tabFiltre)[which(tabFiltre)]
          for(j in filtres){
            rulesNumSimpl <- decisionsrules[which(varDecisions == j)] 
            down <- which(substr(rulesNumSimpl,1,1) == ">")
            newLib <- paste0("", substr(rulesNumSimpl[down], 4, nchar(rulesNumSimpl[down])),
                             " <= <b>", j, "</b> < ", substr(rulesNumSimpl[-down], 3, 
                                                             nchar(rulesNumSimpl[-down])))
            decisionsrules <- decisionsrules[-which(varDecisions == j)]
            varDecisions <- varDecisions[-which(varDecisions == j)]
            varDecisionsOrder <- varDecisionsOrder[-which(varDecisionsOrder == j)]
            varDecisionsOrder <- c(varDecisionsOrder, j)
            varDecisions <- c(varDecisions, "")
            decisionsrules <- c(decisionsrules, newLib)
          }
        }
        varDecisions <- varDecisions[match(varDecisionBegin, varDecisionsOrder )]
        decisionsrules <- decisionsrules[match(varDecisionBegin, varDecisionsOrder )]
      }
      tooltipRules[[i]] <- paste0(paste("<b>",varDecisions, "</b>", decisionsrules), collapse = "<br>")
    }
  }
  
  # ------------------------------
  # Sparklines for nodes
  labelComplete <- NULL
  if(!is.null(data) & !is.null(tooltipColumns)){

    data <- data[, tooltipColumns, drop = FALSE]

    nodesNames <- as.integer(rownames(object$frame))
    classDtaIn <- unlist(lapply(data, function(X){class(X)[1]}))
    classDtaIn <- classDtaIn%in%c("numeric", "integer")
    dataNum <- data[,classDtaIn, drop = FALSE]
    
    if(ncol(dataNum) > 0){
      minPop <- apply(dataNum, 2, min)
      maxPop <- apply(dataNum, 2, max)
      meanPop <- colMeans(dataNum)
      popSpkl <- apply(dataNum,2, function(X){
        .addSparkLineOnlyJs(X, type = "box")
      })
      
      labelComplete <- sapply(nodesNames, function(Z){
        .giveLabelsFromDfWhichInvisible(subsetRpart(object, dataNum, Z),
                                        popSpkl, minPop, maxPop, meanPop)
      })
    }
  
    dataOthr <- data[,!classDtaIn, drop = FALSE]
    
    if(ncol(dataOthr) > 0){
      popSpkl <- apply(dataOthr,2, function(X){
        Y <- sort(table(X))
        spl <- .addSparkLineOnlyJs(Y , type = "pie", labels = names(Y))
        if(length(Y) > 1){
          Y <- data.frame(Y)
        } else {
          Y <- data.frame(X = names(Y), Freq = Y)
        }
        Y$X <- ifelse(nchar(as.character(Y$X) ) > 9,
                      paste0(substr(Y$X, 1, 8), "..."), as.character(Y$X))
        modP <-  Y$X[length(Y$X)]
        paste0(spl, " On pop. (mode: <b>", modP, "</b>)")
      })
      
      namOrder <- lapply(dataOthr, function(X){
        names(sort(table(X)))
      })
      labelComplete <- paste(labelComplete, sapply(nodesNames, function(Z){
        .giveLabelsFromDfChrInvisible(subsetRpart(object, dataOthr, Z),
                                      popSpkl, namOrder)} ) )
    }
    
    labelComplete <- paste0('<hr class = "rPartvisNetwork">
        <div class ="showOnMe"><div style="text-align:center;"><U style="color:blue;" class = "classActivePointer">Details</U></div>
                            <div class="showMeRpartTTp" style="display:none;margin-top: -15px">
                            ',labelComplete,
                            '</script>',
                            '<script type="text/javascript">',
                            '$(document).ready(function(){
                            $(".showOnMe").click(function(){
                            $(".showMeRpartTTp").toggle();
                            $.sparkline_display_visible();
                            });
                        });</script>','</div></div>')
  }
  
  # ------------------------------
  # Terminal nodes colors
  ind_terminal <- which(nodes_var == "<leaf>")
  if(!is.null(attributes(object)$ylevels)){
    # Classification tree
    listColorY <- .generateYColor(object, colorY, nodes_var, digits = digits, infoClass = infoClass, probs = probs)
    
    colNodClust <- as.character(listColorY$colorY$color[match(listColorY$vardecidedClust, listColorY$colorY$modality)])
    nodes_color[ind_terminal] <- colNodClust[ind_terminal]
    nodes_var[ind_terminal] <- listColorY$vardecidedClust[ind_terminal]
    
  }else{
    # regression tree
    listColorY <- .generateYColor(object, colorY, nodes_var, digits = digits)
    
    # for legend color
    colorMin <-  grDevices::rgb(listColorY$colRamp(0), maxColorValue=255)
    colorMax <-   grDevices::rgb(listColorY$colRamp(1), maxColorValue=255)
    
    # terminal nodes
    nodes_color[ind_terminal] <- listColorY$colorTerm[ind_terminal]
    classTerminal <- round(object$frame$yval, digits)
    nodes_var[ind_terminal] <- listColorY$vardecidedClust[ind_terminal]
    
    # cluster
    colNodClust <- listColorY$colorTerm
  }
  
  if(rules) {
    idToSample <- length(tooltipRules)
    idS <- sapply(1:idToSample, function(X){paste0(sample(LETTERS, 15), collapse = "")})
    idS <- paste0("myIdToDisplay", idS)
    
    # <div onclick="toggle_visibility(\'',idS,'\')">
    #   <U>RULES</U></div><div id="',idS,'">', 
    # tooltipRules,'</div>
      
    finalHtmlRules <-  paste0(
'<hr class = "rPartvisNetwork">
<div class ="showOnMe2"><div style="text-align:center;"><U style="color:blue;" class = "classActivePointer">Rules</U></div>
<div class="showMeRpartTTp2" style="display:none;">
',tooltipRules,
'</script>',
'<script type="text/javascript">',
'$(document).ready(function(){
$(".showOnMe2").click(function(){
$(".showMeRpartTTp2").toggle();
$.sparkline_display_visible();
});
  });</script>','</div></div>

')
  }else{
    finalHtmlRules <- ""
  }
  
  finalNodesTooltip <- paste0(
    '<div style="text-align:center;">', "N : <b>",
    round(object$frame$n/object$frame$n[1],digits)*100, 
    "%</b> (", object$frame$n,")<br>", "Complexity : <b>",
    round(object$frame$complexity, digits),
    "</b><br>", statsNodes,
    ifelse(!unlist(lapply(tooltipRules, is.null)), finalHtmlRules, ""), '</div>',
    labelComplete)
  
  # ------------------------------
  # Nodes size on population
  value = object$frame$n
  if(nodesPopSize){
    minNodeSize = minNodeSize
    maxNodeSize = maxNodeSize
  }else{
    minNodeSize = (minNodeSize + maxNodeSize) / 2
    maxNodeSize = minNodeSize
  }
  
  # ------------------------------
  # Legend
  legendX <- lapply(SortLabel[SortLabel != "<leaf>"], function(x){
    col <- as.character(colorVar$color[match(x, colorVar$variable)])
    list(label = x, color = col, shape = shapeVar, size = legendNodesSize, 
         Leaf = 0, font.size = legendFontSize)
  })
  
  legendY <- lapply(infoClass, function(X){
    # if(is.null(colorY)){
    #   col <- colorTerm[which(infoClass== X)]
    # }else{
    col <- as.character(listColorY$colorY$color[match(X, listColorY$colorY$modality)])
    # }
    list(label = X, color = col, shape = shapeY, size = legendNodesSize, 
         Leaf = 1, font.size = legendFontSize)
  })
  
  legendFinal <- do.call(rbind,(lapply(c(legendX, legendY), data.frame)))
  if(!is.null(legendFinal)){
    legendFinal$id <- 10000:(10000 + (nrow(legendFinal))-1)
  }
  
  # ------------------------------
  # Final data for visNetwork
  nodes <- data.frame(id = as.numeric(rpartNodesNames), label =nodes_var,
                      level = level, color = nodes_color, value = value,
                      shape = shape, title = finalNodesTooltip, fixed = TRUE,
                      colorClust = colNodClust, labelClust = listColorY$vardecidedClust, Leaf = 0,
                      font.size = nodesFontSize, scaling.min = minNodeSize, scaling.max = maxNodeSize)
  
  nodes$Leaf[ind_terminal] <- 1
  if(fallenLeaves){
    nodes$level[which(nodes$shape %in% shapeY)] <- max(nodes$level)
  }
  
  if(length(rpartNodesNames) > 1){
    smooth <- list(enabled = TRUE, type = "cubicBezier", roundness = 0.5)
    edges <- data.frame(id = paste0("edge", 1:length(from)),from = from, to = to, label = edgesLabels, 
                        value = nodes_pop, title = edgesTooltip, color = colorEdges,
                        font.size = edgesFontSize, font.align = edgesFontAlign, smooth = smooth)
  } else {
    edges <- NULL
  }
  
  # ------------------------------
  # Coordinate
  # if(coordinates){
  # rpartcoParams <- list(uniform = TRUE, branch = 0.2, nspace = 0.2, minbranch = 0.3)
  # Xp <- rpart:::rpartco(object, rpartcoParams)$x
  # nodes$x <- Xp * 100
  # nodes$y <- nodes$level * 150
  # nodes$y <- nodes$y - mean(nodes$y)
  # nodes$x <- nodes$x - mean(nodes$x)
  # 
  # intervalPositionX <- max(nodes$x)
  # CorrectPosition <- legendWidth*intervalPositionX
  # nodes$x <- nodes$x + CorrectPosition / 8
  # nodes$x <- nodes$x  / (1 + legendWidth)
  # }
  
  tree <- visNetwork(nodes = nodes, edges = edges, height = height, width = width, main = main,
                     submain = submain, footer = footer) %>% 
    visHierarchicalLayout(direction = direction) %>%
    visOptions(highlightNearest =  highlightNearest, collapse = collapse) %>% 
    visInteraction(tooltipDelay = tooltipDelay,
                   dragNodes = FALSE, selectConnectedEdges = FALSE,
                   tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                      white-space: nowrap;
                      font-family: cursive;font-size:12px;font-color:purple;background-color: #E6E6E6;
                      border-radius: 15px;') %>% 
    visEdges(scaling = list(label = list(enabled = FALSE))) %>%
    visEvents(type = "once", stabilized = "function() { 
        this.setOptions({layout:{hierarchical:false}, physics:{solver:'barnesHut', enabled:true, stabilization : false}, nodes : {physics : false, fixed : true}});
    }")
  
  if(!is.null(legendFinal)){
    tree <- visLegend(tree, addNodes = legendFinal, useGroups = FALSE, enabled = legend,
                      width = legendWidth, ncol = legendNcol, position = legendPosition)
  }
  
  # rajout informations class tree
  tree$x$tree <- list(updateShape = updateShape, shapeVar = shapeVar, 
                      shapeY = shapeY, colorVar = colorVar, colorY = listColorY)
  
  
  if(export){
    tree <- tree%>%visExport()
  }
  if(!is.null(labelComplete) | rules){
    tree <- tree %>% sparkline::spk_add_deps() 
  }
  
  tree
}

.visUpdateTree <- function(graph, updateShape = NULL, shapeVar = NULL, shapeY = NULL){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visFocus with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  tree <- list()
  tree$updateShape <- updateShape
  tree$shapeVar <- shapeVar
  tree$shapeY <- shapeY
  
  data <- list(id = graph$id, tree = tree)
  
  graph$session$sendCustomMessage("visShinyUpdateTree", data)
  
  graph
}

#Legend regression tree gradient color, still in dev
# ' <div style= "background: red;
# background: -webkit-linear-gradient(colorMax,',',colorMin,');
#                        background: -o-linear-gradient(colorMax,',',colorMin,');
#                        background: -moz-linear-gradient(colorMax,',',colorMin,');
#                        background: linear-gradient(colorMax,',',colorMin,');">Test gradient color</div>'
# ,

.parent <- function(x) {
  if (x[1] != 1) {
    c(Recall(if (x %% 2 == 0L) x / 2 else (x - 1) / 2), x)
  } else {
    x
  }
}

.vis_give_rules <- function (object)
{
  frame <- object$frame
  ruleNums <- as.numeric(row.names(frame))
  is.leaf <- (frame$var == "<leaf>")
  frame[!is.leaf, "order"] <- seq_along(which(!is.leaf))
  TF <- as.numeric(row.names(frame))[-1]%%2==0
  ret <- ifelse(TF,
                as.numeric(row.names(frame))[-1]/2,
                (as.numeric(row.names(frame))[-1] - 1)/2)
  ordeR <- frame[as.character(ret),"order"]
  ret <- ifelse(TF, paste0("L", ordeR), paste0("R", ordeR))
  
  ret <- c("No", ret)
  rpartNodesNames <- as.numeric(row.names(frame))
  
  out <- ret
  names(out) <- paste0("Node", rpartNodesNames)
  return(out)
}

.rpart_lists <- function (object)
{
  ff <- object$frame
  n <- nrow(ff)
  if (n == 1L)
    return("root")
  is.leaf <- (ff$var == "<leaf>")
  whichrow <- !is.leaf
  vnames <- ff$var[whichrow]
  index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + (!is.leaf)))
  irow <- index[c(whichrow, FALSE)]
  ncat <- object$splits[irow, 2L]
  lsplit <- rsplit <- list()
  if (any(ncat < 2L)) {
    jrow <- irow[ncat < 2L]
    cutpoint <- object$splits[jrow, 4L]
    temp1 <- (ifelse(ncat < 0, "<", ">="))[ncat < 2L]
    temp2 <- (ifelse(ncat < 0, ">=", "<"))[ncat < 2L]
    lsplit[ncat < 2L] <- cutpoint
    rsplit[ncat < 2L] <- cutpoint
  }
  if (any(ncat > 1L)) {
    xlevels <- attr(object, "xlevels")
    jrow <- seq_along(ncat)[ncat > 1L]
    crow <- object$splits[irow[ncat > 1L], 4L]
    cindex <- (match(vnames, names(xlevels)))[ncat > 1L]
    lsplit[jrow] <- lapply(seq_along(jrow), function(i) xlevels[[cindex[i]]][object$csplit[crow[i],
                                                                                           ] == 1L])
    rsplit[jrow] <- lapply(seq_along(jrow), function(i) xlevels[[cindex[i]]][object$csplit[crow[i],
                                                                                           ] == 3L])
  }
  lsplit <- lapply(seq_along(lsplit), function(i) structure(lsplit[[i]],
                                                            compare = ifelse(ncat[i] < 2L, ifelse(ncat[i] < 0, "<",
                                                                                                  ">="), "=")))
  rsplit <- lapply(seq_along(lsplit), function(i) structure(rsplit[[i]],
                                                            compare = ifelse(ncat[i] < 2L, ifelse(ncat[i] < 0, ">=",
                                                                                                  "<"), "=")))
  names(lsplit) <- vnames
  names(rsplit) <- vnames
  results <- list(L = lsplit, R = rsplit)
  return(results)
}

subsetRpart <- function(tree,data,  node = 1L) {
  wh <- sapply(as.integer(rownames(tree$frame)), .parent)
  wh <- unique(unlist(wh[sapply(wh, function(x) node %in% x)]))
  data[rownames(tree$frame)[tree$where] %in% wh[wh >= node], , drop = FALSE]
}


.generateVarColor <- function(colorVar, nodes_var, SortLabel){
  if(is.null(colorVar)){
    colorVar <- data.frame(variable = unique(nodes_var), color = grDevices::hcl(seq(0, 250, length = length(unique(nodes_var))), l = 80))
  }else{
    if("data.frame" %in% class(colorVar)){
      unused_var <- setdiff(colorVar$variable, setdiff(SortLabel, "<leaf>"))
      if(length(unused_var) > 0){
        colorVar <- colorVar[-which(colorVar$variable %in% unused_var), ]
      }
      miss_var <- setdiff(setdiff(SortLabel, "<leaf>"), colorVar$variable)
      if(length(miss_var) > 0){
        tmp_color <- setdiff(grDevices::hcl(seq(0, 250, length = nrow(colorVar) + length(miss_var)), l = 80), colorVar$color)
        miss_color <- data.frame(variable = miss_var, 
                                 color = tmp_color[1:length(unique(miss_var))])
        colorVar <- rbind.data.frame(colorVar, miss_color)
      }
      
    }else if("character" %in% class(colorVar)){
      colorVar <- data.frame(variable = setdiff(SortLabel, "<leaf>"), 
                             color = rep(colorVar, length(SortLabel))[1:length(setdiff(SortLabel, "<leaf>"))])
    }
  }
  colorVar
}

.generateYColor <- function(object, colorY, nodes_var, digits = 3, infoClass = NULL, probs = NULL){
  
  if(!is.null(attributes(object)$ylevels)){
    if(is.null(infoClass)){
      infoClass <- attributes(object)$ylevels
    }
    if(is.null(probs)){
      probaClass <- object$frame[,"yval2"]
      nlevelsClass <- length(infoClass)
      effectif <- data.frame(probaClass[,2:(nlevelsClass+1), drop = F])
      probs <- data.frame(probaClass[,(nlevelsClass+2):(ncol(probaClass)-1), drop = F])
    }
    
    # Classification tree
    vardecidedClust <- infoClass[apply(probs, 1, which.max)]
    if(is.null(colorY)){
      colorY <- data.frame(modality = unique(infoClass),
                           color = grDevices::hcl(seq(250, 360, length = length(unique(infoClass))), l = 60))
    }else{
      if("data.frame" %in% class(colorY)){
        miss_y <- setdiff(infoClass, colorY$modality)
        if(length(miss_y) > 0){
          miss_color <- data.frame(modality = miss_y, 
                                   color = grDevices::hcl(seq(250, 360, length = length(unique(miss_y))), l = 60))
          colorY <- rbind.data.frame(colorY, miss_color)
        }
      }else if("character" %in% class(colorY)){
        colorY <- data.frame(modality = infoClass, 
                             color = rep(colorY, length(infoClass))[1:length(infoClass)])
      }
    }
    list(colorY = colorY, vardecidedClust = vardecidedClust)
  } else {
    # Regression tree
    vardecidedClust <- round(object$frame$yval, digits)
    
    # palette
    if(length(row.names(object$frame)) > 1){
      meanV <- object$frame$yval-min(object$frame$yval)
      meanV <- meanV/max(meanV)
    } else {
      meanV <- 1
    }
    colRamp <- .creatColorRampY(colorY)
    colorTerm <- grDevices::rgb(colRamp(meanV), maxColorValue=255)
    
    if(is.null(colorY)){
      colorY <- c("#E6E0F8", "#8904B1")
    } else if(length(colorY) > 1){
      colorY <- c(colorY[1],colorY[2])
    } else {
      colorY <- c(NA,colorY[1])
    }
    list(colRamp = colRamp, colorTerm = colorTerm, colorY = colorY, vardecidedClust = vardecidedClust)
  }
}

.creatColorRampY <- function(colorY)
{
  if(is.null(colorY))
  {
    colRamp <- grDevices::colorRamp(c("#E6E0F8", "#8904B1"))
  }else{
    if(length(colorY) > 1){
      colRamp <- grDevices::colorRamp(c(colorY[1],colorY[2]))
    } else {
      colRamp <- grDevices::colorRamp(c(NA,colorY[1]))
    }
  }
  colRamp
}


#' Run and edit a visTree, and get back in R
#'
#' Needed packages : shiny, rpart, colourpicker, shinyWidgets
#' 
#' @param  data  \code{rpart or data.drame}
#' @param  ...  all arguments except \code{object} present in \link{visTreeModuleServer}
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' net <- visTreeEditor(data = iris)
#' net <- visTreeEditor(data = rpart(iris), main = "visTree Editor")
#' net <- visTreeEditor(data = rpart(iris), tooltip_data = iris, 
#'     main = "visTree Editor")
#' net
#' 
#' }
#' 
#' @export
#' 
#' @importFrom  utils packageVersion
#' 
#' @seealso \link{visTree}, \link{visTreeModuleServer}, \link{visNetworkEditor}
#'
#' @references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
#'
visTreeEditor <- function(data, ...){
  
  .ctrlPckTree()
  
  if("rpart" %in% class(data)){
    rpartParams <- FALSE
  } else if("data.frame" %in% class(data)){
    rpartParams <- TRUE
  }
  return(shiny::runApp(shiny::shinyApp(ui = shiny::fluidPage(
    visTreeModuleUI(id = "visTreeEditor", rpartParams = rpartParams, visTreeParams = TRUE, quitButton = TRUE)), 
    server = function(input, output, session) {
      shiny::callModule(visTreeModuleServer, id = "visTreeEditor", data = shiny::reactive(data), ...)
    })))
}


.giveLabelsFromDfWhichInvisible <- function(df, popSpkl = NULL, minPop = NULL, maxPop = NULL, meanPop = NULL){
  df <- df[!is.na(df[,1]),, drop = FALSE]
  clM <- colMeans(df)
  if(!is.null(popSpkl)){
    nm <- names(df)
    re <- list()
    for(i in nm){
      re[[i]] <- paste0("<br>", popSpkl[[i]],' : On pop. (mean:<b>', round(meanPop[i],2),"</b>)","<br>",
                        .addSparkLineOnlyJs(df[,i], type = "box",
                                            min = minPop[[i]], max = maxPop[[i]]),
                        " : On grp. (mean:<b>", round(clM[i], 2),"</b>)")
    }
  }
  re <- unlist(re)
  paste(paste("<br> <b>",names(clM), ": </b>", re, collapse = ""))
  
}


.giveLabelsFromDfChrInvisible <- function(df, popSpkl, namOrder){
  nm <- names(df)
  re <- list()
  for(i in nm){
    tbl <- table(df[,i, drop = FALSE])
    tbl <- tbl[na.omit(match(namOrder[[i]], names(tbl)))]
    if(length(tbl) > 1){
      tbl <- data.frame(tbl)
    } else {
      tbl <- data.frame(Var1 = names(tbl), Freq = tbl)
    }
    newMod <- namOrder[[i]][!namOrder[[i]]%in%tbl$Var1]
    if(length(newMod) > 0){
      tbl <- rbind(tbl, data.frame(Var1 = newMod, Freq = 0))
    }
    namOrder
    tbl$Var1 <- ifelse(nchar(as.character(tbl$Var1) ) > 9, paste0(substr(tbl$Var1, 1, 8), "..."), as.character(tbl$Var1))
    re[[i]] <- paste0(.addSparkLineOnlyJs(tbl$Freq, type = "pie", labels = tbl$Var1), "On grp. (mode:<b>", tbl[which.max(tbl$Freq),]$Var1,"</b>)")
    
  }
  re <- unlist(re)
  paste(paste("<br> <b>",names(re), ": </b><br>", popSpkl, "<br>", re, collapse = ""))
}




#' @importFrom grDevices boxplot.stats
.addSparkLineOnlyJs <- function(vect, min = NULL, max = NULL, type = "line", labels = NULL){
  getboxplotValues <- function(x){
    if(!all(is.na(x)) && length(x) >4){
      x_box <- boxplot.stats(x)
      x_out_range <- ifelse(length(x_box$out)>=2, range(x_box$out),NA)
      return(sort(c(x_box$stats, x_out_range))) 
    } else{
      return(NA)
    }
  }
  
  if(is.null(min))min <- min(vect)
  if(is.null(max))max <- max(vect)
  drun <- sample(LETTERS, 15, replace = TRUE)
  drun <- paste0(drun, collapse = "")
  if(!is.null(labels)){
    tltp <- paste0((1:length(labels))-1, ": '", labels, "'", collapse = ",")
    tltp <- paste0("
                   tooltipFormat: \'{{offset:offset}} ({{percent.1}}%)\',   tooltipValueLookups: {
                   \'offset\': { ", tltp, "}}")
  }else{
    tltp <- NULL
  }
  if(type != "box"){
    ttr <- paste0('
         $(function() {
                  $(".inlinesparkline', drun,'").sparkline([',paste0(vect, collapse = ",") ,'], {
                  type: "',type , '", chartRangeMin: ', min,', chartRangeMax: ', max,'
                  , ', tltp, '
                  }); 
  });
                  ')
  } else {
    vect <- getboxplotValues(vect)
    ttr <- paste0('
         $(function() {
         $(".inlinesparkline', drun,'").sparkline([',paste0(vect, collapse = ",") ,'], {
         type: "',type , '", raw : true, chartRangeMin: ', min,', chartRangeMax: ', max,'
         , ', tltp, '
         }); 
         });
         ')
  }

  paste0('<div class="inlinesparkline', drun,'" style="display: inline-block;">&nbsp;</div>',
         '<script type="text/javascript">',
         ttr,
         '</script>')
}