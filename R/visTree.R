#' Plotting rpart objects
#' 
#' Function to plot rpart objects
#' 
#' @param object \code{rpart}, rpart object
#' @param main \code{character}, Graph title
#' @param direction \code{character}, The direction of the hierarchical layout.
#' The available options are: UD, DU, LR, RL. To simplify:
#' up-down, down-up, left-right, right-left. Default UD
#' @param fallenLeaves \code{boolean} position the leaf nodes at the bottom of the graph ? Default to FALSE
#' @param fontSize \code{numeric}, size of label. Defaut to 11
#' @param fontAlign \code{character}, for edges only. Default tp 'horizontal'. Possible options: 'horizontal' (Defaut),'top','middle','bottom'. The alignment determines how the label is aligned over the edge.
#' @param colorVar \code{data.frame} 2 columns :
#' 'variable' with names of variables X
#' 'color' with colors (in hexa)
#' @param colorMod \code{data.frame} 2 columns :
#' 'modality' with levels of Y
#' 'color' with colors (in hexa)
#' @param colorEdges \code{character} color of edges, in hexa. Default #8181F7
#' @param legend \code{boolean}, add legend ? Default TRUE
#' @param legendWidth \code{numeric}, legend width, between 0 and 1. Default 0.1
#' @param legendNcol \code{numeric}, number of column for legend. Default 1
#' @param highlightNearest \code{boolean}, Highlight nearest nodes. See \link{visOptions}
#' @param collapse \code{boolean}, collapse or not using double click on a node ? See \link{visOptions}
#' @param tooltipDelay \code{numeric}, delay before tooltips 
#' apparition in millisecond. Default 500
#' @param rules \code{boolean}, add rules in tooltips ? Default TRUE
#' @param digits \code{numeric}, number of digits. Defaut to 3
#' @param height \code{character}, defaut to "500px"
#' @param width \code{character}, defaut to "100\%"
#' 
#' @return a visNetwork object 
#' 
#' @examples
#' 
#' \dontrun{
#' library(rpart)
#' 
#' # Basic classification tree
#' res <- rpart(Species~., data=iris)
#' visTree(res)
#' visTree(res, direction = 'LR', main = "Iris classification Tree", fontSize = 15)
#' 
#' # Basic regression tree
#' res <- rpart(Petal.Length~., data=iris)
#' visTree(res)
#' 
#' # disable rules in tooltip, and render faster
#' visTree(res, rules = FALSE, tooltipDelay = 0)
#' 
#' # Complex tree
#' data("solder")
#' res<- rpart(Opening~., data=solder, control = (rpart.control(cp = 0.000005)))
#' visTree(res, height = "800px", fontSize = 15)
#' 
#' # fallen leaves
#' visTree(res, fallenLeaves = TRUE, height = "800px", fontSize = 35, fontAlign = "middle")
#' 
#' # Change color
#' colorVar <- data.frame(variable = names(solder), color = c("#339933", "#b30000","#4747d1",
#'                                                            "#88cc00", "#9900ff","#247856"))
#' 
#' colorMod <- data.frame(modality = unique(solder$Opening), color = 
#'                               c("#AA00AA", "#CDAD15", "#213478"))
#' 
#' visTree(res, colorEdges = "#000099", colorVar = colorVar, 
#'         colorMod = colorMod)
#' 
#' }
#' 
#' @export
#' @importFrom grDevices hcl
#' 
visTree <- function(object,
                     main = "",
                     direction = "UD",
                     fallenLeaves = FALSE,
                     fontSize = 11,
                     fontAlign = "horizontal",
                     colorVar = NULL,
                     colorMod = NULL,
                     colorEdges = "#8181F7",
                     legend = TRUE,
                     legendWidth = 0.1,
                     legendNcol = 1,
                     highlightNearest =  list(enabled = TRUE,
                                              degree = list(from = 50000, to = 0), hover = TRUE,
                                              algorithm = "hierarchical"),
                     collapse = list(enabled = TRUE, fit = TRUE, unselect = TRUE, 
                                     clusterOptions = list(fixed = TRUE, physics = FALSE)),
                     tooltipDelay = 500,
                     rules = TRUE,
                     digits = 3, 
                     height = "500px",
                     width = "100%"){
  
  stopifnot("rpart"%in%class(object))
  
  terminal <- object$frame$var=="<leaf>"
  rowNam <- row.names(object$frame)
  
  tt <- sapply(as.numeric(rowNam[2:length(rowNam)]), function(X){
    re <- .parent(X)
    list(re[length(re)-1], re[length(re)], length(re))
  })
  
  from <- unlist(tt[1,])
  to <- unlist(tt[2,])
  level <- c(1, unlist(tt[3,]))
  clas <- NULL
  parentsDec <- list(lapply(rowNam, function(X)(.parent(as.numeric(X)))))
  res2 <- object$frame$var
  te <- .vis_give_rules(object)
  tt <- .rpart_lists(object)
  vv <- attributes(object$terms)$term.labels
  ww <- attributes(object$terms)$dataClasses
  ww <- ww[names(ww)%in%vv]
  decision <- NULL
  decisionLabels <- NULL
  for(i in 1:length(to)){
    re <- strsplit(te[paste0("Nam", to[i])], ",")[[1]]
    sens <- substr(re, 1, 1)
    num <- as.numeric(substr(re, 2, nchar(re)))
    regl <- tt[[sens]][[num]]
    comp <- attributes(regl)$compare
    if(names(comp)%in%names(ww[which(ww%in%c("factor", "character"))])){
      comp <- ""
    }else{
      regl <- round(regl, digits)
    }
    decision <- c(decision, paste( comp, regl, collapse = ","))
    decisionLabels <- c(decisionLabels,
                        paste0('<div style="text-align:center;">',
                               comp,
                               regl,
                               "</div>",
                               collapse = ""))
  }
  
  decision2 <- decision
  Truncc <- function(VV)ifelse(nchar(VV)>10,paste0(substr(VV, 1, 7), "..."), VV)
  decision <- sapply(decision, Truncc)
  eff <- object$frame$n[match(to,rowNam)]
  vardecided <- ifelse(res2 != "<leaf>",as.character(res2),"Terminal")
  shape <- ifelse(res2 != "<leaf>","dot","square")
  SortLabel <-  sort(unique(vardecided))
  if(is.null(colorVar)){
    color <-grDevices::hcl(seq(0, 250, length = length(unique(vardecided))), l = 80)
    colNod <- color[match(vardecided, SortLabel)]
  }else{
    colNod <- as.character(colorVar$color[match(vardecided,
                                                colorVar$variable)])
  }
  probsend <- NULL
  if(!is.null(attributes(object)$ylevels))
  {
    #Classification TREE
    clas <- attributes(object)$ylevels
    nbClas <- length(clas)
    VAL <- object$frame[,"yval2"]
    effectif <- VAL[,2:(nbClas+1)]
    effectif <- data.frame(effectif)
    probs <- VAL[,(nbClas+2):(ncol(VAL)-1)]
    probs <- data.frame(probs)
    probs2 <- data.frame(probs)
    for(i in 1:length(clas)){
      probs[,i] <- paste0(clas[i], " : <b>",
                          round(probs[,i], digits)*100, "%</b>",
                          " (", effectif[,i], ")")
    }
    totDta <- cbind(effectif, probs)
    probsend <- apply(probs, 1, function(X){paste0(X,
                                                   collapse = "<br>")})
    probsend <- paste('<hr>', probsend)
  }else{
    #Regression TREE
    probsend <- paste0("Mean : <b>" ,
                       round(object$frame$yval,digits),
                       "</b>")
  }
  returndec <- list()
  for(i in 2:length(parentsDec[[1]])){
    use <- parentsDec[[1]][[i]]
    returndec[[i]] <- paste0(paste("<b>",
                                   vardecided[match(as.character(use[-length(use)]),
                                                    rowNam)],
                                   "</b>",decision2[match(as.character(use),
                                                          rowNam)-1]),
                             collapse = "<br>")
  }
  
  terminal <- which(vardecided=="Terminal")
  if(!is.null(attributes(object)$ylevels))
  {
    #Classification tree
    classTerminal <- clas[apply(probs2[which(vardecided=="Terminal"),],1,which.max)]
    vardecided[which(vardecided=="Terminal")] <- classTerminal
    if(is.null(colorMod)){
      colorTerm <- grDevices::hcl(seq(250, 360, length = length(unique(clas))), l = 60)
      colNod[terminal] <- colorTerm[match(classTerminal, clas)]
    }else{
      colNod[terminal] <- as.character(colorMod$color[match(classTerminal,
                                                            colorMod$modality)])
    }
  }else{
    #Regression tree
    # Ynam <- strsplit(as.character(object$call)[2], "~")[[1]][1]
    vardecided[which(vardecided=="Terminal")] <- round(object$frame$yval[which(vardecided=="Terminal")],digits)
    meanV <- object$frame$yval-min(object$frame$yval)
    meanV <- meanV/max(meanV)*(360-180)+180
    colorTerm <- grDevices::hcl(meanV, l = 60)
    colNod[terminal] <- colorTerm[terminal]
  }
  
  if(rules) 
  {
    rul <-  paste0('<hr><div class="rPartvisNetworkTooltipShowhim" style="color:blue;"><U>RULES</U>
                            <div class="rPartvisNetworkTooltipShowme" style="color:black;">',
                   returndec,'
         </div>
         </div>')
  }else{
    rul <- ""
  }
  
  labelsNode <- paste0('<div style="text-align:center;">', "N : <b>",
                       round(object$frame$n/object$frame$n[1],digits)*100
                       , "%</b> (", object$frame$n,")<br>",
                       "Complexity : <b>",
                       round(object$frame$complexity, digits),
                       "</b><br>",
                       probsend,
                       ifelse(!unlist(lapply(returndec, is.null)),
                              rul
                              , ""),
                       "</div>"
  )
  
  legelDvIS <- sapply(SortLabel[SortLabel!="Terminal"], function(X){
    if(is.null(colorVar))
    {
      col <- color[which(SortLabel== X)]
    }else{
      col <- as.character(colorVar$color[match(X, colorVar$variable)])
    }
    list(label = X, color = col, shape = "dot")
  }, simplify = FALSE, USE.NAMES = FALSE)
  
  legendVis2 <- sapply(clas, function(X){
    if(is.null(colorMod))
    {
      col <- colorTerm[which(clas== X)]
    }else{
      col <- as.character(colorMod$color[match(X, colorMod$modality)])
    }
    list(label = X, color = col, shape = "square")
  }, simplify = FALSE, USE.NAMES = FALSE)
  
  legelDvIS <- c(legelDvIS, legendVis2)
  smooth <- list(enabled = TRUE, type = "cubicBezier", roundness = 0.5)
  
  nodes <- data.frame(id = as.numeric(rowNam), label =vardecided,
                      level = level, color = colNod,
                      shape = shape, title = labelsNode, fixed = TRUE) 
  
  if(fallenLeaves){
    nodes$level[which(nodes$shape %in%"square")] <- max(nodes$level)
  }
  
  edges <- data.frame(from = from, to = to, label = decision, value = eff, title = decisionLabels)
  
  visNetwork(nodes = nodes, edges = edges, height = height, width = width, main = main)%>% 
    visHierarchicalLayout(direction = direction) %>%
    visLegend(addNodes = legelDvIS, 
              useGroups = FALSE, enabled = legend,
              width = legendWidth,
              ncol = legendNcol) %>%
    visOptions(highlightNearest =  highlightNearest, collapse = collapse) %>% 
    visInteraction(tooltipDelay = tooltipDelay,
                   dragNodes = FALSE, selectConnectedEdges = FALSE,
                   tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                      white-space: nowrap;
                      font-family: cursive;font-size:12px;font-color:purple;background-color: #E6E6E6;
                      border-radius: 15px;') %>% 
    visEdges(font = list(size = fontSize, align = fontAlign), value = 3, smooth = smooth, color = colorEdges,
             scaling = list(label = list(enabled = FALSE))) %>%
    visNodes(font = list(size = fontSize)) %>%
    visEvents(type = "once", stabilized = "function() { 
        this.setOptions({layout:{hierarchical:false}, physics:{solver:'barnesHut', enabled:true, stabilization : false}, nodes : {physics : false, fixed : true}});
  }")
}

.parent <- function(x) {
  if (x[1] != 1)
    c(Recall(if (x %% 2 == 0L) x / 2 else (x - 1) / 2), x) else x
}

.vis_give_rules <- function (object) 
{
  frame <- object$frame
  ruleNums <- as.numeric(row.names(frame))
  is.leaf <- (frame$var == "<leaf>")
  frame[!is.leaf, "order"] <- seq_along(which(!is.leaf))
  ret <- sapply(as.numeric(row.names(frame))[-1], function(X){
    if (X%%2 == 0) {
      rules <- paste0("L", frame[as.character(X/2), "order"])
    }else{
      rules <- paste0("R", frame[as.character((X - 1)/2), "order"])
    }
    rules
  })
  ret <- c("No", ret)
  rownam <- as.numeric(row.names(frame))
  
  out <- ret
  names(out) <- paste0("Nam", rownam)
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