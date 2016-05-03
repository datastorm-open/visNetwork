shiny::tabPanel(
  title = "Update options",
  fluidRow(
    column(
      width = 4,
      checkboxInput("highlightNearest", "highlight ?", FALSE),
      selectInput("algorithm", "highlight algoritm ", c("all", "hierarchical")),
      checkboxInput("hover", "highlight when hover ?", FALSE),
      checkboxInput("nodesIdSelection", "nodes Selection", FALSE),
      checkboxInput("selectedby", "Groups Selection", FALSE)
      # selectInput("selectedby", "Group selection :",
      #             c("A", "B", "C"), multiple = TRUE, selected = NULL)
    ),
    column(
      width = 8,
      visNetworkOutput("network_proxy_options", height = "400px")
    )
  ),
  verbatimTextOutput("code_proxy_options")
)

