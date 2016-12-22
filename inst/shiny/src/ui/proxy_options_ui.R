shiny::tabPanel(
  title = "Update options",
  fluidRow(
    column(
      width = 4,
      checkboxInput("highlightNearest", "highlight ?", FALSE),
      sliderInput("deg", "Degree :", min = 1, max = 10, value = 1),
      selectInput("algorithm", "highlight algoritm ", c("all", "hierarchical")),
      checkboxInput("hover", "highlight when hover ?", FALSE),
      sliderInput("opahigh", "Opacity highlight :", min = 0, max = 1, value = 0.5),
      checkboxInput("nodesIdSelection", "nodes Selection", FALSE),
      checkboxInput("selectedby", "Groups Selection", FALSE),
      sliderInput("opasel", "Opacity selection :", min = 0, max = 1, value = 0.5)
    ),
    column(
      width = 8,
      visNetworkOutput("network_proxy_options", height = "400px")
    )
  ),
  verbatimTextOutput("code_proxy_options")
)

