shiny::tabPanel(
  title = "Update nodes/edges data",
  fluidRow(
    column(
      width = 4,
      checkboxInput("highlightNearest", "highlight Nearest", FALSE),
      checkboxInput("nodesIdSelection", "nodes Selection", FALSE),
      selectInput("selectedby", "Group selection :",
                  c("A", "B", "C"), multiple = TRUE, selected = NULL)
    ),
    column(
      width = 8,
      visNetworkOutput("network_proxy_options", height = "400px")
    )
  ),
  verbatimTextOutput("code_proxy_options")
)

