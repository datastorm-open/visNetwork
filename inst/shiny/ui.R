require(shiny)
require(visNetwork)

shiny::shinyUI(shiny::navbarPage(
  title = "Examples",
  
  source("./src/ui/basic_ui.R", local = TRUE)$value,
  source("./src/ui/options_ui.R", local = TRUE)$value,
  source("./src/ui/manip_ui.R", local = TRUE)$value,
  navbarMenu(
    title = "Use proxy",
    source("./src/ui/proxy_nodes_ui.R", local = TRUE)$value,
    source("./src/ui/proxy_anim_ui.R", local = TRUE)$value,
    source("./src/ui/proxy_update_ui.R", local = TRUE)$value
  )
))
