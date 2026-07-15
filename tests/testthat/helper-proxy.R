# Helpers shared across tests.

# A fake Shiny session that records every sendCustomMessage() call, so the
# proxy API (visNetworkProxy + visUpdate*/visRemove*/visGet*/visSet*/visFit/...)
# can be tested without a running Shiny app.
make_proxy <- function(id = "mynetwork") {
  sent <- list()
  session <- list(
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
      invisible(NULL)
    }
  )
  proxy <- structure(list(id = id, session = session), class = "visNetwork_Proxy")
  # `sent()` reads back what has been pushed to the fake session
  list(proxy = proxy, sent = function() sent)
}

# small reusable graph
demo_nodes <- function(n = 5) {
  data.frame(id = seq_len(n), label = paste0("N", seq_len(n)),
             stringsAsFactors = FALSE)
}
demo_edges <- function() {
  data.frame(from = c(1, 2, 3, 4), to = c(2, 3, 4, 5))
}
