# Proxy API tests, driven by what inst/shiny exercises. The proxy functions
# only build a payload and push it through session$sendCustomMessage(); a fake
# session lets us assert the message type and content without a live Shiny app.

test_that("visNetworkProxy returns a proxy object and needs a session", {
  p <- make_proxy("net1")$proxy
  expect_s3_class(p, "visNetwork_Proxy")
  expect_equal(p$id, "net1")
  expect_error(visNetworkProxy("net1", session = NULL), "Shiny")
})

test_that("each proxy verb sends the expected message type and id", {
  cases <- list(
    list(fn = function(p) visFit(p),                        type = "visShinyFit"),
    list(fn = function(p) visFocus(p, id = 1),              type = "visShinyFocus"),
    list(fn = function(p) visUpdateNodes(p, demo_nodes(2)), type = "visShinyUpdateNodes"),
    list(fn = function(p) visUpdateEdges(p, demo_edges()),  type = "visShinyUpdateEdges"),
    list(fn = function(p) visRemoveNodes(p, id = 1),        type = "visShinyRemoveNodes"),
    list(fn = function(p) visRemoveEdges(p, id = "e1"),     type = "visShinyRemoveEdges"),
    list(fn = function(p) visSelectNodes(p, id = 1),        type = "visShinySelectNodes"),
    list(fn = function(p) visSelectEdges(p, id = "e1"),     type = "visShinySelectEdges"),
    list(fn = function(p) visGetNodes(p),                   type = "visShinyGetNodes"),
    list(fn = function(p) visGetEdges(p),                   type = "visShinyGetEdges"),
    list(fn = function(p) visSetSelection(p, nodesId = 1),  type = "visShinySetSelection"),
    list(fn = function(p) visSetTitle(p, main = "hello"),   type = "visShinySetTitle")
  )

  for (case in cases) {
    h <- make_proxy("netX")
    out <- case$fn(h$proxy)
    sent <- h$sent()
    expect_length(sent, 1L)
    expect_equal(sent[[1]]$type, case$type, info = case$type)
    expect_equal(sent[[1]]$message$id, "netX", info = case$type)
    # proxy verbs are chainable: they return the proxy
    expect_s3_class(out, "visNetwork_Proxy")
  }
})

test_that("visUpdateNodes forwards the node payload and options", {
  h <- make_proxy()
  visUpdateNodes(h$proxy, demo_nodes(3), updateOptions = FALSE, legend = TRUE)
  msg <- h$sent()[[1]]$message
  expect_equal(nrow(as.data.frame(msg$nodes)), 3)
  expect_false(msg$updateOptions)
  expect_true(msg$legend)
})

test_that("visGetNodes lets the caller name the shiny input", {
  h <- make_proxy("g")
  visGetNodes(h$proxy, input = "my_nodes")
  msg <- h$sent()[[1]]$message
  expect_equal(msg$input, "my_nodes")
  expect_true(msg$addCoordinates)
})

test_that("option builders run in proxy mode send visShinyOptions", {
  for (build in list(
    function(p) visOptions(p, highlightNearest = TRUE),
    function(p) visNodes(p, color = "red"),
    function(p) visEdges(p, arrows = "to"),
    function(p) visPhysics(p, solver = "repulsion")
  )) {
    h <- make_proxy()
    build(h$proxy)
    sent <- h$sent()
    # option builders push one or more option messages (visOptions also emits a
    # visShinyCustomOptions); contract: at least one visShinyOptions, all
    # messages targeting this network.
    types <- vapply(sent, function(s) s$type, "")
    expect_true("visShinyOptions" %in% types)
    expect_true(all(types %in% c("visShinyOptions", "visShinyCustomOptions")))
    expect_true(all(vapply(sent, function(s) s$message$id, "") == "mynetwork"))
  }
})

test_that("visSetTitle carries the title text", {
  h <- make_proxy()
  visSetTitle(h$proxy, main = "Main", submain = "Sub", footer = "Foot")
  msg <- h$sent()[[1]]$message
  expect_true(any(grepl("Main", unlist(msg), fixed = TRUE)))
})
