# Structure tests for the static constructor / option-builder API.
# These guard the R -> JS "pass-through": the payload generated for a given
# call must keep carrying the options the user supplied. They are the automated
# version of the regression gallery used when bumping the vis.js bundle.

test_that("visNetwork builds an htmlwidget carrying nodes & edges", {
  w <- visNetwork(demo_nodes(), demo_edges())
  expect_s3_class(w, "htmlwidget")
  expect_s3_class(w, "visNetwork")
  expect_equal(w$x$nodes$id, 1:5)
  expect_equal(nrow(w$x$edges), 4)
  expect_true(w$x$nodesToDataframe)
  expect_true(w$x$edgesToDataframe)
})

test_that("edges pointing to unknown nodes are dropped", {
  nodes <- demo_nodes(3)
  edges <- data.frame(from = c(1, 2, 1), to = c(2, 3, 99)) # last one is orphan
  w <- visNetwork(nodes, edges)
  expect_equal(nrow(w$x$edges), 2)
  expect_false(99 %in% w$x$edges$to)
})

test_that("groups are extracted from nodes$group", {
  nodes <- data.frame(id = 1:4, group = c("A", "B", "A", "B"))
  w <- visNetwork(nodes, demo_edges())
  expect_setequal(unlist(w$x$groups), c("A", "B"))
})

test_that("main / submain / footer accept character and list forms", {
  w1 <- visNetwork(demo_nodes(), demo_edges(), main = "Title")
  expect_equal(w1$x$main$text, "Title")
  expect_true(nzchar(w1$x$main$style)) # default style injected

  w2 <- visNetwork(demo_nodes(), demo_edges(),
                   main = list(text = "T", style = "color:red;"))
  expect_equal(w2$x$main$style, "color:red;")
})

test_that("visNodes / visEdges / visInteraction / visPhysics populate options", {
  w <- visNetwork(demo_nodes(), demo_edges()) |>
    visNodes(shape = "square", color = "tomato") |>
    visEdges(arrows = "to", dashes = TRUE) |>
    visInteraction(dragNodes = FALSE) |>
    visPhysics(solver = "forceAtlas2Based")

  expect_equal(w$x$options$nodes$shape, "square")
  expect_equal(w$x$options$edges$arrows, "to")
  expect_false(w$x$options$interaction$dragNodes)
  expect_equal(w$x$options$physics$solver, "forceAtlas2Based")
})

test_that("visPhysics keeps the wind option (added in vis-network 9.x)", {
  w <- visNetwork(demo_nodes(), demo_edges()) |>
    visPhysics(wind = list(x = 1, y = 0))
  expect_equal(w$x$options$physics$wind, list(x = 1, y = 0))
})

test_that("visOptions enables id selection and selectedBy", {
  nodes <- data.frame(id = 1:6, group = rep(c("A", "B"), 3))
  w <- visNetwork(nodes, demo_edges()) |>
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group")
  expect_true(w$x$idselection$enabled)
  expect_true(w$x$byselection$enabled)
  expect_equal(w$x$byselection$variable, "group")
})

test_that("visHierarchicalLayout sets the layout option", {
  w <- visNetwork(demo_nodes(), demo_edges()) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")
  expect_true(w$x$options$layout$hierarchical$enabled)
  expect_equal(w$x$options$layout$hierarchical$direction, "LR")
})

test_that("image nodes are carried through (per-node, global and per-group)", {
  img <- "https://visjs.org/images/visjs_logo.png"
  broken <- "https://visjs.org/images/broken.png"

  # 1. per-node columns in the nodes data.frame
  nodes <- data.frame(id = 1:2, shape = "image", image = img,
                      stringsAsFactors = FALSE)
  w <- visNetwork(nodes, demo_edges())
  expect_equal(unique(w$x$nodes$shape), "image")
  expect_equal(unique(w$x$nodes$image), img)

  # 2. global default through visNodes (incl. brokenImage fallback)
  w2 <- visNetwork(demo_nodes(), demo_edges()) |>
    visNodes(shape = "circularImage", image = img, brokenImage = broken)
  expect_equal(w2$x$options$nodes$shape, "circularImage")
  expect_equal(w2$x$options$nodes$image, img)
  expect_equal(w2$x$options$nodes$brokenImage, broken)

  # 3. selected / unselected image list form
  w3 <- visNetwork(demo_nodes(), demo_edges()) |>
    visNodes(shape = "image",
             image = list(unselected = img, selected = broken))
  expect_equal(w3$x$options$nodes$image$unselected, img)
  expect_equal(w3$x$options$nodes$image$selected, broken)

  # 4. per-group image shape
  gnodes <- data.frame(id = 1:2, group = c("logo", "logo"))
  w4 <- visNetwork(gnodes, demo_edges()) |>
    visGroups(groupname = "logo", shape = "image", image = img)
  grp <- w4$x$options$groups$logo
  expect_equal(grp$shape, "image")
  expect_equal(grp$image, img)
})

test_that("tibble / data.table-like inputs are coerced to data.frame", {
  skip_if_not_installed("tibble")
  nodes <- tibble::tibble(id = 1:3)
  edges <- tibble::tibble(from = c(1, 2), to = c(2, 3))
  w <- visNetwork(nodes, edges)
  expect_s3_class(w$x$nodes, "data.frame")
  expect_true(w$x$nodesToDataframe)
})
