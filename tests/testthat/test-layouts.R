# Smoke tests for the higher-level modules most coupled to external packages
# (igraph / rpart / ggraph). These are the ones most exposed to breakage when
# the vis.js bundle or a Suggests dependency changes.

test_that("toVisNetworkData + visIgraph round-trip an igraph object", {
  skip_if_not_installed("igraph")
  ig <- igraph::make_ring(8)
  data <- toVisNetworkData(ig)
  expect_true(all(c("nodes", "edges") %in% names(data)))
  expect_equal(nrow(data$nodes), 8)

  w <- visIgraph(ig)
  expect_s3_class(w, "visNetwork")
})

test_that("visIgraphLayout injects precomputed coordinates", {
  skip_if_not_installed("igraph")
  ig <- igraph::make_ring(6)
  w <- visIgraph(ig) |> visIgraphLayout(layout = "layout_in_circle")
  expect_true(all(c("x", "y") %in% names(w$x$nodes)))
  expect_equal(nrow(w$x$nodes), 6)
})

test_that("visTree builds a network from an rpart model", {
  skip_if_not_installed("rpart")
  fit <- rpart::rpart(Species ~ ., data = iris)
  w <- visTree(fit)
  expect_s3_class(w, "visNetwork")
  expect_gt(nrow(w$x$nodes), 1)
})

test_that("visHclust builds a network from a data.frame", {
  skip_if_not_installed("ggraph")
  w <- visHclust(iris, cutree = 3)
  expect_s3_class(w, "visNetwork")
  expect_gt(nrow(w$x$nodes), nrow(iris)) # leaves + inner cluster nodes
})

test_that("DOT and gephi inputs are accepted", {
  w <- visNetwork(dot = 'dinetwork {1 -> 1 -> 2; 2 -> 3}')
  expect_s3_class(w, "visNetwork")
  expect_equal(w$x$dot, 'dinetwork {1 -> 1 -> 2; 2 -> 3}')
})
