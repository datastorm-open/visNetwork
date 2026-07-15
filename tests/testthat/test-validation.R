# Error / input-validation paths. Cheap and high value: they pin the contract
# of the public constructors.

test_that("visNetwork requires some data", {
  expect_error(visNetwork(), "dot.*gephi.*nodes.*edges|Must")
})

test_that("node ids must be unique", {
  nodes <- data.frame(id = c(1, 2, 2))
  expect_error(visNetwork(nodes, demo_edges()), "unique")
})

test_that("nodes / edges must be data.frame or list", {
  expect_error(visNetwork(nodes = 1:3, edges = demo_edges()),
               "data.frame or a list")
})

test_that("invalid main / submain / footer are rejected", {
  expect_error(visNetwork(demo_nodes(), demo_edges(), main = list(foo = "x")),
               "Invalid 'main'")
  expect_error(visNetwork(demo_nodes(), demo_edges(), submain = list(style = "x")),
               "text")
})

test_that("visPhysics rejects an unknown solver", {
  expect_error(
    visNetwork(demo_nodes(), demo_edges()) |> visPhysics(solver = "notASolver"),
    "Invalid"
  )
})

test_that("visPhysics rejects combining several force models", {
  expect_error(
    visNetwork(demo_nodes(), demo_edges()) |>
      visPhysics(barnesHut = list(gravitationalConstant = -1),
                 repulsion = list(nodeDistance = 100)),
    "just use one"
  )
})

test_that("visPhysics still accepts a single force model", {
  expect_no_error(
    visNetwork(demo_nodes(), demo_edges()) |>
      visPhysics(barnesHut = list(gravitationalConstant = -1))
  )
})

test_that("proxy-only functions error on a plain visNetwork object", {
  w <- visNetwork(demo_nodes(), demo_edges())
  expect_error(visFit(w), "visNetworkProxy")
  expect_error(visUpdateNodes(w, demo_nodes()), "visNetworkProxy")
  expect_error(visGetNodes(w), "visNetworkProxy")
})
