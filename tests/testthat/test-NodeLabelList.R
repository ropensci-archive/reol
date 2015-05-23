context("NodeLabelList")

test_that("works as expected", {
  skip_on_cran()

  data(MyHiers)
  labels <- NodeLabelList(MyHiers, "all")

  expect_is(labels, "list")
  expect_is(labels[[1]], "character")
  expect_is(labels[[1]][1], "character")
  expect_named(labels)
})

test_that("label param works", {
  skip_on_cran()

  expect_error(NodeLabelList(MyHiers, "stuff"), "not found")
})
