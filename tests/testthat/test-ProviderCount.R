context("ProviderCount")

test_that("works as expected", {
  skip_on_cran()

  data(MyEOLs)
  out <- ProviderCount(MyEOLs[6])

  expect_is(out, "numeric")
  expect_is(out[1], "numeric")
  expect_named(out)
})
