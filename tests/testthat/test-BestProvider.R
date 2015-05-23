context("BestProvider")

test_that("works as expected", {
  skip_on_cran()

  data(MyEOLs)

  out <- BestProvider(MyEOLs[6])
  expect_is(out, "character")
  expect_equal(length(out), 1)

  out2 <- BestProvider(MyEOLs[1])
  expect_is(out2, "character")
  expect_equal(length(out2), 1)
})
