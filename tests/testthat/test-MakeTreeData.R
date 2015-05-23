context("MakeTreeData")

test_that("works as expected", {
  skip_on_cran()

  data(MyHiers)
  dat <- MakeTreeData(MyHiers)

  expect_is(dat, "data.frame")
  expect_is(dat$Superkingdom, "character")
  expect_equal(dim(dat), c(6, 15))
})
