context("AutofillTaxonNames")

test_that("works as expected", {
  skip_on_cran()

  data(MyHiers)
  dat <- MakeTreeData(MyHiers)
  dat2 <- AutofillTaxonNames(dat)

  expect_is(dat, "data.frame")
  expect_is(dat2, "data.frame")
  expect_equal(dat$Superkingdom, dat2$Superkingdom)
  expect_false(identical(dat$Order, dat2$Order))
})
