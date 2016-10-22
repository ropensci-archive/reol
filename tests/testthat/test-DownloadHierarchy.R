context("DownloadHierarchy")

test_that("works as expected", {
  skip_on_cran()

  library(XML)

  data(MyEOLs)
  out <- DownloadHierarchy(MyEOLs[6], FALSE, database = "NCBI Taxonomy", verbose = FALSE)

  expect_is(out, "list")
  expect_is(out$hier51345859, "character")
  expect_is(XML::xmlParse(out$hier51345859), "XMLInternalDocument")
  expect_named(out, "hier51345859")
})

test_that("database param works", {
  skip_on_cran()

  out2 <- DownloadHierarchy(MyEOLs, FALSE, "NCBI Taxonomy", verbose = FALSE)
  expect_is(out2, "list")
  expect_is(out2$hier51377077, "character")
  expect_is(XML::xmlParse(out2$hier51377070), "XMLInternalDocument")
  expect_equal(length(out2), 6)
})
