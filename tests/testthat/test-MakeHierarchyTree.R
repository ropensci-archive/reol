context("MakeHierarchyTree")

test_that("works as expected", {
  skip_on_cran()

  data(MyHiers)
  tree <- MakeHierarchyTree(MyHiers, includeNodeLabels = TRUE)

  expect_is(tree, "phylo")
  expect_is(tree$edge, "matrix")
  expect_is(tree$tip.label, "character")
  expect_named(tree)
})

test_that("includeNodeLabels param works", {
  skip_on_cran()

  tree <- MakeHierarchyTree(MyHiers, includeNodeLabels = FALSE)
  expect_named(tree, c("edge", "tip.label", "Nnode"))
})

test_that("userRanks param works", {
  skip_on_cran()

  tree <- MakeHierarchyTree(MyHiers, userRanks = c("Phylum", "Class", "Species"))

  expect_is(tree, "phylo")
})
