test_that("that synonyms function works", {
  test <- LCVP("Hibiscus vitifolius", synonyms = TRUE)
  expect_equal(nrow(test), 2)
  expect_equal(as.character(test$Status[2]), "synonym")
})

test_that("that retuning genus names works", {
  test <- LCVP("Hibiscus", genus_tab = TRUE)
  expect_equal(nrow(test), 1502)
  expect_equal(unique(as.character(test$Genus)), "Hibiscus")
  expect_equal(length(unique(as.character(test$Genus))), 1)
})

test_that("that infra-specific names work", {
  test <- LCVP("Hibiscus vitifolius", infra_specific = TRUE)
  expect_equal(nrow(test), 8)
  expect_equal(unique(as.character(test$Genus)), "Hibiscus")
  expect_equal(length(unique(as.character(test$Genus))), 1)
  expect_equal(unique(as.character(test$Species)), "vitifolius")
})