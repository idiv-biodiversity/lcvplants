test_that("perfect matching a single species works", {
  test <- LCVP("Hibiscus vitifolius")
  
  expect_equal(nrow(test), 1)
  expect_equal(as.character(test$Status), "valid")
  expect_equal(as.character(test$LCVP_Accepted_Taxon), "Hibiscus vitifolius L.")
})


test_that("perfect matching with authorities works", {
  test <- LCVP("Hibiscus abelmoschus var. betulifolius Mast.")
  
  expect_equal(nrow(test), 1)
  expect_equal(as.character(test$Status), "synonym")
  expect_equal(as.character(test$Infraspecies), "betulifolius")
  expect_equal(as.character(test$LCVP_Accepted_Taxon), "Abelmoschus moschatus Medik.")
})

test_that("perfect matching with multiple species works", {
  test <- LCVP(c("Hibiscus abelmoschus var. betulifolius Mast.", 
                "Hibiscus abutiloides Willd.", 
                "Hibiscus aculeatus", 
                "Hibiscus acuminatus"), max.cores = 1)
  
  expect_equal(nrow(test), 4)
  expect_equal(as.character(test$Status), c("synonym", "synonym", "valid", "synonym"))
})

test_that("fuzzy matching works", {
  test <- LCVP("Hibiscus vitifolios", max.distance = 1)
  
  expect_equal(nrow(test), 1)
  expect_equal(as.character(test$LCVP_Accepted_Taxon), "Hibiscus vitifolius L.")
  expect_equal(as.character(test$Score), "misspelling: Epithet")
  
  test <- LCVP("Hibiscus vitifolios", max.distance = 2)
  
  expect_equal(nrow(test), 1)
  expect_equal(as.character(test$LCVP_Accepted_Taxon), "Hibiscus vitifolius L.")
  expect_equal(as.character(test$Score), "misspelling: Epithet")
  
  test <- LCVP("Hibiscus acetosulla", max.distance = 5)
  
  expect_equal(nrow(test), 2)
  expect_equal(as.character(test$LCVP_Accepted_Taxon), c("Hibiscus acetosella Welw. ex Hiern", "Hibiscus acicularis Standl."))
  expect_equal(as.character(test$Score), rep("misspelling: Epithet",2))
  expect_equal(test$Substitution, c(1,2))
})

test_that("fuzzy matching of genus works", {
  test <- LCVP("Hubiscus vitifolius", max.distance = 1, genus_search = TRUE)
  
  expect_equal(nrow(test), 1)
  expect_equal(as.character(test$LCVP_Accepted_Taxon), "Hibiscus vitifolius L.")
  expect_equal(as.character(test$Score), "misspelling: Genus")
})

