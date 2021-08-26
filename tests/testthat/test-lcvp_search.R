test_that("lcvp_search works for one species", {
  res_ex <- lcvp_search("Hibiscus vitifolius")
  expect_equal(class(res_ex), "data.frame")
  expect_equal(ncol(res_ex), 8)
  expect_equal(nrow(res_ex), 1)
  expect_equal(res_ex$Input.Taxon, "Hibiscus vitifolius L.")
})

test_that("lcvp_search works for one species, fuzzy", {
  expect_warning(
    res_ex <- res_ex <- lcvp_search("Hibiscus vitifoliuse")
  )
  expect_equal(class(res_ex), "data.frame")
  expect_equal(ncol(res_ex), 8)
  expect_equal(nrow(res_ex), 1)
  expect_equal(res_ex$Input.Taxon, "Hibiscus vitifolius L.")
})


test_that("lcvp_search works for one species, fuzzy in genus", {
  res_ex <- lcvp_search("Tibiscus vitifolius", max.distance = 2)
  expect_equal(class(res_ex), "data.frame")
  expect_equal(ncol(res_ex), 8)
  expect_equal(nrow(res_ex), 1)
  expect_equal(res_ex$Input.Taxon, "Hibiscus vitifolius L.")
})


test_that("lcvp_search works for one species with infracategories", {
  res_ex <- lcvp_search("Hibiscus abelmoschus var. betulifolius Mast.")
  expect_equal(class(res_ex), "data.frame")
  expect_equal(ncol(res_ex), 8)
  expect_equal(nrow(res_ex), 1)
  expect_equal(res_ex$Input.Taxon, 
               "Hibiscus abelmoschus var. betulifolius Mast.")
})


test_that("lcvp_search works for multiple species, no fuzzy", {
  res_ex <- lcvp_search(c("Hibiscus abelmoschus var. betulifolius Mast.",
                          "Hibiscus abutiloides Willd.",
                          "Hibiscus aculeatus",
                          "Hibiscus acuminatus",
                          "Hibiscus furcatuis"), max.distance = 0)
  expect_equal(class(res_ex), "data.frame")
  expect_equal(ncol(res_ex), 8)
  expect_equal(nrow(res_ex), 5)
  expect_true(is.na(res_ex$Input.Taxon[5]))
})

test_that("lcvp_search works for multiple species, with fuzzy", {
  expect_warning(res_ex <-
                   lcvp_search(
                     c(
                       "Hibiscus abelmoschus var. betulifolius Mast.",
                       "Hibiscus abutiloides Willd.",
                       "Hibiscus aculeatus",
                       "Hibiscus acuminatus",
                       "Hibiscus furcatuis"
                     ),
                     max.distance = 0.1
                   ))
  expect_equal(class(res_ex), "data.frame")
  expect_equal(ncol(res_ex), 8)
  expect_equal(nrow(res_ex), 5)
  expect_true(!is.na(res_ex$Input.Taxon[5]))
})

test_that("lcvp_search output errors for wrong inputs", {
  expect_error(lcvp_search(1))
  expect_error(lcvp_search("a"))
  expect_error(lcvp_search("\x9a oi"))
  expect_error(lcvp_search(NA))
})

