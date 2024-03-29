if (requireNamespace("LCVP", quietly = TRUE)) {
  test_that("lcvp_search works for one species", {
    expect_warning(res_ex <- lcvp_search("Hibiscus vitifolius"))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 14)
    expect_equal(nrow(res_ex), 1)
    input.taxon <- paste(res_ex[, c(3, 4, 7)], collapse = " ")
    expect_equal(input.taxon, "Hibiscus vitifolius L.")
  })
  
  test_that("lcvp_search works for one species, fuzzy", {
    expect_warning(res_ex <-
                     res_ex <- lcvp_search("Hibiscus vitifoliuse"))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 14)
    expect_equal(nrow(res_ex), 1)
    input.taxon <- paste(res_ex[, c(3, 4, 7)], collapse = " ")
    expect_equal(input.taxon, "Hibiscus vitifolius L.")
  })
  
  
  test_that("lcvp_search works for one species, fuzzy in genus", {
    expect_warning(res_ex <- lcvp_search("Tibiscus vitifolius", 
                                         max_distance = 2,
                                         genus_fuzzy = TRUE))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 14)
    expect_equal(nrow(res_ex), 1)
    input.taxon <- paste(res_ex[, c(3, 4, 7)], collapse = " ")
    expect_equal(input.taxon, "Hibiscus vitifolius L.")
  })
  
  
  test_that("lcvp_search works for one species with infracategories", {
    res_ex <-
      lcvp_search("Hibiscus abelmoschus var. betulifolius Mast.")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 14)
    expect_equal(nrow(res_ex), 1)
    input.taxon <- paste(res_ex[, 3:7], collapse = " ")
    expect_equal(input.taxon,
                 "Hibiscus abelmoschus var. betulifolius Mast.")
  })
  
  
  test_that("lcvp_search works for multiple species, no fuzzy", {
    expect_warning(res_ex <-
      lcvp_search(
        c(
          "Hibiscus abelmoschus var. betulifolius Mast.",
          "Hibiscus abutiloides Willd.",
          "Hibiscus aculeatus",
          "Hibiscus acuminatus",
          "Hibiscus furcatuis"
        ),
        max_distance = 0
      ))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 14)
    expect_equal(nrow(res_ex), 5)
    expect_true(is.na(res_ex[5, 2]))
  })
  
  test_that("lcvp_search works for multiple species, with fuzzy", {
    sps <- c(
      "Hibiscus abelmoschus var. betulifolius Mast.",
      "Hibiscus abutiloides Willd.",
      "Hibiscus aculeatus",
      "Hibiscus acuminatus"
    )
    expect_warning(res_ex <- lcvp_search(sps, max_distance = 0.1))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 14)
    expect_equal(nrow(res_ex), 4)
  })
  
  test_that("lcvp_search output errors for wrong inputs", {
    expect_error(lcvp_search(1))
    expect_error(lcvp_search("a"))
    expect_error(lcvp_search("\x9a oi"))
    expect_error(lcvp_search(NA))
  })
  
}
