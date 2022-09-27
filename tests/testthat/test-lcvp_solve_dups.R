if (requireNamespace("LCVP", quietly = TRUE)) {
  splist <- sample(apply(LCVP::tab_lcvp[1:100, 2:3], 1, paste, collapse = " "))
  search <- suppressWarnings(lcvp_search(splist))
  x <- data.frame(
    "Species" = search$Output.Taxon,
    "Trait2" = runif(length(splist)),
    "Trait3" = runif(length(splist)),
    "Trait4" = sample(c("a", "b"), length(splist), replace = TRUE),
    "Trait5" = sample(c(TRUE, FALSE), length(splist), replace = TRUE)
  )
  
  x2 <- x
  x2$Species_input <- search$Input.Taxon
  
  test_that("lcvp_solve_dups works", {
    res_ex <- lcvp_solve_dups(x, 1)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 5)
    expect_equal(nrow(res_ex), 79)
  })
  
  test_that("lcvp_solve_dups works for median", {
    res_ex <- lcvp_solve_dups(x, 1, func_numeric = median)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 5)
    expect_equal(nrow(res_ex), 79)
  })
  
  test_that("lcvp_solve_dups works for sample character", {
    res_ex <-
      lcvp_solve_dups(
        x,
        1,
        func_character = function(x) {
          sample(x, 1)
        }
      )
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 5)
    expect_equal(nrow(res_ex), 79)
  })
  
  
  test_that("lcvp_solve_dups works for all logical", {
    res_ex <- lcvp_solve_dups(x, 1, func_logical = all)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 5)
    expect_equal(nrow(res_ex), 79)
  })
  
  
  test_that("lcvp_solve_dups works", {
    res_ex <- lcvp_solve_dups(x2, 1, fixed_cols = ncol(x))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 5)
    expect_equal(nrow(res_ex), 79)
  })
  
  test_that("lcvp_solve_dups output errors for wrong inputs", {
    expect_error(lcvp_solve_dups(1))
    expect_error(lcvp_solve_dups("a"))
    expect_error(lcvp_solve_dups(x2, 10))
    expect_error(lcvp_solve_dups(x2, 1, func_numeric = 10))
    expect_error(lcvp_solve_dups(x2, 1, func_character = 10))
    expect_error(lcvp_solve_dups(x2, 1, func_logical = 10))
  })
  
  
  
}