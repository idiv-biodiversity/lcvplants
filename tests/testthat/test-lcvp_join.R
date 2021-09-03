if (requireNamespace("LCVP", quietly = TRUE)) {
  # data.frame1
  splist1 <- sample(LCVP::tab_lcvp$Input.Taxon[2:97])
  x <-
    data.frame("Species" = splist1, "Trait1" = runif(length(splist1)))
  
  # data.frame2
  splist2 <- sample(LCVP::tab_lcvp$Input.Taxon[98:8])
  n2 <- length(splist2)
  y <- data.frame(
    "Species" = splist2,
    "Trait2" = runif(n2),
    "Trait3" = runif(n2),
    "Trait4" = sample(c("a", "b"), n2, replace = TRUE),
    "Trait5" = sample(c(TRUE, FALSE), n2, replace = TRUE),
    "Trait6" = as.factor(sample(c("Hey", "Ho"),
                                n2, replace = TRUE))
  )
  
  
  test_that("lcvp_join works, type = full", {
    res_ex <- lcvp_join(x, y, c("Species", "Species"), type = "full")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 10)
    expect_equal(nrow(res_ex), 97)
  })
  
  test_that("lcvp_join works, type = full & solve_duplicated = TRUE", {
    res_ex <- lcvp_join(x,
                        y,
                        c("Species", "Species"),
                        type = "full",
                        solve_duplicated = TRUE)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 10)
    expect_equal(nrow(res_ex), 75)
  })
  
  
  test_that("lcvp_join works, type = left", {
    res_ex <- lcvp_join(x, y, c("Species", "Species"), type = "left")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 10)
    expect_equal(nrow(res_ex), 96)
  })
  
  test_that("lcvp_join works, type = right", {
    res_ex <- lcvp_join(x, y, c("Species", "Species"), type = "right")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 10)
    expect_equal(nrow(res_ex), 91)
  })
  
  test_that("lcvp_join works, type = inner", {
    res_ex <- lcvp_join(x, y, c("Species", "Species"), type = "inner")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 10)
    expect_equal(nrow(res_ex), 90)
  })
  
  test_that("lcvp_join output errors for wrong inputs", {
    expect_error(lcvp_join(1))
    expect_error(lcvp_join(x, "b"))
    expect_error(lcvp_join(x, y, sp_columns = c("Species", "Hey")))
    expect_error(lcvp_join(
      x,
      y,
      sp_columns = c("Species", "Species"),
      type = "Hey"
    ))
  })
  
}