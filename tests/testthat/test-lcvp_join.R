if (requireNamespace("LCVP", quietly = TRUE)) {
  # Create data.frame1
  splist1 <- sample(apply(LCVP::tab_lcvp[2:10, 2:3], 1, paste, collapse = " "))
  x <- data.frame("Species" = splist1, "Trait1" = runif(length(splist1)))

  # Create data.frame2
  splist2 <-sample(apply(LCVP::tab_lcvp[11:3, 2:3], 1, paste, collapse = " "))
  y <- data.frame("Species" = splist2,
  "Trait2" = runif(length(splist2)),
  "Trait3" = runif(length(splist2)),
  "Trait4" = sample(c("a", "b"), length(splist2), replace = TRUE),
  "Trait5" = sample(c(TRUE, FALSE), length(splist2), replace = TRUE))
  
  
  test_that("lcvp_join works, type = full", {
    res_ex <- lcvp_join(x, y, c("Species", "Species"), type = "full")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 19)
    expect_equal(nrow(res_ex), 10)
  })
  
  test_that("lcvp_join works, type = full & solve_duplicated = TRUE", {
    res_ex <- lcvp_join(x,
                        y,
                        c("Species", "Species"),
                        type = "full",
                        solve_duplicated = TRUE)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 19)
    expect_equal(nrow(res_ex), 10)
  })
  
  
  test_that("lcvp_join works, type = left", {
    res_ex <- lcvp_join(x, y, c("Species", "Species"), type = "left")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 19)
    expect_equal(nrow(res_ex), 9)
  })
  
  test_that("lcvp_join works, type = right", {
    res_ex <- lcvp_join(x, y, c("Species", "Species"), type = "right")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 19)
    expect_equal(nrow(res_ex), 9)
  })
  
  test_that("lcvp_join works, type = inner", {
    res_ex <- lcvp_join(x, y, c("Species", "Species"), type = "inner")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 19)
    expect_equal(nrow(res_ex), 8)
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