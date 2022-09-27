
if (requireNamespace("LCVP", quietly = TRUE)) {
  splist1 <- sample(apply(LCVP::tab_lcvp[2:10, 2:3], 1, paste, collapse = " "))
  splist2 <- sample(apply(LCVP::tab_lcvp[11:3, 2:3], 1, paste, collapse = " "))
  
  test_that("lcvp_match works, include_all = TRUE", {
    res_ex <- lcvp_match(splist1, splist2, include_all = TRUE)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 17)
    expect_equal(nrow(res_ex), 10)
  })
  
  test_that("lcvp_match works, include_all = FALSE", {
    res_ex <- lcvp_match(splist1, splist2, include_all = FALSE)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 17)
    expect_equal(nrow(res_ex), 9)
  })
  
  test_that("lcvp_match works, include_all = TRUE & identify_dups = FALSE", {
    res_ex <- lcvp_match(splist1,
                         splist2,
                         include_all = TRUE,
                         identify_dups = FALSE)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 16)
    expect_equal(nrow(res_ex), 10)
  })
  
  test_that("lcvp_match works, include_all = FALSE & max_distance = 0", {
    res_ex <- lcvp_match(splist1,
                         splist2,
                         include_all = FALSE,
                         max_distance = 0)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 17)
    expect_equal(nrow(res_ex), 9)
  })
  
  test_that("lcvp_match output errors for wrong inputs", {
    expect_error(lcvp_match(1))
    expect_error(lcvp_match("a", "b"))
    expect_error(lcvp_match("a a", 1))
    expect_error(lcvp_match("\x9a oi", "ok ok"))
    expect_error(lcvp_match("ok ok", "\x9a oi"))
    expect_error(lcvp_match(NA))
  })
}
