if (requireNamespace("LCVP", quietly = TRUE)) {
  test_that("lcvp_fuzzy_search works for one species", {
    res_ex <- lcvp_fuzzy_search("Hibiscus vitifolia")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 8)
    expect_equal(nrow(res_ex), 2)
  })
  
  test_that("lcvp_fuzzy_search works for one species, only accepted", {
    res_ex <-
      lcvp_fuzzy_search("Hibiscus vitifolia", status = "accepted")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 8)
    expect_equal(nrow(res_ex), 1)
    expect_true(all(res_ex$Status == "accepted"))
  })
  
  test_that("lcvp_fuzzy_search works for multiple species, bind_result = TRUE",
            {
              res_ex <-
                lcvp_fuzzy_search(c("Hibiscus vitifolia", "Adansonia digitata"),
                                  bind_result = TRUE)
              expect_equal(class(res_ex), "data.frame")
              expect_equal(ncol(res_ex), 8)
              expect_equal(nrow(res_ex), 3)
            })
  
  test_that("lcvp_fuzzy_search works for multiple species, bind_result = FALSE",
            {
              res_ex <-
                lcvp_fuzzy_search(c("Hibiscus vitifolia", "Adansonia digitata"),
                                  bind_result = FALSE)
              expect_equal(class(res_ex), "list")
              expect_equal(length(res_ex), 2)
            })
  
  test_that("lcvp_fuzzy_search output errors for wrong inputs", {
    expect_error(lcvp_fuzzy_search(1))
    expect_error(lcvp_fuzzy_search("a"))
    expect_error(lcvp_fuzzy_search("\x9a oi"))
    expect_error(lcvp_fuzzy_search(NA))
    expect_error(lcvp_fuzzy_search("Hibiscus vitifolia", status = "Hey"))
  })
  
}
