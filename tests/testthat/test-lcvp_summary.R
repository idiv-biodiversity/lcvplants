if (requireNamespace("LCVP", quietly = TRUE)) {
  test_that("lcvp_summary works", {
    expect_warning(res_ex <-
                     lcvp_search(
                       c(
                         "Hibiscus abelmoschus var. betulifolius Mast.",
                         "Hibiscus abutiloides Willd.",
                         "Hibiscus aculeatus",
                         "Hibiscus acuminatus",
                         "Hibiscus furcatuis",
                         "Hibiscus error"
                       ),
                       max.distance = 1
                     ))
    expect_output(lcvp_summary(res_ex))
  })
}