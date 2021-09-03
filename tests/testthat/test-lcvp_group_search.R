if (requireNamespace("LCVP", quietly = TRUE)) {
  test_that("lcvp_group_search works for one genus", {
    res_ex <- lcvp_group_search("AA",
                                search_by = "Genus")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 7)
    expect_equal(nrow(res_ex), 42)
  })
  
  test_that("lcvp_group_search works for two genus, bind_result = TRUE", {
    res_ex <- lcvp_group_search(c("AA", "Adansonia"),
                                search_by = "Genus",
                                bind_result = TRUE)
    
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 7)
    expect_equal(nrow(res_ex), 73)
  })
  
  test_that("lcvp_group_search works for two genus, bind_result = FALSE", {
    res_ex <- lcvp_group_search(c("AA", "Adansonia"),
                                search_by = "Genus",
                                bind_result = FALSE)
    
    expect_equal(class(res_ex), "list")
    expect_equal(length(res_ex), 2)
  })
  
  test_that("lcvp_group_search works for one author, only accepted", {
    res_ex <- lcvp_group_search("Schltr.",
                                search_by = "Author",
                                status = "accepted")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 7)
    expect_equal(nrow(res_ex), 5330)
  })
  
  test_that("lcvp_group_search works for one family, only unresolved", {
    res_ex <- lcvp_group_search("Orchidaceae",
                                search_by = "Family",
                                status = "unresolved")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 7)
    expect_equal(nrow(res_ex), 1442)
  })
  
  test_that("lcvp_group_search works for one order, only synonym", {
    res_ex <- lcvp_group_search("Asparagales",
                                search_by = "Order",
                                status = "synonym")
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex), 7)
    expect_equal(nrow(res_ex), 75154)
  })
  
  test_that("lcvp_group_search works for two families, bind_result = FALSE,
          status = external",
            {
              res_ex <- lcvp_group_search(
                c("Orchidaceae", "POACEAE"),
                search_by = "Family",
                bind_result = FALSE,
                status = "external"
              )
              expect_equal(class(res_ex), "list")
              expect_equal(length(res_ex), 2)
            })
  
  
  test_that("lcvp_group_search output errors for wrong inputs", {
    expect_error(lcvp_group_search(1))
    expect_error(lcvp_group_search("a"))
    expect_error(lcvp_group_search("\x9a oi"))
    expect_error(lcvp_group_search(NA))
    expect_error(lcvp_group_search("AA",
                                   search_by = "Hey"))
    expect_error(lcvp_group_search("AA",
                                   search_by = "Genus",
                                   status = "Hey"))
    
  })
}