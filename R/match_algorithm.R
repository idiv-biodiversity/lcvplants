# Code for the algorithms to exactly and fuzzy match names
# Author: Bruno Vilela

#-------------------------------------------------------#
# The matching algorithm
.match_algorithm  <- function(splist_class,
                              max_distance,
                              progress_bar = FALSE,
                              keep_closest = TRUE,
                              genus_fuzzy = TRUE, 
                              grammar_check = FALSE) {
  # N species
  n_sps <- nrow(splist_class)
  
  # N classes
  n_class <- ncol(LCVP::lcvp_sps_class)
  
  # Save results
  exact <- matrix(ncol = n_class + 1, nrow = n_sps)
  
  
  # Loop across species
  if (progress_bar) {
    pb <- utils::txtProgressBar(min = 0,
                                max = n_sps,
                                style = 3)
  }
  for (i in seq_len(n_sps)) {
    splist_class_i <- splist_class[i, ]
    check_non_defined <-
      splist_class_i[3] %in% c("SP", "SP.",
                               "SPEC.", "AGG.")
    if (!check_non_defined) {
      # Search genus position
      max_distance2 <- ifelse(genus_fuzzy, max_distance, 0)
      pos_genus_pre <- .lcvp_group_ind(
        group_name = splist_class_i[2],
        group_ref = LCVP::tab_position$Genus,
        max_distance2,
        only_one = FALSE,
        closest = TRUE
      )
      pos_genus <- .genus_search(pos_genus_pre)
      
      if (!any(is.na(pos_genus))) {
        # Try exact match first
        exact[i,] <- .exact_match(splist_class_i,
                                  pos_genus,
                                  n_class)
        # Try common grammar errors 
        if (any(is.na(exact[i, ])) & grammar_check) {
          epis <- .sub_common(splist_class_i[3])
          if (length(epis) > 0) {
            names_grammar <- paste(splist_class_i[2],
                                         epis)
            splist_class_i_mult <- .splist_classify(names_grammar)
            n_gram <- length(names_grammar)
            temp <- matrix(nrow = n_gram,
                           ncol = length(exact[i, ]))
            for (k in seq_len(n_gram)) {
              temp[k, ] <- .exact_match(splist_class_i_mult[k, ],
                                        pos_genus,
                                        n_class)
            }
            pos_gram <- apply(temp, 1, function(x) {all(!is.na(x))})
            n_pos_gram <- sum(pos_gram)
            if (n_pos_gram > 1) {
              pos_gram_t <- pos_gram == TRUE
              pos_gram[pos_gram_t] <- c(TRUE, rep(FALSE, (n_pos_gram - 1)))
              temp[pos_gram, ncol(temp)] <- TRUE
            }
            if (any(pos_gram)) {
              exact[i,] <- temp[pos_gram, ] 
            }
          }
        }

        # Try fuzzy
        if (any(is.na(exact[i, ])) & max_distance > 0) {
          exact[i,] <- .fuzzy_match(splist_class_i,
                                    pos_genus,
                                    max_distance,
                                    n_class,
                                    keep_closest = keep_closest,
                                    max_distance2 = max_distance2)
        }
      }
      ## may improve match performance (a little), but consumes more
      ## computational time; turned off.
      # else {
      #
      #   if (max_distance2 > 0) {
      #     # Fuzzy if did not find the genus
      #     exact[i,] <- .fuzzy_match(splist_class_i,
      #                               pos_genus = NULL,
      #                               max_distance,
      #                               n_class,
      #                               keep_closest = keep_closest)
      #   }
      # }
    }
    if (progress_bar) {
      utils::setTxtProgressBar(pb, i)
    }
  }
  if (progress_bar) {
    close(pb)
  }
  return(exact)
}




#-------------------------------------------------------#
# Exact match function
.exact_match <- function(splist_class_i,
                         pos_genus,
                         n_class,
                         fuzzy = FALSE) {
  # Look the categories that are equal
  sp_pos <- apply(LCVP::lcvp_sps_class[pos_genus, -n_class,
                                       drop = FALSE],
                  1,
                  function(x) {
                    x == splist_class_i
                  })
  
  # Identify the actual number positions
  choosen <- which(sp_pos[3, ])
  
  # Set homonyms FALSE
  homonyms <- FALSE
  
  
  # Work when fuzzy
  if (fuzzy) {
    choosen <- seq_len(ncol(sp_pos))
  }
  
  
  n_choosen <- length(choosen)
  
  # IF NEEDED INSERT COMMON ERRORS HERE
  if (n_choosen == 0) {
    # No match found
    return(rep(NA, n_class + 1))
  } else {
    # if there is more than one matched genus and epithet
    # keep the one with more matches across subcategories
    if (n_choosen > 1) {
      sum_equals <- colSums(sp_pos[, choosen])
      pos_equals <- sum_equals == max(sum_equals)
      choosen <- choosen[pos_equals]
      if (length(choosen) > 1) {
        matched_sp <-
          LCVP::lcvp_sps_class[pos_genus, , drop = FALSE][choosen, "ID"]
        status_pos <-
          LCVP::tab_lcvp[as.numeric(matched_sp), "Status"]
        test_accepted <- status_pos == "accepted"
        if (any(test_accepted)) {
          choosen <- choosen[test_accepted][1]
          homonyms <- TRUE
        } else {
          homonyms <- TRUE
          choosen <- choosen[1]
        }
      }
    }
    
    
    # Pick matched species ID
    matched_sp <-
      LCVP::lcvp_sps_class[pos_genus, , drop = FALSE][choosen, "ID"]
    
    # Concatenate with the matching info
    matched_result <- c(matched_sp, sp_pos[, choosen], homonyms)
    
    return(matched_result)
  }
}

#-------------------------------------------------------#
# Fuzzy matching function
.fuzzy_match <- function(splist_class_i,
                         pos_genus = NULL,
                         max_distance,
                         n_class,
                         return_all = FALSE,
                         keep_closest = TRUE,
                         max_distance2 = max_distance) {
  # If we did not find an approximation of the genus
  fuzzy_match <- NULL
  if (!is.null(pos_genus)) {
    # Use the `.agrep_whole` function with the max_distance parameter
    name1 <- paste(splist_class_i[2], splist_class_i[3])
    name2 <- paste(LCVP::lcvp_sps_class[pos_genus, 2],
                   LCVP::lcvp_sps_class[pos_genus, 3])
    fuzzy_match <- .agrep_whole(name1,
                                name2,
                                max_distance = max_distance)
  }
  if (is.null(pos_genus) | length(fuzzy_match) == 0) {
    pos_genus <- seq_len(nrow(LCVP::lcvp_sps_class))
    # Use the `.agrep_whole` function with the max_distance parameter
    name1 <- paste(splist_class_i[2], splist_class_i[3])
    name2 <- paste(LCVP::lcvp_sps_class[, 2],
                   LCVP::lcvp_sps_class[, 3])
    fuzzy_match <- .agrep_whole(name1,
                                name2,
                                max_distance = max_distance2)
  }
  
  if (length(fuzzy_match) == 0) {
    # No match found
    return(rep(NA, n_class + 1))
  } else {
    if (keep_closest) {
      # Keep the closest
      dist_names <- utils::adist(name1, name2[fuzzy_match])
      which_closest <- which(dist_names == min(dist_names))
      fuzzy_match <- fuzzy_match[which_closest]
    }
    # Reuse the exact_match function, but look only for fuzzy matches
    pos_genus <-
      as.numeric(LCVP::lcvp_sps_class[pos_genus, "ID"][fuzzy_match])
    n_pos_genus <- length(pos_genus)
    
    res_fuzzy <- matrix(nrow = n_pos_genus, ncol = n_class + 1)
    
    for (i in seq_len(n_pos_genus)) {
      res_fuzzy[i, ] <- .exact_match(splist_class_i,
                                     pos_genus[i],
                                     n_class,
                                     fuzzy = TRUE)
    }
    # keep only the ones with highest number of classes matches
    rights <- apply(res_fuzzy[, -1, drop = FALSE],
                    1,
                    function(x) {
                      sum(x == "TRUE")
                    })
    pos_genus2 <- which(rights == max(rights))
    # If more than one
    if (!return_all) {
      if (length(pos_genus2) > 1) {
        sub_tab <- LCVP::tab_lcvp[res_fuzzy[pos_genus2, 1], ]
        pos_genus2 <- which(sub_tab$Status == "accepted")
        res_fuzzy[, n_class + 1] <- TRUE # homonyms to TRUE
        if (length(pos_genus2) == 0) {
          pos_genus2 <- 1
        }
      }
      return(res_fuzzy[pos_genus2[1], ])
    }
    if (return_all) {
      return(res_fuzzy[pos_genus2, 1])
    }
  }
}
