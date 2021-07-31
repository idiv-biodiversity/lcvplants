#-------------------------------------------------------#
# The matching algorithm
.match_algorithm  <- function(splist_class, max.distance) {
  # N species
  n_sps <- nrow(splist_class)
  
  # N classes
  n_class <- ncol(LCVP::lcvp_sps_class)
  
  # Save results
  exact <- matrix(ncol = n_class, nrow = n_sps)
  
  
  # Loop across species
  for (i in 1:n_sps) {
    splist_class_i <- splist_class[i, ]
    
    # Search genus position
    #pos_genus <- .genus_matching(splist_class_i)
    
    pos_genus_pre <- .lcvp_group_ind(splist_class_i[2],
                                LCVP::tab_position$Genus,
                                max.distance)
    pos_genus <- .genus_search(pos_genus_pre)
    
    if (!any(is.na(pos_genus))) {
      # Try exact match first
      exact[i, ] <- .exact_match(splist_class_i,
                                pos_genus,
                                n_class)
      # Try fuzzy
      if (any(is.na(exact[i,]))) {
        exact[i, ] <- .fuzzy_match(splist_class_i,
                                  pos_genus,
                                  max.distance,
                                  n_class)
      }
    } else {
      # Fuzzy if did not find the genus
      exact[i,] <- .fuzzy_match(splist_class_i,
                                pos_genus = NULL,
                                max.distance,
                                n_class)
    }
    
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
  
  # Work for the when fuzzy
  if (fuzzy) {
    choosen <- 1:ncol(sp_pos)
  }
  
  
  n_choosen <- length(choosen)
  
  if (n_choosen == 0) {
    # No match found
    return(rep(NA, n_class))
  } else {
    # if there is more than one matched genus and epithet
    # keep the one with more matches across subcategories
    if (n_choosen > 1) {
      choosen <- choosen[which.max(colSums(sp_pos[, choosen]))]
    }
    
    # Pick matched species ID
    matched_sp <-
      LCVP::lcvp_sps_class[pos_genus, , drop = FALSE][choosen, "ID"]
    
    # Concatenate with the matching info
    matched_result <- c(matched_sp, sp_pos[, choosen])
    
    return(matched_result)
  }
}

#-------------------------------------------------------#
# Fuzzy matching function
.fuzzy_match <- function(splist_class_i,
                         pos_genus = NULL,
                         max.distance,
                         n_class) {
  # If we did not find an approximation of the genus
  if (!is.null(pos_genus)) {
  # Use the `agrep` function with the max.distance parameter
    name1 <- paste(splist_class_i[2], splist_class_i[3])
    name2 <- paste(LCVP::lcvp_sps_class[pos_genus, 2], 
                   LCVP::lcvp_sps_class[pos_genus, 3])
  fuzzy_match <- agrep(name1,
                       name2,
                       max.distance = max.distance)
  }
  if (is.null(pos_genus) | length(fuzzy_match) == 0) {
    pos_genus <- 1:nrow(LCVP::lcvp_sps_class)
    # Use the `agrep` function with the max.distance parameter
    name1 <- paste(splist_class_i[2], splist_class_i[3])
    name2 <- name2
    fuzzy_match <- agrep(name1,
                         name2,
                         max.distance = max.distance)
  }
  
  if (length(fuzzy_match) == 0) {
    # No match found
    return(rep(NA, n_class))
  } else {
    # Keep the closest
    which_closest <- which.min(utils::adist(name1, name2[fuzzy_match]))
    fuzzy_match <- fuzzy_match[which_closest]
    
    # Reuse the exact_match function, but look only for fuzzy matches
    pos_genus <-
      as.numeric(LCVP::lcvp_sps_class[pos_genus, "ID"][fuzzy_match])
    res_fuzzy <- .exact_match(splist_class_i,
                              pos_genus,
                              n_class,
                              fuzzy = TRUE)
    return(res_fuzzy)
  }
}