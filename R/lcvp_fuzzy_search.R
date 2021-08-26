#' Fuzzy match plant names according to the Leipzig Catalogue of Plants (LCVP)
#'
#' This function return all names matched in the Leipzig Catalogue of Plants
#' (LCVP) based on a user defined word distance.
#'
#' @param splist A character specifying the input taxon, each element
#' including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name and author name.
#'
#' @param max.distance It represents the maximum distance allowed for a match
#' when comparing the submitted name with the closest name matches in the LCVP.
#' Expressed either as integer, or as a fraction of the pattern length times the maximal
#' transformation cost (will be replaced by the smallest integer not less than
#' the corresponding fraction). See \code{\link[base]{agrep}} for more details.
#'
#' @param status A character vector indicating what status should be included
#' in the results: "accepted", "synonym", "unresolved", "external".
#'
#' @param bind_result If TRUE the function will return one data.frame.
#' If False, the function will return a list of separate data.frames for
#' each input group.
#' 
#' @examples \dontrun{
#'
#' res_ex <- lcvp_fuzzy_search(c("Hibiscus vitifolia", "Adansonia digitata"),
#' bind_result = FALSE)
#' }
#'@export



lcvp_fuzzy_search <- function(splist,
                              max.distance = 0.1,
                              status = c("accepted",
                                         "synonym",
                                         "unresolved",
                                         "external"),
                              bind_result = TRUE) {
  # Defensive function here, check for user input errors
  if (is.factor(splist)) {
    splist <- as.character(splist)
  }
  .names_check(splist, "splist")
  .check_status(status)
  
  # Fix species name
  species_std <- .names_standardize(splist)
  
  # Classify species
  species_class <- .splist_classify(species_std)
  
  # Check binomial
  .check_binomial(species_class, splist)
  
  # Run individual algorithm to multiple species
  n_sps <- length(splist)
  result <- list()
  for (i in 1:n_sps) {
    result [[i]] <-
      .lcvp_fuzzy_search_ind(species_class[i, , drop = FALSE],
                            max.distance,
                            status)
  }
  # If need to bind the results
  if (bind_result) {
    result <- do.call(rbind, result)
    result <- result[!is.na(result[, 1]), , drop = FALSE]
    if (nrow(result) == 0) {
      return(NULL)
    } 
  } else {
    names(result) <- splist
  }
  return(result) 
}




#----------------------------------------------------

.lcvp_fuzzy_search_ind <- function(species_class,
                              max.distance,
                              status) {
  
  
  if (is.na(species_class[, 3])) {
    warning(paste0("'", species_class[, 1], "' does not include an epithet."),
            call. = FALSE)
    return(NA)
  } else {
    # Now match
    ## Get the genus  first
    gen_number <- .lcvp_group_ind(species_class[1, 2],
                                  LCVP::tab_position$Genus,
                                  max.distance,
                                  FALSE)
    pos_genus <- unlist(.genus_search_multiple(gen_number))
    
    ## Than get all matches
    name1 <- paste(species_class[1, 2], species_class[1, 3])
    name2 <- paste(LCVP::lcvp_sps_class[pos_genus, 2],
                   LCVP::lcvp_sps_class[pos_genus, 3])
    fuzzy_match <- agrep(name1,
                         name2,
                         max.distance = max.distance)
    if (length(fuzzy_match) > 0) {
      # Result
      pos_res <-
        as.numeric(LCVP::lcvp_sps_class[pos_genus, "ID"][fuzzy_match])
      result <- LCVP::tab_lcvp[pos_res, , drop = FALSE]
      # Add a column indicating the distance
      Name.Distance <- t(utils::adist(name1, name2[fuzzy_match]))
      result <- cbind(result, Name.Distance)
      
      if (!all(c("accepted", "synonym", "unresolved", "external") %in% status)) {
        result <- result[result$Status %in% status, , drop = FALSE]
      }
      rownames(result) <- NULL
      return(result)
    } else {
      warning(paste0("No match found for ", "'", species_class[, 1], "'."),
              call. = FALSE)
      return(NA)
    }
  }
}
