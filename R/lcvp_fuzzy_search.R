#' Fuzzy match plant names according to the Leipzig Catalogue of Plants (LCVP)
#'
#' This function return all names matched in the Leipzig Catalogue of Plants
#' (LCVP) based on a user defined word distance.
#'
#' @param species A character specifying one input taxon, each element
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
#' @examples \dontrun{
#'
#' res_ex <- lcvp_fuzzy_search("Hibiscus vitifolia")
#' }
#'@export



lcvp_fuzzy_search <- function(species,
                              max.distance = 0.1,
                              status = c("accepted",
                                         "synonym",
                                         "unresolved",
                                         "external")) {
  # Defensive function here, check for user input errors
  .names_check(species, "species")
  
  # Fix species name
  species_std <- .names_standardize(species)
  
  # Classify species
  species_class <- .splist_classify(species_std)
  
  if (is.na(species_class[, 3])) {
    warning("species argument does not include an epithet", call. = FALSE)
    return(NULL)
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
      return(result)
    } else {
      return(NULL)
    }
  }
}
