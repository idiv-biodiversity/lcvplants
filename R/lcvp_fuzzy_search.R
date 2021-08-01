#' Fuzzy match plant names according to the Leipzig Catalogue of Plants (LCVP)
#'
#' This function return all names matched in the Leipzig Catalogue of Plants 
#' (LCVP) based on a user defined word distance.
#' 
#' @param splist A character specifying one input taxon, each element
#' including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name and author name.
#' 
#' @param max.distance is an integer value. It represents the maximum distance
#' (number of characters) allowed for a match when comparing the submitted name
#' with the closest name matches in the LCVP
#'
#' @examples \dontrun{
#' 
#' res_ex <- lcvp_fuzzy_search("Hibiscus vitifolia")
#' }
#'@export
lcvp_fuzzy_search <- function(splist,
                              max.distance = 0.1) {
  # Defensive function here, check for user input errors
  .names_check(splist, "splist")

  # Fix species name
  splist_std <- .names_standardize(splist)

  # Classify splist
  splist_class <- .splist_classify(splist_std)

  # Now match
  ## Get the genus  first
  gen_number <- .lcvp_group_ind(splist_class[1, 2], 
                  LCVP::tab_position$Genus,
                  max.distance,
                  FALSE)
  pos_genus <- unlist(.genus_search_multiple(gen_number))
  
  ## Than get all matches 
  name1 <- paste(splist_class[1, 2], splist_class[1, 3])
  name2 <- paste(LCVP::lcvp_sps_class[pos_genus, 2], 
                 LCVP::lcvp_sps_class[pos_genus, 3])
  fuzzy_match <- agrep(name1,
                       name2,
                       max.distance = max.distance)
  
  # Result
  pos_res <- as.numeric(LCVP::lcvp_sps_class[pos_genus, "ID"][fuzzy_match])
  result <- LCVP::tab_lcvp[pos_res, , drop = FALSE]
  # Add a column indicating the distance
  Name.Distance <- t(utils::adist(name1, name2[fuzzy_match]))
  result <- cbind(result, Name.Distance)
  return(result)
}
  