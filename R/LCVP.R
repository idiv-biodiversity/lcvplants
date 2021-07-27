#' Standardize plant names according to the Leipzig Catalogue of Plants (LCVP)
#'
#' This is the new version of the LCVP function

#' @param splist A character vector specifying the input taxon, each element
#' including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name and author name
#' 
#' @param max.distance is an integer value. It represents the maximum distance
#' (number of characters) allowed for a match when comparing the submitted name
#' with the closest name matches in the LCVP
#'
#' @examples \dontrun{
#' 
#' res_ex <- lcvp_search("Hibiscus vitifolius")
#' res_ex <- lcvp_search("Hibiscus vitifoliuse")
#' res_ex <- lcvp_search("Hibiscusa vitifolius")
#' 
#' res_ex <- lcvp_search("Hibiscus abelmoschus var. betulifolius Mast.")
#' 
#' res_ex <- lcvp_search(c("Hibiscus abelmoschus var. betulifolius Mast.",
#' "Hibiscus abutiloides Willd.",
#'  "Hibiscus aculeatus",
#'  "Hibiscus acuminatus"))
#' 
#' system.time(
#' res_ex <- lcvp_search(rep("Hibiscusa vitifolius", 10))
#' )
#' }
#'@export

lcvp_search <- function(splist,
                  max.distance = 0.1) {
  # Defensive function here, check for user input errors
  .checksplist(splist)
  
  # Fix species name
  splist_std <- .standardize(splist)
  
  # Columns position
  # (this helps with future changes in column position)
  species_position <- 1
  
  # Classify splist
  splist_class <- .splist_classify(splist_std)
  
  # Now match
  matching <- .match_algorithm(splist_class,
                               max.distance)
  
  # Elaborate the return object
  ## Return Null if it did not find anything
  if (all(is.na(matching))) {
    result_final <- NULL
  ## Return The matrix with matched species and whether it matched each class
  } else {
    comb_match <- matching[,-(1:2), drop = FALSE]
    
    names_col <-
      colnames(LCVP::lcvp_sps_class)[-c(1, ncol(LCVP::lcvp_sps_class))]
    
    colnames(comb_match) <- paste(names_col, "match", sep = "_")
    
    result_final <- data.frame("Search" = splist,
                               LCVP::tab_lcvp[matching[, 1], , drop = FALSE],
                               comb_match)
    
  }
  return(result_final)
}
