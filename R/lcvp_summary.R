#' Summary results for lcvp_search
#'
#' The functions summarizes the matching results from lcvp_search
#' 
#' @param lcvp_match The output data.frame from lcvp_search.
#'
#' @examples \dontrun{
#' res_ex <- lcvp_search(c("Hibiscus abelmoschus var. betulifolius Mast.",
#' "Hibiscus abutiloides Willd.",
#' "Hibiscus aculeatus",
#' "Hibiscus acuminatus",
#' "Hibiscus furcatuis",
#' "Hibiscus error"), 
#' max.distance = 1)
#' lcvp_summary(res_ex)
#' }
#'@export


lcvp_summary <- function(lcvp_match) {
  
  # Total number of searches
  n_searches <- nrow(lcvp_match)
  
  # Total matches
  n_matches <- sum(!is.na(lcvp_match$Input.Taxon))
  p_matches <- round((n_matches / n_searches) * 100, 2)
  # Correct species names
  n_sp_corre <- sum(lcvp_match$Epithet_match & lcvp_match$Genus_match, 
                    na.rm = TRUE)
  p_sp <- round((n_sp_corre / n_searches) * 100, 2)
  # Authors correct
  n_authors <- sum(lcvp_match$Author_match, 
                   na.rm = TRUE)
  p_authors <- round((n_authors / n_searches) * 100, 2)
  # Infracategories correct
  n_infra <- sum(lcvp_match$Subspecies_match &
                     lcvp_match$Variety_match &
                     lcvp_match$Subvariety_match &
                     lcvp_match$Forma_match &
                     lcvp_match$Subforma_match, 
                 na.rm = TRUE)
  p_infra <- round((n_infra / n_searches) * 100, 2)
  
  # Message output
  cat(paste("Species searched:", n_searches))
  cat(paste("\nSpecies matched:", 
            paste0(n_matches, " (", p_matches, "%)")))
  cat(paste("\nSpecies exactly matched:", 
            paste0(n_sp_corre, " (", p_sp, "%)")))
  cat(paste("\nAuthors exactly matched:", 
            paste0(n_authors, " (", p_authors, "%)")))
  cat(paste("\nInfracategories exactly matched:", 
            paste0(n_infra, " (", p_infra, "%)")))
}
