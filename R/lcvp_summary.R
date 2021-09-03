#' Summarize the results from lcvp_search
#'
#' Summarizes the matching results from 
#' \code{\link[lcvplants:lcvp_search]{lcvp_search}}.
#' 
#' 
#' @param lcvp_match The output data.frame from 
#' \code{\link[lcvplants:lcvp_search]{lcvp_search}}.
#' 
#' @details 
#' \code{lcvp_summary} gives a report on the searching results using the 
#' \code{\link[lcvplants:lcvp_search]{lcvp_search}} function. Indicating the
#' number of species searched, how many were matched. Among the matched names, 
#' it indicates how many were exactly or fuzzy matched. Then it checks how many 
#' author and infracategory names were exactly matched. Note that if authors or 
#' infracategory is not provided, it will be considered a no match.
#' 
#' 
#' @return
#' It returns:
#' * The number of species searched;
#' * The number and percentage of species names found in the LCVP data;
#' * The number and percentage of species names exactly matched;
#' * The number and percentage of species names fuzzy matched;
#' * The number and percentage of author names exactly matched;
#' * The number and percentage of infracategories exactly matched.
#'
#' @author 
#' Bruno Vilela & Alexander Ziska
#' 
#' @seealso 
#' \code{\link[lcvplants:lcvp_search]{lcvp_summary}} 
#' 
#' @references 
#' Freiberg, M., Winter, M., Gentile, A. et al. LCVP, The Leipzig 
#' catalogue of vascular plants, a new taxonomic reference list for all known 
#' vascular plants. Sci Data 7, 416 (2020). 
#' https://doi.org/10.1038/s41597-020-00702-z 
#' 
#' @keywords R-package nomenclature taxonomy vascular plants
#' 
#' @examples
#' # Ensure that LCVP package is available before running the example.
#' # If it is not, see the `lcvplants` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("LCVP", quietly = TRUE)) { # Do not run this
#' 
#' # Perform the search
#' x <- lcvp_search(c("Hibiscus abelmoschus var. betulifolius Mast.",
#' "Hibiscus abutiloides Willd.",
#' "Hibiscus aculeatus",
#' "Hibiscus acuminatis",
#' "Hibiscus error"), 
#' max.distance = 1)
#' 
#' # Summarize the results
#' lcvp_summary(x)
#' 
#' }
#'@export


lcvp_summary <- function(lcvp_match) {
  
  lcvp_match <- attributes(lcvp_match)$match.names
  # Total number of searches
  n_searches <- nrow(lcvp_match)

  # Total matches
  n_matches <- sum(!is.na(lcvp_match$Genus_match))
  p_matches <- round((n_matches / n_searches) * 100, 2)
  
  # Correct species names
  n_sp_corre <- sum(lcvp_match$Epithet_match & lcvp_match$Genus_match, 
                    na.rm = TRUE)
  p_sp <- round((n_sp_corre / n_searches) * 100, 2)
  
  # Fuzzy matches 
  n_fuzzy <- n_matches - n_sp_corre
  p_fu <- round((n_fuzzy / n_searches) * 100, 2)
  
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
  cat(paste("\nSpecies fuzzy matched:", 
            paste0(n_fuzzy, " (", p_fu, "%)")))
  cat(paste("\nAuthors exactly matched:", 
            paste0(n_authors, " (", p_authors, "%)")))
  cat(paste("\nInfracategories exactly matched:", 
            paste0(n_infra, " (", p_infra, "%)")))
}
