#' Match two name list using the Leipzig Catalogue of Plants (LCVP)
#'
#'
#'Match two name list using the Leipzig Catalogue of Plants (LCVP)
#'
#' @param splist1 A character vector specifying the reference input taxon to be matched.
#' Each element including genus and specific epithet and, potentially,
#' infraspecific rank, infraspecific name, and author name.
#'
#' @param splist2 A character vector specifying the input taxon to match splist1.
#' Each element including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name, and author name.
#'
#' @param max.distance It represents the maximum distance allowed for a match
#' when comparing the submitted name with the closest name matches in the LCVP.
#' Expressed either as integer, or as a fraction of the pattern length times the maximal
#' transformation cost (will be replaced by the smallest integer not less than
#' the corresponding fraction). See \code{\link[base]{agrep}} for more details.
#' 
#' @param include_all If TRUE (default), it will include all species in both
#'  splist1 and splist2. If FALSE, it will exclude species only found in splist2.
#'  
#' @param identify_dups If TRUE (default), a column indicating the position 
#'  of duplicated LCVP output names in the resulting data.frame.
#'
#'
#'@examples \dontrun{
#' splist1 <- sample(LCVP::tab_lcvp$Input.Taxon[5:100])
#' splist2 <- sample(LCVP::tab_lcvp$Input.Taxon[100:2])
#' res <- lcvp_match(splist1, splist2, include_all = TRUE)
#' }
#'@export


lcvp_match <- function(splist1,
                       splist2,
                       max.distance = 0.1,
                       include_all = TRUE, 
                       identify_dups = TRUE) {
  # Defensive
  .names_check(splist1, "splist1")
  .names_check(splist2, "splist2")
  
  # Run the search
  search1 <- lcvp_search(splist1, max.distance)
  if (is.null(search1)) {
    stop(paste("No match found for splist1.",
               "Try increasing the 'max.distance' argument."),
         call. = FALSE)
  }
  search2 <- lcvp_search(splist2, max.distance)
  if (is.null(search2)) {
    stop(paste("No match found for splist2.",
         "Try increasing the 'max.distance' argument."),
         call. = FALSE)
  }
  # match
  match_pos <- match(search1$Input.Taxon,
                     search2$Input.Taxon,
                     incomparables = NA)
  
  # Adjust output
  sp2 <- splist2[match_pos]
  result <- data.frame(
    "Species.List.1" = splist1,
    "Species.List.2" = sp2,
    "LCVP.Input.Taxon" = search1$Input.Taxon,
    "LCVP.Output.Taxon" = search1$Output.Taxon,
    "Match.Position.2to1" = match_pos
  )
  
  ## Include species only in second dataset
  if (include_all) {
    pos_no_match <- which(!(splist2 %in% sp2))
    if (length(pos_no_match) > 0) {
      sp2_miss <- splist2[pos_no_match]
      for (i in 1:length(sp2_miss)) {
        extra_lines <- c(NA,
                         sp2_miss[i],
                         search2$Input.Taxon[pos_no_match[i]],
                         search2$Output.Taxon[pos_no_match[i]],
                         pos_no_match[i])
        result <- rbind(result, extra_lines)
      }
    }
  }
  if (identify_dups) {
  result$Duplicated.Output.Position <- .find_dups(result)
  }
  return(result)
}



