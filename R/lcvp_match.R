#' Match two name lists using the Leipzig Catalogue of Plants (LCVP)
#'
#'
#' Match and compares two name lists using based on the taxonomic resolution of 
#' plant taxa names listed in the "Leipzig Catalogue of Vascular Plants" (LCVP).
#'
#' @param splist1 A character vector specifying the reference input taxon to be matched.
#' Each element including genus and specific epithet and, potentially,
#' infraspecific rank, infraspecific name, and author name. Only valid characters are allowed 
#' (see \code{\link[base:validEnc]{base:validEnc()}}).
#'
#' @param splist2 A character vector specifying the input taxon to match splist1.
#' Each element including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name, and author name. Only valid characters are allowed 
#' (see \code{\link[base:validEnc]{base:validEnc()}}).
#'
#' @param max.distance It represents the maximum distance allowed for a match 
#' when comparing the submitted name with the closest name matches in the LCVP. 
#' Expressed either as integer, or as a fraction of the pattern length times the 
#' maximal transformation cost (will be replaced by the smallest integer not 
#' less than the corresponding fraction). 
#' See \code{\link[base:agrep]{agrep}} for more details.
#' 
#' @param include_all If \code{TRUE} (default), it will include all species in both
#'  \code{splist1} and \code{splist2}. If \code{FALSE}, it will exclude species 
#'  only found in \code{splist2}.
#'  
#' @param identify_dups If \code{TRUE} (default), a column indicating the position 
#'  of duplicated LCVP output names in the resulting data.frame.
#'
#' 
#' @return 
#' A data.frame with the following columns:
#' 
#' \itemize{
#' \item{\emph{Species.List.1}}{: Taxa name list provided by the user in the 
#' splist1.}
#' \item{\emph{Species.List.2}}{: Taxa name list provided by the user in the 
#' splist2.}
#' \item{\emph{LCVP.Input.Taxon}}{: Matched taxa names listed in the LCVP data.}  
#' \item{\emph{Status}}{: Nomenclature status: 'accepted', 'synonym', 
#' 'unresolved' or 'external'.}
#' \item{\emph{LCVP.Output.Taxon}}{: The list of the accepted plant taxa names 
#' according to the LCVP.}  
#' \item{\emph{Match.Position.2to1}}{: positions of the names in splist1 in
#'  splist2. Can be used to reorder splist2 to match splist1.
#'  }
#' \item{\emph{Duplicated.Output.Position}}{: If identify_dups = TRUE, it will 
#' indicate the position of duplicated names in LCVP.Output.Taxon column. 
#' This may occur if two inputs are now synonyms. It will output NA if there is 
#' no duplicated for the species name.
#' }
#'  
#' }
#' See \code{\link[LCVP:tab_lcvp]{LCVP:tab_lcvp}} for more details.
#' 
#' If include_all = TRUE, all species will be included. Ordered based on the 
#' splist1, and followed by non matched names in splist2. If include_all = FALSE,
#' non matched names in splist2 are not included.
#' 
#' @author 
#' Bruno Vilela & Alexander Ziska
#' 
#' @seealso 
#' \code{\link[lcvplants:lcvp_join]{lcvp_join}}
#' 
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
#'
#' # Generate two lists of species name
#' splist1 <- sample(LCVP::tab_lcvp$Input.Taxon[5:100])
#' splist2 <- sample(LCVP::tab_lcvp$Input.Taxon[100:2])
#' 
#' # Including all species in both lists
#' lcvp_match(splist1, splist2, include_all = TRUE)
#' 
#' # Including all species only in the first list
#' matchLists <- lcvp_match(splist1, splist2, include_all = FALSE)
#' ## This can be used to quickly change positions in splist2 to match splist1
#' splist2[matchLists$Match.Position.2to1]
#' 
#' 
#'@export


lcvp_match <- function(splist1,
                       splist2,
                       max.distance = 0.1,
                       include_all = TRUE, 
                       identify_dups = TRUE) {
  # Defensive
  if (is.factor(splist1)) {
    splist1 <- as.character(splist1)
  }
  if (is.factor(splist2)) {
    splist2 <- as.character(splist2)
  }
  
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
    "Status" = search1$Status,
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
                         search2$Status[pos_no_match[i]],
                         search2$Output.Taxon[pos_no_match[i]],
                         pos_no_match[i])
        result <- rbind(result, extra_lines)
      }
    }
  }
  if (identify_dups) {
  result$Duplicated.Output.Position <- .find_dups(result, output_pos = 5)
  }
  return(result)
}



