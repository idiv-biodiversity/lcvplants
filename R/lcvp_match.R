#' Match two name list using the Leipzig Catalogue of Plants (LCVP)
#'
#'
#'Match two name list using the Leipzig Catalogue of Plants (LCVP)
#' 
#' @param splist1 A character vector specifying the input taxon, each element
#' including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name and author name
#' 
#' @param splist2 A character vector specifying the input taxon, each element
#' including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name and author name
#' 
#' @param max.distance It represents the maximum distance allowed for a match 
#' when comparing the submitted name with the closest name matches in the LCVP. 
#' Expressed either as integer, or as a fraction of the pattern length times the maximal 
#' transformation cost (will be replaced by the smallest integer not less than 
#' the corresponding fraction). See \code{\link[base]{agrep}} for more details.
#'
#'@export

# splist1 <- sample(LCVP::tab_lcvp$Input.Taxon[1:10])
# splist2 <- sample(LCVP::tab_lcvp$Input.Taxon[1:10])
# max.distance = 0.1

lcvp_match <- function(splist1, 
                       splist2, 
                       max.distance = 0.1) {
  
  search1 <- lcvp_search(splist1, max.distance)
  search2 <- lcvp_search(splist2, max.distance)
  
}
