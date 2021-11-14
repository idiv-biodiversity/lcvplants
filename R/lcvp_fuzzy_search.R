#' Fuzzy match plant names according to the Leipzig Catalogue of Plants (LCVP)
#'
#' Same as \code{\link[lcvplants:lcvp_search]{lcvp_search}}, but it returns
#' all matches from a fuzzy search of plant taxa names listed in the "Leipzig
#' Catalogue of Vascular Plants" (LCVP).
#'
#' @param splist A character vector specifying the input taxon, each element
#' including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name and author name. Only valid characters are allowed 
#' (see \code{\link[base:validEnc]{validEnc}}).
#'
#'@param max_distance It represents the maximum string distance allowed for a
#'  match when comparing the submitted name with the closest name matches in the
#'  LCVP. The distance used is a generalized Levenshtein distance that indicates
#'  the total number of insertions, deletions, and substitutions allowed to
#'  match the two names. It can be expressed as an integer or as the fraction of
#'  the binomial name. For example, a name with length 10, and a max_distance =
#'  0.1, allow only one change (insertion, deletion, or substitution). A
#'  max_distance = 2, allows two changes.
#'  
#' @param genus_fuzzy If TRUE, the fuzzy match algorithm based on max_distance
#'  will also be applied to the genus (note that this may considerably increase
#'  computational time). If FALSE, fuzzy match will only apply to the epithet.
#'
#' @param status A character vector indicating what taxa status should be 
#' included in the results: "accepted", "synonym", "unresolved", "external".
#' 
#' The "unresolved" rank means that the status of the plant name could be 
#' either valid or synonym, but the information available does not allow 
#' a definitive decision. "external" is an extra rank that lists names 
#' outside the scope of this publication but useful to keep on this 
#' updated list.
#'
#' @param bind_result If TRUE the function will return one data.frame (default).
#' If False, the function will return a list of separate data.frames for
#' each input group.
#' 
#' @param keep_closest if TRUE the function will return only the closest names
#'   within the max_distance specified. If FALSE, it will return all names
#'   within the specified distance.
#' 
#' @param progress_bar If TRUE, a progress bar will be printed.
#'
#' 
#' @details 
#' 
#' The algorithm will look for all the names within the given maximum distance
#' defined in `max_distance`. It can return all best matches (keep_closest =
#' TRUE), or all the matches within the distance (keep_closest = FALSE).
#' 
#' Note that only binomial names with valid characters are allowed in this
#' function. Search based on  genus, family, order or author names should use 
#' the function  \code{\link[lcvplants:lcvp_group_search]{lcvp_group_search}}.
#' 
#' @return 
#' A data.frame or a list of data.frames (if \code{bind_result = FALSE}) 
#' with the following columns:
#' 
#' \itemize{
#'  \item{\emph{Search}}{: Taxa name list provided by the user.}
#'  \item{\emph{Input.Taxon}}{: The matched taxa name listed in the LCVP data.}
#'  \item{\emph{Status}}{: Nomenclature status in the Leipzig Catalogue of
#'  Plants: 'accepted', 'synonym', 'unresolved' or 'external'. 
#' The "unresolved" rank means that the status of the plant name could be 
#' either valid or synonym, but the information available does not allow 
#' a definitive decision. "external" is an extra rank that lists names 
#' outside the scope of this publication but useful to keep on this 
#' updated list.}
#'  \item{\emph{PL.comparisson}}{: This field provides a direct comparison with
#'  ‘The Plant List’ (TPL; The Plant List http://www.theplantlist.org/ accessed:
#'  1.1. 2013) reporting further information such as ‘identical', 'synonym',
#'  'other synonym', 'different authors', 'missing', 'misspelling',
#'  'unresolved'.}
#'  \item{\emph{PL.alternative}}{: This field provides a possible alternative
#'  name from ‘The Plant List’.} 
#'  \item{\emph{Output.Taxon}}{: The list of the accepted plant taxa names
#'  according to the LCVP. If input is 'accepted', the output is the same as the
#'  input taxon.}
#'  \item{\emph{Family}}{: The corresponding family name of the Input.Taxon,
#'  staying empty if the Status is unresolved.}
#'  \item{\emph{Order}}{: The corresponding order name of the Input.Taxon,
#'  staying empty if the Status is unresolved.}
#' \item{\emph{Name.Distance}}{The approximate string distance between the Search 
#' and matched Input.Taxon names. See \code{\link[utils:adist]{utils:adist}}
#' for more details.}
#' }
#' See \code{\link[LCVP:tab_lcvp]{LCVP::tab_lcvp}} for more details.
#' 
#' If no match is found for one species it will return NA for the columns in 
#' the LCVP table. But, if no match is found for all species the function will 
#' return NULL and a warning message.
#' 
#' @author 
#' Bruno Vilela & Alexander Ziska
#' 
#' @seealso 
#' \code{\link[lcvplants:lcvp_search]{lcvp_search}}, 
#' \code{\link[lcvplants:lcvp_group_search]{lcvp_group_search}}.
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
#' # Returns a data.frame
#' lcvp_fuzzy_search(c("Hibiscus vitifolia", "Artemisia vulgaris"))
#' 
#' # Returns a list of data.frames
#' lcvp_fuzzy_search(c("Hibiscus vitifolia", "Artemisia vulgaris"),
#' bind_result = FALSE)
#' 
#' # Returns all accepted names within a max_distance of 6.
#' lcvp_fuzzy_search("Hibiscus vitifolia", status = "accepted",
#' keep_closest = FALSE, max_distance = 6)
#' 
#' } 
#'@export



lcvp_fuzzy_search <- function(splist,
                              max_distance = 0.2, 
                              genus_fuzzy = FALSE,
                              status = c("accepted",
                                         "synonym",
                                         "unresolved",
                                         "external"),
                              bind_result = TRUE,
                              keep_closest = TRUE,
                              progress_bar = FALSE) {
  hasData() # Check if LCVP is installed
  # Defensive functions, check for user input errors
  ## Change factors in characters
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
  # Progress_bar
  if (progress_bar) {
    pb <- utils::txtProgressBar(min = 0, max = n_sps, style = 3)
  }
  
  for (i in 1:n_sps) {
    result[[i]] <-
      .lcvp_fuzzy_search_ind(species_class[i, , drop = FALSE],
                            max_distance,
                            status,
                            keep_closest, 
                            genus_fuzzy = genus_fuzzy)
    if (progress_bar) {
      utils::setTxtProgressBar(pb, i)
    }
  }
  if (progress_bar) {
    close(pb)
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
                              max_distance,
                              status,
                              keep_closest, 
                              genus_fuzzy) {
  
  
  if (is.na(species_class[, 3])) {
    warning(paste0("'", species_class[, 1], "' does not include an epithet."),
            call. = FALSE)
    return(NA)
  } else {
    # Now match
    ## Get the genus  first
    max_distance2 <- ifelse(genus_fuzzy, max_distance, 0)
    gen_number <- .lcvp_group_ind(species_class[1, 2],
                                  LCVP::tab_position$Genus,
                                  max_distance = max_distance2,
                                  FALSE)
    pos_genus <- unlist(.genus_search_multiple(gen_number))
    n_class <- ncol(LCVP::lcvp_sps_class)
    
    if (!any(is.na(pos_genus))) {
      # Try fuzzy
      pos_res <- .fuzzy_match(species_class[1,],
                              pos_genus,
                              max_distance,
                              n_class,
                              return_all = TRUE, 
                              keep_closest = keep_closest)
    } else {
      # Fuzzy if did not find the genus
      pos_res <- NULL
    }
    if (length(pos_res) > 0 & !all(is.na(pos_res))) {
      # Result
      result <- LCVP::tab_lcvp[pos_res, , drop = FALSE]
      ## names 1 and 2
      name1 <- paste(species_class[1, 2], species_class[1, 3])
      name2 <- paste(LCVP::lcvp_sps_class[as.numeric(pos_res), 2],
                     LCVP::lcvp_sps_class[as.numeric(pos_res), 3])
      
      # Add a column indicating the distance
      Name.Distance <- t(utils::adist(name1, name2))
      result <- cbind(result, Name.Distance)
      result <- result[order(Name.Distance), , drop = FALSE]
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
