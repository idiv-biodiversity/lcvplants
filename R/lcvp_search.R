#'Standardize plant names according to the Leipzig Catalogue of Plants (LCVP)
#'
#'Allow taxonomic resolution of plant taxa names listed in the "Leipzig
#'Catalogue of Vascular Plants" (LCVP). Connects to the LCVP table and validates
#'the names of a vector of plant taxa, replacing synonyms with accepted names
#'and removing orthographic errors in plant names. The LCVP data package must be
#'installed. It is available from https://github.com/idiv-biodiversity/LCVP.
#'
#'@param splist A character vector specifying the input taxon, each element
#'  including genus and specific epithet and, potentially, infraspecific rank,
#'  infraspecific name and author name. Only valid characters are allowed (see
#'  \code{\link[base:validEnc]{base::validEnc}}).
#'
#' @param max_distance match when comparing the submitted name with the closest name matches in the
#'  LCVP. The distance used is a generalized Levenshtein distance that indicates
#'  the total number of insertions, deletions, and substitutions allowed to
#'  match the two names. It can be expressed as an integer or as the fraction of
#'  the binomial name. For example, a name with length 10, and a max_distance =
#'  0.1, allow only one change (insertion, deletion, or substitution). A
#'  max_distance = 2, allows two changes.
#'
#'@param genus_fuzzy If TRUE, the fuzzy match algorithm based on max_distance
#'  will also be applied to the genus (note that this may considerably increase
#'  computational time). If FALSE, fuzzy match will only apply to the epithet.
#'  
#'@param grammar_check if TRUE, the algorithm will try to fix common latin 
#'grammar mistakes.
#'
#'@param show_correct If TRUE, a column is added to the final result indicating
#'  whether the binomial name was exactly matched (TRUE), or if it is misspelled
#'  (FALSE).
#'  
#'@param progress_bar If TRUE, a progress bar will be printed.
#'
#'@details
#'
#'The function tries to match a name (Input.Taxon column) in LCVP, which has a
#'corresponding accepted valid name according to LCVP (Output.Taxon column). If
#'the Input.Taxon is a valid name, it will be the duplicated in Output.Taxon
#'column.
#'
#'The algorithm will first try to exactly match the binomial names provided in
#'`splist`. If no match is found, it will try to find the closest name given the
#'maximum distance defined in `max_distance`. If more than one name is exactly
#'or fuzzy matched, only the accepted or the first will be returned and a
#'warning message will be printed on the console. The list of input names that
#'matched multiple names in LCVP can be obtained using \code{attr(x,
#'"matched_mult")}, being x the resulting data.frame. The function
#'\code{\link[lcvplants:lcvp_fuzzy_search]{lcvp_fuzzy_search}} can then be used
#'to return all results of the algorithm.
#'
#'The \code{\link[lcvplants:lcvp_summary]{lcvp_summary}} function can be used to
#'summarize the results from a multiple species search, indicating the number of
#'species matched, and how many of them were exactly or fuzzy matched.
#'
#'Note that only binomial names with valid characters are allowed in this
#'function. Search based on  genus, family, order or author names should use the
#'function \code{\link[lcvplants:lcvp_group_search]{lcvp_group_search}}.
#'
#'
#'@return A data frame with the following columns:
#'   \describe{\item{global.Id}{The fixed species id of the input taxon in the
#'   Leipzig Catalogue of Vascular Plants (LCVP).} 
#'   \item{Input.Genus}{A
#'   character vector. The input genus of the corresponding vascular plant
#'   species name listed in LCVP.} 
#'   \item{Input.Epitheton}{A character vector.
#'   The input epitheton of the corresponding vascular plant species name listed
#'   in LCVP.} 
#'   \item{Rank}{A character vector. The taxonomic rank ("species",
#'   subspecies: "subsp.", variety: "var.", subvariety: "subvar.", "forma", or
#'   subforma: "subf.") of the corresponding vascular plant species name listed
#'   in LCVP.} 
#'   \item{Input.Subspecies.Epitheton}{A character vector. If the
#'   indicated rank is below species, the subspecies epitheton input of the
#'   corresponding vascular plant species name listed in LCVP. If the rank is
#'   "species", the input is "nil".} 
#'   \item{Input.Authors}{A character vector.
#'   The taxonomic authority input of the corresponding vascular plant species
#'   name listed in LCVP.} 
#'   \item{Status}{A character vector. description if a
#'   taxon is classified as ‘valid’, ‘synonym’, ‘unresolved’, ‘external’ or
#'   ‘blanks’. The ‘unresolved’ rank means that the status of the plant name
#'   could be either valid or synonym, but the information available does not
#'   allow a definitive decision. ‘External’ in an extra rank which lists names
#'   outside the scope of this publication but useful to keep on this updated
#'   list. ‘Blanks’ means that the respective name exists in bibliography but it
#'   is neither clear where it came from valid, synonym or unresolved. (see the
#'   main text Freiberg et al. for more details)}
#'   \item{globalId.of.Output.Taxon}{The fixed species id of the output taxon
#'   in LCVP.} 
#'   \item{Output.Taxon}{A character vector. The list of the accepted
#'   plant taxa names according to the LCVP.} 
#'   \item{Family}{A character vector.
#'   The corresponding family name of the Input.Taxon, staying empty if the
#'   Status is unresolved.} 
#'   \item{Order}{A character vector. The corresponding
#'   order name of the Input.Taxon, staying empty if the Status is unresolved.}
#'   \item{Literature}{A character vector. The bibliography used.}
#'   \item{Comments}{A character vector. Further taxonomic comments.}
#'  \item{\emph{Correct}}{: if show_correct = TRUE, this column is added to the
#'  final result indicating whether the binomial name was exactly matched
#'  (TRUE), or if it is misspelled (FALSE).} }
#'
#'See \code{\link[LCVP:tab_lcvp]{LCVP::tab_lcvp}} for more
#'details.
#'
#'If no match is found for one species it will return NA for the columns in the
#'LCVP table. But, if no match is found for all species the function will return
#'NULL and a warning message.
#'
#'@author Bruno Vilela & Alexander Ziska
#'
#'@seealso \code{\link[lcvplants:lcvp_summary]{lcvp_summary}},
#'\code{\link[lcvplants:lcvp_group_search]{lcvp_group_search}},
#'\code{\link[lcvplants:lcvp_fuzzy_search]{lcvp_fuzzy_search}}.
#'
#'@references Freiberg, M., Winter, M., Gentile, A. et al. LCVP, The Leipzig
#'catalogue of vascular plants, a new taxonomic reference list for all known
#'vascular plants. Sci Data 7, 416 (2020).
#'https://doi.org/10.1038/s41597-020-00702-z
#'
#'@keywords R-package nomenclature taxonomy vascular plants
#'
#' @examples
#' # Ensure that LCVP package is available before running the example.
#' # If it is not, see the `lcvplants` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("LCVP", quietly = TRUE)) { # Do not run this
#'
#' # Search one species
#' lcvp_search("Aa argyrolepis")
#'
#' # Search one species with misspelled name
#' lcvp_search("Aa argyrolepise", show_correct = TRUE)
#' lcvp_search("Aa argyrolepise", max_distance = 2)
#'
#' # Search for a variety
#' lcvp_search("Hibiscus abelmoschus var. betulifolius Mast.")
#'
#' # Search for multiple species
#' splist <- c(
#' "Hibiscus abelmoschus var. betulifolius Mast.",
#' "Hibiscus abutiloides Willd.",
#' "Hibiscus aculeatus",
#' "Hibiscus acuminatus",
#' "Hibiscus furcatuis" # This is a wrong name
#' )
#' mult <- lcvp_search(splist, max_distance = 0.2)
#'
#'  ## Results for multiple species search can be summarized using lcvp_summary
#' lcvp_summary(mult)
#'
#' }
#'@export

lcvp_search <- function(splist, 
                        max_distance = 0.2,
                        show_correct = FALSE,
                        genus_fuzzy = FALSE,
                        grammar_check = FALSE,
                        progress_bar = FALSE) {
  hasData() # Check if LCVP is installed
  # Defensive function here, check for user input errors
  if (is.factor(splist)) {
    splist <- as.character(splist)
  }
  .names_check(splist, "splist")
  
  # Fix species name
  splist_std <- .names_standardize(splist)
  
  # Classify splist
  splist_class <- .splist_classify(splist_std)
  
  # Check binomial
  .check_binomial(splist_class, splist)
  
  # Now match
  matching <- .match_algorithm(splist_class,
                               max_distance,
                               progress_bar = progress_bar, 
                               genus_fuzzy = genus_fuzzy,
                               grammar_check = grammar_check)
  
  # Elaborate the return object
  ## Return Null if it did not find anything
  if (all(is.na(matching))) {
    result_final <- NULL
  ## Return the matrix with matched species 
  } else {
    comb_match <- matching[, -(1:2), drop = FALSE]
    # keep homonyms to the warning
    ho_pos <- ncol(comb_match) 
    homonyms <- as.logical(comb_match[, ho_pos])
    homonyms[is.na(homonyms)] <- FALSE
    comb_match <- comb_match[, -ho_pos, drop = FALSE]
    
    comb_match <- as.matrix(apply(comb_match, 2, as.logical))
    
    if (ncol(comb_match) == 1) { # If only one column, need to be transposed
      comb_match <- t(comb_match)
    }
    # Transform in data.frame
    comb_match <- as.data.frame(comb_match) 
    names_col <-
      colnames(LCVP::lcvp_sps_class)[-c(1, ncol(LCVP::lcvp_sps_class))]
    
    colnames(comb_match) <- paste(names_col, "match", sep = "_")
    
    result_final <- data.frame("Search" = splist,
                               LCVP::tab_lcvp[matching[, 1], , drop = FALSE])
    
    # Add whether the searched name matched each class, 
    # will be used in the summary function
    attributes(result_final)$match.names <- comb_match  
    # Remove row names
    rownames(result_final) <- NULL
    # Warning more than one match
    if (any(homonyms)) {
      warning(
        paste0(
          "More than one name was matched for some species. ",
          "Only the first 'accepted' (if present) name was returned. ",
          "Consider using the function lcvp_fuzzy_search ",
          "to return all names for these species:\n",
          paste(result_final[homonyms, 1], collapse = ", ")
        ),
        call. = FALSE
      )
      attributes(result_final)$matched_mult <- result_final[homonyms, 1]
    }
  }
  
  # If no match, give a warning
  if (is.null(result_final)) {
    warning(paste0("No match found for the species list provided.",
                   " Try increasing the 'max_distance' argument."))
  } else {
    if (show_correct) {
      
      result_final$Correct <- rowSums(comb_match[, 1:2, drop = FALSE]) == 2
    }
  }
  return(result_final)
}
