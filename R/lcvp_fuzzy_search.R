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
#' @param max.distance It represents the maximum distance allowed for a match
#' when comparing the submitted name with the closest name matches in the LCVP.
#' Expressed either as integer, or as a fraction of the pattern length times the maximal
#' transformation cost (will be replaced by the smallest integer not less than
#' the corresponding fraction). See \code{\link[base]{agrep}} for more details.
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
#' @details 
#' 
#' The algorithm will look for all the names within the given maximum distance 
#' defined in `max.distance`.
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
#' \item{\emph{Search}}{: Taxa name list provided by the user.}
#' \item{\emph{Input.Taxon}}{: Matched taxa names listed in the LCVP data.}  
#' \item{\emph{Status}}{: Nomenclature status: 'accepted', 'synonym', 
#' 'unresolved' or 'external'.}
#' \item{\emph{PL.comparisson}}{: This field provides a direct comparison with
#' ‘The Plant List’: ‘identical', 'synonym', 'other synonym', 
#' 'different authors', 'missing', 'misspelling' or 'unresolved'.}  
#' \item{\emph{PL.alternative}}{: This field provides a possible alternative 
#' name from ‘The Plant List’.}
#' \item{\emph{Output.Taxon}}{: The list of the accepted plant taxa names 
#' according to the LCVP.}  
#' \item{\emph{Family}}{: The corresponding family name of the Input.Taxon, 
#' staying empty if the Status is unresolved.}  
#' \item{\emph{Order}}{: The corresponding order name of the Input.Taxon,
#'  staying empty if the Status is unresolved.}
#' \item{\emph{Name.Distance}}{The approximate string distance between the Search 
#' and matched Input.Taxon names. See \code{\link[utils:adist]{utils:adist}}
#' for more details.}
#' }
#' See \code{\link[LCVP:tab_lcvp]{LCVP:tab_lcvp}} for more details.
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
#' 
#' # Returns a data.frame
#' lcvp_fuzzy_search(c("Hibiscus vitifolia", "Adansonia digitata"))
#' 
#' # Returns a list of data.frames
#' lcvp_fuzzy_search(c("Hibiscus vitifolia", "Adansonia digitata"),
#' bind_result = FALSE)
#' 
#' # Returns only accepted names
#' lcvp_fuzzy_search("Hibiscus vitifolia", status = "accepted")
#' 
#'@export



lcvp_fuzzy_search <- function(splist,
                              max.distance = 0.1,
                              status = c("accepted",
                                         "synonym",
                                         "unresolved",
                                         "external"),
                              bind_result = TRUE) {
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
  for (i in 1:n_sps) {
    result [[i]] <-
      .lcvp_fuzzy_search_ind(species_class[i, , drop = FALSE],
                            max.distance,
                            status)
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
                              max.distance,
                              status) {
  
  
  if (is.na(species_class[, 3])) {
    warning(paste0("'", species_class[, 1], "' does not include an epithet."),
            call. = FALSE)
    return(NA)
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
      rownames(result) <- NULL
      return(result)
    } else {
      warning(paste0("No match found for ", "'", species_class[, 1], "'."),
              call. = FALSE)
      return(NA)
    }
  }
}
