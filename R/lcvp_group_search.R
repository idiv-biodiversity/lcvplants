#' Search plants by taxa or author names in the Leipzig Catalogue of Plants (LCVP)
#'
#' Search all plant taxa names listed in the "Leipzig
#' Catalogue of Vascular Plants" (LCVP) by order, family, 
#' genus or author. 
#'
#' @param group_names A character vector specifying the taxa or author names.
#' This includes names of orders, families, genus or authors. Only valid 
#' characters are allowed (see \code{\link[base:validEnc]{validEnc}}).
#'
#' @param search_by A character indicating whether to search by "Order",
#' "Family", "Genus" or "Author".
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
#' @param bind_result If TRUE the function will return one data.frame (default).
#' If False, the function will return a list of separate data.frames for
#' each input group.
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
#' @details 
#' 
#' The algorithm will look for all the plant taxa names listed in the "Leipzig
#' Catalogue of Vascular Plants" (LCVP) based on a user-specified list of 
#' orders,  families, genus, or authors names. If no match is found, it will try 
#' to find the closest name given the maximum distance defined in `max_distance`. 
#' If more than one name is fuzzy matched, only the first will be returned.
#' 
#' @return 
#' A data.frame or a list of data.frames (if \code{bind_result = FALSE}) 
#' with the following columns for all species of the matched groups:
#' 
#'   \describe{#'   \item{global.Id}{The fixed species id of the input taxon in the
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
#'   \item{Comments}{A character vector. Further taxonomic comments.}}
#'   
#' See \code{\link[LCVP:tab_lcvp]{LCVP:tab_lcvp}} for more details.
#' 
#' If no match is found for all searched names the function will 
#' return NULL and a warning message.
#' 
#' @author 
#' Bruno Vilela & Alexander Ziska
#' 
#' @seealso 
#' \code{\link[lcvplants:lcvp_search]{lcvp_search}},   
#' \code{\link[lcvplants:lcvp_fuzzy_search]{lcvp_fuzzy_search}}.
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
#' # Search by Genus
#' lcvp_group_search(c("AA", "Adansonia"), search_by = "Genus")
#' 
#' # Search by Author and keep only accepted names
#' lcvp_group_search("Schltr.", search_by = "Author", status = "accepted")
#'
#' }
#'@export



lcvp_group_search <- function(group_names,
                              search_by,
                              max_distance = 0.2,
                              bind_result = TRUE,
                              status = c("accepted",
                                         "synonym",
                                         "unresolved",
                                         "external")) {
  hasData() # Check if LCVP is installed
  # Check names
  if (is.factor(group_names)) {
    group_names <- as.character(group_names)
  }
  .names_check(group_names, "group_names")
  
  .search_by_check(search_by)
  
  .check_status(status)
  # Fix entry names
  group_names <- .names_standardize(group_names)
  
  # Get position in the table based on group
  if (search_by == "Genus") {
    ref_names <- LCVP::tab_position$Genus
  }
  
  if (search_by == "Family") {
    ref_names <- names(LCVP::lcvp_family)
  }
  
  if (search_by == "Order") {
    ref_names <- names(LCVP::lcvp_order)
  }
  
  if (search_by == "Author") {
    # not ready yet
    ref_names <- names(LCVP::lcvp_authors)
  }
  # Get the position list
  pos_group <- .lcvp_group(group_names,
                           ref_names,
                           max_distance)
  if (all(is.na(pos_group))) {
    return(NULL)
  } else {
    # Get position list based on the group used
    if (search_by == "Genus") {
      pos_list <- .genus_search_multiple(pos_group)
    }
    
    if (search_by == "Family") {
      pos_list <- LCVP::lcvp_family[pos_group]
    }
    
    if (search_by == "Order") {
      pos_list <- LCVP::lcvp_order[pos_group]
    }
    
    if (search_by == "Author") {
      # not ready yet
      pos_list <- LCVP::lcvp_authors[pos_group]
    }
    
    # Return the actual data.frames
    result <- list()
    
    for (i in 1:length(pos_list)) {
      result[[i]] <- LCVP::tab_lcvp[pos_list[[i]],]
      rownames(result[[i]]) <- NULL
      if (!all(c("accepted", "synonym", "unresolved", "external") %in% status)) {
        result[[i]] <-
          result[[i]][result[[i]]$Status %in% status, , drop = FALSE]
      }
    }
    
    # Bind the results or not
    if (bind_result) {
      result <- do.call(rbind, result)
      if (nrow(result) == 0) {
        return(NULL)
      }
    } else {
      names(result) <- ref_names[pos_group]
    }
    return(result)
  }
}

