#' Standardize plant names according to the Leipzig Catalogue of Plants (LCVP)
#' 
#' This is the new version of the LCVP function 

#' @param splist A character vector specifying the input taxon, each element
#' including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name and author name
#' 
#'@export



LCVP2 <- function(splist) {
  
  # Defensive function here, check for user input errors
  .checksplist(splist)
  
  # Fix species name 
  splist_std <- .standardize(splist)
  
  # Columns position 
  # (this helps with future changes in column position)
  species_position <- 1
  
  #------- This should go to LCVP package
  # ATENTION QUICK FIX OF THE NAMES
  remove_for_now <- c(258521, 669979, 1133520) 
  
  # All upper case to avoid problems
  lcvp_sps <- .standardize(LCVP::tab_lcvp[-remove_for_now,
                                          species_position]) # ATENTION QUICK FIX OF THE NAMES
  
  lcvp_sps_class <- .splist_classify(lcvp_sps)
  #--------------------------------------------
  
  # Classify splist
  splist_class <- .splist_classify(splist_std)
  
  # Now match
  # Match function here
} 

