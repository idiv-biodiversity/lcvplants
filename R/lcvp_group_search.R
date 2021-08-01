#' Search plants by group in the Leipzig Catalogue of Plants (LCVP)
#'
#' Search plants by order, family, genus or author in the
#' Leipzig Catalogue of Plants (LCVP) 
#' 
#' @param group_names A character vector specifying the group names. 
#' This includes names of order, family, genus or authors.
#' @param search_by A character indicating whether to search by order ("Order"), 
#' family ("Family"), genus ("Genus") or authors ("Authors").
#' @param max.distance is an integer value. It represents the maximum distance
#' (number of characters) allowed for a match when comparing the submitted name
#' with the closest name matches in the LCVP
#' @param bind_result If TRUE the function will return one data.frame. 
#' If False, the function will return a list of separate data.frames for 
#' each input group.
#'
#' @examples \dontrun{
#' 
#' res <- lcvp_group_search("AA", search_by = "Genus")
#' }
#'@export



lcvp_group_search <- function(group_names, 
                              search_by,
                              max.distance = 0.1,
                              bind_result = TRUE) {
  
  # Check names
  .names_check(group_names, "group_names")
  
  .search_by_check(search_by)
  
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
  
  if (search_by == "Authors") { # not ready yet
    ref_names <- NA
  }
  # Get the position list
  pos_group <- .lcvp_group(group_names,
                          ref_names,
                          max.distance)
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
  
  if (search_by == "Authors") { # not ready yet
    pos_list <- NA
  }
  
  # Return the actual data.frames
  result <- list()
  
  for (i in 1:length(pos_list)) {
    result[[i]] <- LCVP::tab_lcvp[pos_list[[i]], ]
  }
  
  
  if (bind_result) {
    result <- do.call(rbind, result)
  } else {
    names(result) <- ref_names[pos_group]
  }
  
  return(result)
}
