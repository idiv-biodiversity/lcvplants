#' Solve duplicated names by summarizing traits
#'
#'
#' Solve duplicated species names by summarizing traits given user provided
#' functions for common classes of variables (numeric, character, and logical).
#' 
#' @param x data.frame.
#' 
#' @param duplicated_col The number of the column position with duplicated names
#'  to be solved.
#' 
#' @param fixed_cols The columns positions that should be left out of the 
#' summarizing processes. Normally applies for columns with fixed values across 
#' repeated names. 
#' 
#' @param func_numeric A function to summarize numeric columns 
#' if solve_duplicated = TRUE. Default will return the mean.
#' 
#' @param func_character A function to summarize character or factor columns 
#' if solve_duplicated = TRUE. Default will keep all unique strings separated 
#' by comma.
#' 
#' @param func_logical A function to summarize logical columns 
#' if solve_duplicated = TRUE.Default will return TRUE if any is TRUE.
#' 
#' @details 
#' The function will combine lines in \code{x} with duplicated names found in 
#' \code{duplicated_col}. User-defined functions to combine the information in 
#' \code{x} should take a vector (of length > 2) of the corresponding class
#'  (numeric, character, and logical) and output only one value of the 
#'  corresponding class. Factors are transformed into characters. 
#'  
#' @return 
#' A data.frame with the same number of columns in  \code{x} 
#' and combined duplicated lines according to functions provided.
#'  
#' @keywords R-package nomenclature taxonomy vascular plants 
#' 
#' @author 
#' Bruno Vilela & Alexander Ziska
#' 
#' @examples
#' # Ensure that LCVP package is available before running the example.
#' # If it is not, see the `lcvplants` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("LCVP", quietly = TRUE)) { # Do not run this
#' 
#' # Create a data.frame with duplicated names and different traits
#' splist <- sample(LCVP::tab_lcvp$Input.Taxon[1:100])
#' search <- lcvp_search(splist)
#' 
#' x <- data.frame("Species" = search$Output.Taxon,
#' "Trait1" = runif(length(splist)),
#' "Trait2" = sample(c("a", "b"), length(splist), replace = TRUE),
#' "Trait3" = sample(c(TRUE, FALSE), length(splist), replace = TRUE))
#'
#' # Solve with default parameters
#' lcvp_solve_dups(x, 1)
#'
#' # Summarize numbers using the median
#' lcvp_solve_dups(x, 1, func_numeric = median)
#' 
#' # Get one of characters at random
#' lcvp_solve_dups(x, 1, func_character = function(x){sample(x, 1)})
#' 
#' }
#'@export


lcvp_solve_dups <- function(x, 
                            duplicated_col,
                            fixed_cols = NULL,
                            func_numeric = mean, 
                            func_character = .keep_all,
                            func_logical = any) {
  # Defensive
  .check_funcs(func_numeric,
               func_character,
               func_logical)
  .check_x(x)
  
  # Which dups
  dups <- .find_dups(x, output_pos = duplicated_col)
  
  # Transform factors in characters
  i <- sapply(x, is.factor)
  x[i] <- lapply(x[i], as.character)
  
  
  # Loop to solve
  n <- length(dups)
  for (i in 1:n) {
    # If not NA, solve it
    if (!is.na(dups[i])) {
      # Get position i dups 
      pos <- as.numeric(unlist(strsplit(dups[i], ",")))
      
      # Select traits
      fixed_cols <- c(duplicated_col, fixed_cols)
      traits <- x[pos, -(fixed_cols), drop = FALSE]
      
      # Summarize them 
      x[i, -(fixed_cols)] <- .trait_summary(traits, 
                                     func_numeric, 
                                     func_character,
                                     func_logical)
    }
  }
  # Remove dups
  result <- x[!duplicated(x[, duplicated_col], incomparables = NA), ]
  return(result)
}

#-------------------------------------------------------#
# summary traits based on functions provied
.trait_summary <- function(traits, 
                           func_numeric,
                           func_character,
                           func_logical) {
  
  # Loop to apply the defined functions to solve each column class
  n_col <- ncol(traits)
  solved <- traits[1, , drop = FALSE]
  for(i in 1:n_col) {
    if (is.numeric(traits[, i])) {
      solved[, i] <- func_numeric(traits[, i]) # make this
    }
    if (is.character(traits[, i])) {
      solved[, i] <- func_character(traits[, i])
    }
    if (is.logical(traits[, i])) {
      solved[, i] <- func_logical(traits[, i])
    }
  }
  
  return(solved)
}

#-------------------------------------------------------#
# Find duplicates and identify their position
.find_dups <- function(x, output_pos = 4) {
  # Identify dups
  dups <- duplicated(x[, output_pos], incomparables = NA) |
    duplicated(x[, output_pos], fromLast = TRUE, incomparables = NA)
  # Loop to find which duplicates which
  n <- length(dups)
  dups_which <- numeric(n)
  for (i in 1:n) {
    if (dups[i]) {
      # Give the position of the dups
      dups_which[i] <-
        paste(which(x[, output_pos] == x[i, output_pos]), collapse = ", ")
    } else {
      # If not dup, NA
      dups_which[i] <- NA
    }
  }
  return(dups_which)
}