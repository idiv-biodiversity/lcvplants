#'Join two data.frames using the Leipzig Catalogue of Plants (LCVP)
#'
#'
#'Join two data.frames based on the taxonomic resolution of plant taxa names
#'listed in the "Leipzig Catalogue of Vascular Plants" (LCVP). Inspired by the
#'\code{\link[dplyr:join]{dplyr:join}} function.
#'
#'@param x,y data.frames to join.
#'
#'@param sp_columns A character vector indicating the column names in x and y
#'  with respective species names to join by.
#'
#'  For example, c("species", "Species_name").
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
#'@param genus_fuzzy If TRUE, the fuzzy match algorithm based on max_distance
#'  will also be applied to the genus (note that this may considerably increase
#'  computational time). If FALSE, fuzzy match will only apply to the epithet.
#'  
#'@param grammar_check if TRUE, the algorithm will try to fix common latin 
#'grammar mistakes.
#'
#'@param type What type of join should be done: "full" (default), "left",
#'  "right" or "inner". * "full" return all rows and all columns from both x and
#'  y. * "left" return all rows from x. * "right" return all rows from y. *
#'  "inner" return all rows from x where there are matching species in y.
#'
#'@param solve_duplicated if TRUE, it will summarize duplicated output names
#'  given a function for each column class.
#'
#'  See \code{\link[lcvplants:lcvp_solve_dups]{lcvp_solve_dups}} for details.
#'
#'@param func_numeric A function to summarize numeric columns if
#'  solve_duplicated = TRUE. Default will return the mean.
#'
#'@param func_character A function to summarize character columns if
#'  solve_duplicated = TRUE. Default will keep all unique strings separated by
#'  comma.
#'
#'@param func_logical A function to summarize logical columns if
#'  solve_duplicated = TRUE. Default will return TRUE if any is TRUE.
#'
#'@details The function add the columns from y to x based on the list of species
#'name in both tables. It first standardizes the species names in both tables
#'based on the "Leipzig Catalogue of Vascular Plants" (LCVP) using the algorithm
#'in \code{\link[lcvplants:lcvp_search]{lcvp_search}}. Note that
#'\code{lcvp_join} can also deal with misspelling by fuzzy matching species name
#'given a \code{max_distance} choice. These standardized names of both tables
#'are then matched using the algorithm in
#'\code{\link[lcvplants:lcvp_match]{lcvp_match}}. The type "full" join will keep
#'all species and add NAs to missing values. No NA is added in "inner", "left"
#'and "right" options.
#'
#'Duplicated taxonomic resolution may occur if two inputs are now synonyms. If
#'\code{solve_duplicated} is \code{TRUE} the
#'\code{\link[lcvplants:lcvp_match]{lcvp_solve_dups}} function is applied to
#'merge duplicated output names.
#'
#'
#'@return A data.frame with the columns in both tables. The rows will depend on
#'the \code{type} selected. For "inner", a subset of x rows. For "left", all x
#'rows. For "right", a subset of x rows, followed by unmatched y rows. For
#'"full", all x rows, followed by unmatched y rows.
#'
#'@author Bruno Vilela & Alexander Ziska
#'
#'@seealso \code{\link[lcvplants:lcvp_search]{lcvp_search}},
#'\code{\link[lcvplants:lcvp_match]{lcvp_match}},
#'\code{\link[lcvplants:lcvp_solve_dups]{lcvp_solve_dups}}.
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
#' # Create data.frame1
#' splist1 <- sample(apply(LCVP::tab_lcvp[2:10, 2:3], 1, paste, collapse = " "))
#' x <- data.frame("Species" = splist1, "Trait1" = runif(length(splist1)))
#'
#' # Create data.frame2
#' splist2 <-sample(apply(LCVP::tab_lcvp[11:3, 2:3], 1, paste, collapse = " "))
#' y <- data.frame("Species" = splist2,
#' "Trait2" = runif(length(splist2)),
#' "Trait3" = runif(length(splist2)),
#' "Trait4" = sample(c("a", "b"), length(splist2), replace = TRUE),
#' "Trait5" = sample(c(TRUE, FALSE), length(splist2), replace = TRUE))
#'
#' # Full join
#' lcvp_join(x, y, c("Species", "Species"), type = "full")
#'
#' # Left join
#' lcvp_join(x, y, c("Species", "Species"), type = "left")
#'
#' # Right join
#' lcvp_join(x, y, c("Species", "Species"), type = "right")
#'
#' # Inner join and solve duplicates
#' lcvp_join(x, y, c("Species", "Species"),
#' type = "inner", solve_duplicated = TRUE)
#'
#' }
#'@export



lcvp_join <- function(x,
                      y,
                      sp_columns,
                      max_distance = 0.2,
                      genus_fuzzy = FALSE,
                      grammar_check = FALSE,
                      type = "full",
                      solve_duplicated = FALSE,
                      func_numeric = mean,
                      func_character = .keep_all,
                      func_logical = any) {
  hasData() # Check if LCVP is installed
  # Defensive here
  .check_join(x, y, sp_columns, type)
  .check_funcs(func_numeric,
               func_character,
               func_logical)
  
  # Transform factors in characters
  i <- vapply(x, is.factor, logical(1))
  x[i] <- lapply(x[i], as.character)
  
  
  # Get names
  splist1 <- x[, sp_columns[1], drop = TRUE]
  splist2 <- y[, sp_columns[2], drop = TRUE]
  
  # lcvp_match
  match_result <- lcvp_match(splist1,
                             splist2,
                             max_distance = max_distance,
                             genus_fuzzy = genus_fuzzy,
                             grammar_check = genus_fuzzy,
                             include_all = TRUE,
                             identify_dups = FALSE)[, -4] #remove status
  
  # Adjust tables to join
  y2 <- y[match_result$Match.Position.2to1, , drop = FALSE]
  n_x <- nrow(x)
  n_y2 <- nrow(y2)
  x2 <- x[c(seq_len(n_x), rep(NA, (n_y2 - n_x))),]
  
  # Join
  result <-
    data.frame(
      subset(match_result, select = -get("Match.Position.2to1")),
      subset(x2, select = -get(sp_columns[1])),
      subset(y2, select = -get(sp_columns[2]))
    )
  
  if (type == "left") {
    result <- result[!is.na(result$Species.List.1), , drop = FALSE]
  }
  if (type == "right") {
    result <- result[!is.na(result$Species.List.2), , drop = FALSE]
  }
  if (type == "inner") {
    both <-
      !is.na(result$Species.List.2) & !is.na(result$Species.List.1)
    if (all(!both)) {
      stop("No match was found between the two tables.")
    }
    result <- result[both, , drop = FALSE]
  }
  
  if (solve_duplicated) {
    result <- lcvp_solve_dups(result,
                              duplicated_col = 4,
                              fixed_cols = 1:3,
                              func_numeric = func_numeric,
                              func_character = func_character,
                              func_logical = func_logical)
  }
  
  return(result)
}
