#' Join two data.frames using the Leipzig Catalogue of Plants (LCVP)
#'
#'
#' Join two data.frames using the Leipzig Catalogue of Plants (LCVP)
#'
#' @param x,y data.frames to join.
#'
#' @param sp_columns A character vector indicating the column names in x and in y
#' with respective species names to join by.
#' For example, c("species", "Species_name").
#'
#' @param max.distance It represents the maximum distance allowed for a match
#' when comparing the submitted name with the closest name matches in the LCVP.
#' Expressed either as integer, or as a fraction of the pattern length times the maximal
#' transformation cost (will be replaced by the smallest integer not less than
#' the corresponding fraction). See \code{\link[base]{agrep}} for more details.
#'
#' @param type What type of join should be done: "full" (default), "left", "right"
#' or "inner".
#' *"full" return all rows and all columns from both x and y.
#' *"left" return all rows from x.
#' *"right" return all rows from y.
#' *"inner" return all rows from x where there are matching species in y.
#'
#' @param solve_duplicated if TRUE, it will summarize duplicated output names
#' given a function for each column class.
#' 
#' @param func_numeric A function to summarize numeric columns 
#' if solve_duplicated = TRUE. Default will return the mean.
#' 
#' @param func_character A function to summarize character columns 
#' if solve_duplicated = TRUE. Default will keep all unique strings separated 
#' by comma.
#' 
#' @param func_logical A function to summarize logical columns 
#' if solve_duplicated = TRUE.Default will return TRUE if any is TRUE.
#' 
#'
#'@examples \dontrun{
#' # data.frame1
#' splist1 <- sample(LCVP::tab_lcvp$Input.Taxon[2:100])
#' x <- data.frame("Species" = splist1, "Trait1" = runif(length(splist1)))
#'
#' # data.frame2
#' splist2 <- sample(LCVP::tab_lcvp$Input.Taxon[98:8])
#' y <- data.frame("Species" = splist2,
#' "Trait2" = runif(length(splist2)),
#' "Trait3" = runif(length(splist2)),
#' "Trait4" = sample(c("a", "b"), length(splist2), replace = TRUE),
#' "Trait5" = sample(c(TRUE, FALSE), length(splist2), replace = TRUE))
#'
#'
#' lcvp_join(x, y, c("Species", "Species"), type = "full")
#'
#' lcvp_join(x, y, c("Species", "Species"), type = "left")
#'
#' lcvp_join(x, y, c("Species", "Species"), type = "right")
#'
#' lcvp_join(x, y, c("Species", "Species"), 
#' type = "inner", solve_duplicated = TRUE)
#' 
#' }
#'@export



lcvp_join <- function(x,
                      y,
                      sp_columns,
                      max.distance = 0.1,
                      type = "full",
                      solve_duplicated = FALSE,
                      func_numeric = mean,
                      func_character = .keep_all,
                      func_logical = any) {
  # Defensive here
  .check_join(x, y, sp_columns)
  .check_funcs(func_numeric,
               func_character,
               func_logical)
  
  # Transform factors in characters
  i <- sapply(x, is.factor)
  x[i] <- lapply(x[i], as.character)
  
  
  # Get names
  splist1 <- x[, sp_columns[1], drop = TRUE]
  splist2 <- y[, sp_columns[2], drop = TRUE]
  
  # lcvp_match
  match_result <- lcvp_match(splist1,
                             splist2,
                             include_all = TRUE,
                             identify_dups = FALSE)
  
  # Adjust tables to join
  y2 <- y[match_result$Match.Position.2to1, , drop = FALSE]
  n_x <- nrow(x)
  n_y2 <- nrow(y2)
  x2 <- x[c(1:n_x, rep(NA, (n_y2 - n_x))),]
  
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
                              remove_cols = 1:4,
                              func_numeric = func_numeric,
                              func_character = func_character,
                              func_logical = func_logical)
  }
  
  return(result)
}
