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
#' @param func_numeric A function to summarize numeric columns 
#' if solve_duplicated = TRUE.
#' @param func_character A function to summarize character columns 
#' if solve_duplicated = TRUE.
#' @param func_logical A function to summarize logical columns 
#' if solve_duplicated = TRUE.
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
#' lcvp_join(x, y, c("Species", "Species"), type = "inner")
#' }
#'@export



lcvp_join <- function(x, y, 
                      sp_columns, 
                      max.distance = 0.1,
                      type = "full",
                      solve_duplicated = FALSE,
                      func_numeric = mean, 
                      func_character = .keep_all,
                      func_logical = any) {
  
  # Defensive here
  .check_join(x, y, sp_columns)
  
  # Get names
  splist1 <- x[, sp_columns[1], drop = TRUE]
  splist2 <- y[, sp_columns[2], drop = TRUE]
  
  # lcvp_match
  match_result <- lcvp_match(splist1, splist2, include_all = TRUE,
                             identify_dups = FALSE)
  
  # Adjust tables to join
  y2 <- y[match_result$Match.Position.2to1, , drop = FALSE]
  n_x <- nrow(x)
  n_y2 <- nrow(y2)
  x2 <- x[c(1:n_x, rep(NA, (n_y2 - n_x))), ]
  
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
    both <- !is.na(result$Species.List.2) & !is.na(result$Species.List.1)
    if (all(!both)) {
      stop("No match was found between the two tables.")
    }
    result <- result[both, , drop = FALSE]
  }
  
  if (solve_duplicated) {
    result <- .solve_dups(result,
                           func_numeric, 
                           func_character,
                           func_logical)
  }
  
  return(result)
}

#-------------------------------------------------------#
# Solve for duplicated names  
.solve_dups <- function(x, 
                        func_numeric, 
                        func_character,
                        func_logical) {
  # Which dups
  dups <- .find_dups(x)
  
  # Loop to solve
  n <- length(dups)
  for (i in 1:n) {
    # If not NA, solve it
    if (!is.na(dups[i])) {
      # Get position i dups 
      pos <- as.numeric(unlist(strsplit(dups[i], ",")))
      
      # Select traits
      traits <- x[pos, -(1:4), drop = FALSE]
      
      # Summarize them 
      x[i, -(1:4)] <- .trait_summary(traits, 
                               func_numeric, 
                               func_character,
                               func_logical)
    }
  }
  # Remove dups
  result <- x[!duplicated(x$LCVP.Output.Taxon), ]
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


