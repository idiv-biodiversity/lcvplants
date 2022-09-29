# Small functions to support main functions
# Author: Bruno Vilela

#-------------------------------------------------------#
# Make names standard
.names_standardize <- function(splist) {
  fixed1 <- toupper(splist) # all up
  fixed2 <- gsub("CF\\.", "", fixed1)
  fixed3 <- gsub("AFF\\.", "", fixed2)
  fixed4 <- trimws(fixed3) # remove trailing and leading space
  fixed5 <- gsub("_", " ", fixed4) # change names separated by _ to space
  
  # Hybrids 
  fixed6 <- gsub("(^X )|( X$)|( X )", " ", fixed5)
  hybrids <- fixed5 == fixed6
  if (!all(hybrids)) {
    sp_hybrids <- splist[!hybrids]
    warning(paste("The 'x' sign indicating hybrids have been removed in the",
                  "following names before search:", 
                  paste(paste0("'", sp_hybrids, "'"), collapse = ", ")),
            immediate. = TRUE)
  }
  # Merge multiple spaces
  fixed7 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", fixed6, perl = TRUE)
  return(fixed7)
}

#-------------------------------------------------------#
# Function default lcvp_join argument func_character
# It returns all names separated by comma. 
.keep_all <- function(x) {
  return(paste(unique(x), collapse = ", "))
}

#-------------------------------------------------------#
# Function to match the closest fuzzy name
.agrep_whole <- function(x, y, max_distance) {
  if (max_distance < 1 & max_distance > 0) {
    max_distance <- ceiling(nchar(x) * max_distance)
  } 
  a <- utils::adist(x, y)
  return(which(a <= max_distance))
}
