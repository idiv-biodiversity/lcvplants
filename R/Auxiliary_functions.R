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
  # Merge multiple spaces
  fixed6 <- gsub("(^X )|( X$)|( X )", " ", fixed5)
  fixed7 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", fixed6, perl=TRUE)
  return(fixed7)
}

#-------------------------------------------------------#
# Function default lcvp_join argument func_character
# It returns all names separated by comma. 
.keep_all <- function (x) {
  return(paste(unique(x), collapse = ", "))
}