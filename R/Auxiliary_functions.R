# Make names standard
.names_standardize <- function(splist) {
  fixed <- toupper(splist) # all up
  fixed2 <- trimws(fixed) # remove trailing and leading space
  fixed3 <- gsub("_", " ", fixed2) # change names separated by _ to space
  return(fixed3)
}


