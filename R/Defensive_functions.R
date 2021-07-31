#-------------------------------------------------------#
# Function to check list of names input
.names_check <- function(splist, 
                          argument_name) {
  
  # Check if it is a character
  if (!is.character(splist) | !is.vector(splist)) {
    stop(paste0("argument '", argument_name, 
                "' should be character vector, not '", 
                paste(class(splist), collapse = " "), "'"))
  }
  enc_valid <- !validEnc(splist)
  
  # Check if it has invalid encoding
  if (any(enc_valid)) {
    stop(paste(argument_name, 
               "should include only valid characters,",
               "please check the name(s) at position(s):",
               paste(which(enc_valid), collapse = ", ")))
  }
}

#-------------------------------------------------------#
# Check the search_by in lcvp_group 
.search_by_check <- function(search_by) {
  
  cats <- c("Genus", "Family", "Order", "Author")
  check <- search_by %in% cats
  if (!check) {
    stop(paste0("search_by argument should be one of the following:",
                paste(cats, collapse = ", ")), "; not '", search_by, "'")
  }
}

