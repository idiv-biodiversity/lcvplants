# Check input
.checksplist <- function(splist) {
  
  # Check if it is a character
  if(!is.character(splist) | !is.vector(splist)) {
    stop(paste0("obejct 'splist' should be character vector, not '", 
                paste(class(splist), collapse = " "), "'"))
  }
  enc_valid <- !validEnc(splist)
  
  # Check if it has invalid encoding
  if(any(enc_valid)) {
    stop(paste("'splist' should include only valid characters,",
               "please check the name(s) at position(s):",
               paste(which(enc_valid), collapse = ", ")))
  }
}

