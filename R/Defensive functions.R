# Check input
.checksplist <- function(splist) {
  
  # Check if it is a character
  if(!is.character(splist)) {
    stop(paste0("obect 'splist' class should be 'character', not '", 
                class(splist), "'"))
  }
  enc_valid <- !validEnc(splist)
  
  # Check if it has invlaid encoding
  if(any(enc_valid)) {
    stop(paste("'splist' should include only valid characters,",
               "please check the name(s) at position(s):",
               paste(which(enc_valid), collapse = ", ")))
  }
}

