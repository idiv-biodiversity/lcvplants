#-------------------------------------------------------#
# Function to check list of names input
.names_check <- function(splist, 
                          argument_name) {
  
  # Check if it is a character
  if (!is.character(splist) | !is.vector(splist)) {
    stop(paste0("argument '", argument_name, 
                "' should be character vector, not '", 
                paste(class(splist), collapse = " "), "'"),
         call. = FALSE)
  }
  enc_valid <- !validEnc(splist)
  
  # Check if it has invalid encoding
  if (any(enc_valid)) {
    stop(paste(argument_name, 
               "should include only valid characters,",
               "please check the name(s) at position(s):",
               paste(which(enc_valid), collapse = ", ")),
         call. = FALSE)
  }
}

#-------------------------------------------------------#
# Check the search_by in lcvp_group 
.search_by_check <- function(search_by) {
  
  cats <- c("Genus", "Family", "Order", "Author")
  check <- search_by %in% cats
  if (!check) {
    stop(paste0("search_by argument should be one of the following: ",
                paste0("'", cats, "'", collapse = ", "), ". Not '", search_by, "'"),
         call. = FALSE)
  }
}

#-------------------------------------------------------#
# Check if names are binomial
.check_binomial <- function(splist_class, splist) {
  
  missing_bino <- which(apply(splist_class[, 2:3, drop = FALSE], 
                              1,
                              function(x) {any(is.na(x))}))
  if (length(missing_bino) > 0) {
    stop(paste0("splist should include only binomial names,",
                " please check the following names: ",
                paste(paste0("'", splist[missing_bino], "'"), collapse = ", ")),
         call. = FALSE)
    
  }
}

#-------------------------------------------------------#
# Check inputs for hoin

.check_join <- function(x, y, sp_columns) {
  # Check classes
  class_x <- class(x)
  if (class_x != "data.frame") {
    stop(paste0("x should be a data.frame, not '", class_x, "'."),
         call. = FALSE)
  }
  class_y <- class(y)
  if (class_y != "data.frame") {
    stop(paste0("y should be a data.frame, not '", class_x, "'."),
         call. = FALSE)
  }
  
  class_sp <- class(sp_columns)
  if (class_sp != "character") {
    stop(paste0("sp_columns should be a character, not '", class_sp, "'."),
         call. = FALSE)
  }
  
  # Length names
  n_sp_columns <- length(sp_columns)
  if (n_sp_columns != 2) {
    stop(paste0("sp_columns should include 2 characters. Not ",
                n_sp_columns, "."),
         call. = FALSE)
  }
  
  # Check if names correspond to columns
  if (!sp_columns[1] %in% colnames(x)) {
    stop(paste0("First name in sp_columns '", sp_columns[1], "'",
                " not found in x columns names."), 
         call. = FALSE)
  }
  if (!sp_columns[2] %in% colnames(y)) {
    stop(paste0("Second name in sp_columns '", sp_columns[2], "'",
                " not found in y columns names."), 
         call. = FALSE)
  }
    
}

