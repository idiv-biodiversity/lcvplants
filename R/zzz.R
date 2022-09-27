# Functions to check LCVP package dependence
.pkgenv <- new.env(parent = emptyenv())

.onLoad  <- function(libname, pkgname) {
  has_data <- requireNamespace("LCVP", quietly = TRUE)
  if (has_data) {
    has_data <- has_data & utils::packageVersion("LCVP") >= "3.0" # change accordingly
  }
  .pkgenv[["has_data"]] <- has_data
}

.onAttach <- function(libname, pkgname) {
  if (!.pkgenv$has_data) {
    msg <- paste("To use this package, you must install the",
                 "LCVP package version 3.0 or higher. To install LCVP package,", 
                 "run `devtools::install_github('idiv-biodiversity/LCVP')`.",
                 "See the `LCVP` vignette for more details.")
    msg <- paste(strwrap(msg), collapse = "\n")
    packageStartupMessage(msg)
  }
}

hasData <- function() {
  has_data <- requireNamespace("LCVP", quietly = TRUE)
  if (has_data) {
    has_data <- has_data & utils::packageVersion("LCVP") >= "3.0" # change accordingly
  }
  if (!has_data) {
    msg <- paste("To use this function, you must have the",
                 "`LCVP` package version 3.0 or higher installed.",
                 "To install LCVP package, run",
                 "`devtools::install_github('idiv-biodiversity/LCVP')`.",
                 "See the `LCVP` package vignette for more details.")
    msg <- paste(strwrap(msg), collapse = "\n")
    stop(msg)
  }
}

