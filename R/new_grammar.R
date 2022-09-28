# Find a pattern at the end of the character
.find_mat <- function(x, pattern) {
  n_c <- nchar(x)
  n_p <- nchar(pattern) - 1
  return(substr(x, n_c - n_p, n_c) == pattern)
}

# find lati or late
.sub_lat <- function(x) {
  if (all(substr(x, 1, 4) %in% c("LATE", "LATI"))) {
    substring(x, 1, 4) <- "LATE"
    x2 <- x
    substring(x2, 1, 4) <- "LATI"
    return(c(x, x2))
  } else {
    return(x)
  }
}

# Find which pattern matched
.find_common <- function(x) {
  ei <- .find_mat(x, "EI")
  ii <- .find_mat(x, "II")
  i <- .find_mat(x, "I") & !ii & !ei
  iae <- .find_mat(x, "IAE")
  ae <- .find_mat(x, "AE") & !iae
  iifolia <- .find_mat(x, "IIFOLIA")
  iiflora <- .find_mat(x, "IIFLORA")
  ifolia <- .find_mat(x, "IFOLIA") & !iifolia
  iflora <- .find_mat(x, "IFLORA") & !iiflora
  iodes <- .find_mat(x, "IODES")
  oides <- .find_mat(x, "OIDES")
  odes <- .find_mat(x, "ODES") & !iodes
  stats::setNames(
    c(ei, ii, i, iae, ae, iifolia, iiflora, ifolia, iflora, 
      iodes, oides, odes),
    c("ei", "ii", "i", "iae", "ae", "iifolia", "iiflora",
      "ifolia", "iflora", "iodes", "oides", "odes")
  )
}

# Substitute
.sub_common <- function(x) {
  x0 <- x
  commons <- which(.find_common(x))
  n_c <- nchar(x)
  n_p <- nchar(names(commons)) 
  if (length(n_p) != 0) {
    base_str <- substr(x, 1, n_c - n_p)
    sub_str <-
      list(
        "EI" = 1:3,
        "II" = 2:3,
        "I" = 2:3,
        "IAE" = 2:5,
        "AE" = 2:5,
        "IIFOLIA" = c(6, 8),
        "IIFLORA" = c(7, 9),
        "IFOLIA" = c(6, 8),
        "IFLORA" = c(7, 9),
        "IODES" = 10:12,
        "OIDES" = 10:12,
        "ODES" = 10:12
      )
    x <- paste0(base_str, names(sub_str)[sub_str[[commons]]])
  } 
  result <- .sub_lat(x)
  return(result[result != x0])
}



# library(stringr)
# 
# pos <- 1:1000
# subs <- lapply(as.list(LCVP::lcvp_sps_class[pos, 3]), .sub_common)
# pos3 <- which(sapply(subs, function(x){length(x) > 1}))
# errors <- NULL
# for (i in 1:length(pos3)) {
#   print(i)
#   splist <- paste(lcvp_sps_class[pos, 2][pos3[i]], subs[[pos3[i]]])
#   names_temp <- lcvp_search(splist)
#   original <- word(lcvp_sps_class[pos, 1][pos3[i]], 1, 2)
#   y <- word(names_temp$Input.Taxon, 1, 2)
#   if (!all(toupper(y) == original)) {
#     errors <- c(errors, i)
#   }
# }
# 
# 
# subs2 <- mapply(function(x, y){paste(x, y)}, x = subs[pos3], 
# y = )
# 
# resus <- lapply(subs2, lcvplants::lcvp_search, show_correct = TRUE)
