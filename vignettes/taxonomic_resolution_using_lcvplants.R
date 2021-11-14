## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE
)

hasData <- requireNamespace("LCVP", quietly = TRUE)
if (!hasData) {
    knitr::opts_chunk$set(eval = FALSE)
    msg <- paste("Note: Examples in this vignette require that the", 
                 "`LCVP` package be installed. The system",
                 "currently running this vignette does not have that package",
                 "installed, so code examples will not be evaluated.")
    msg <- paste(strwrap(msg), collapse = "\n")
    message(msg)
}

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("devtools")

## ----installation, eval = FALSE-----------------------------------------------
#  devtools::install_github("idiv-biodiversity/lcvplants")
#  devtools::install_github("idiv-biodiversity/LCVP")

## ----package load-------------------------------------------------------------
library(lcvplants)

## ----Search one species, eval = FALSE-----------------------------------------
#  lcvp_search("Hibiscus vitifolius")

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(lcvp_search("Hibiscus vitifolius"))

## ----Search one species with misspelled name, eval = FALSE--------------------
#  lcvp_search("Hibiscus vitifoliuse",
#              max_distance = 0.1,
#              show_correct = TRUE)

## ---- echo = FALSE, warning = FALSE-------------------------------------------
knitr::kable(lcvp_search("Hibiscus vitifoliuse", 
                         max_distance = 0.1, 
                         show_correct = TRUE))

## ----fuzzy match, eval = FALSE------------------------------------------------
#  lcvp_fuzzy_search("Hibiscus vitifoliuse",
#                    max_distance = 0.1,
#                    keep_closest = TRUE)

## ---- echo = FALSE, warning = FALSE-------------------------------------------
knitr::kable(lcvp_fuzzy_search("Hibiscus vitifoliuse",
                               max_distance = 0.1, 
                               keep_closest = TRUE))

## ----multiple species search--------------------------------------------------
splist <- c(
  "Hibiscus abelmoschus var. betulifolius Mast.",
  "Hibiscus abutiloides Willd.",
  "Hibiscus aculeatus",
  "Hibiscus acuminatus",
  "Hibiscus furcatuis" 
)
x <- lcvp_search(splist, max_distance = 0.2, show_correct = TRUE)

## ----show multiple, eval = FALSE----------------------------------------------
#  x

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(x)

## -----------------------------------------------------------------------------
lcvp_summary(x)

## -----------------------------------------------------------------------------
sps_mult <- attr(x, "matched_mult")
sps_mult

## ---- eval = FALSE------------------------------------------------------------
#  lcvp_fuzzy_search(sps_mult)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(lcvp_fuzzy_search(sps_mult))

## -----------------------------------------------------------------------------
# Search by Genus
x <- lcvp_group_search(c("AA", "Adansonia"), search_by = "Genus")

## ---- eval = FALSE------------------------------------------------------------
#  head(x)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(x))

## -----------------------------------------------------------------------------
# Search by Author and keep only accepted names
x <- lcvp_group_search("Schltr.", search_by = "Author", status = "accepted")

## ---- eval = FALSE------------------------------------------------------------
#  head(x)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(x))

## -----------------------------------------------------------------------------
# Generate two lists of species name
splist1 <- sample(LCVP::tab_lcvp$Input.Taxon[5:100])
splist2 <- sample(LCVP::tab_lcvp$Input.Taxon[100:2])

## -----------------------------------------------------------------------------
# Match both lists
x <- lcvp_match(splist1, splist2)

## ---- eval = FALSE------------------------------------------------------------
#  head(x)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(x))

## -----------------------------------------------------------------------------
# Match both lists
matchLists <- lcvp_match(splist1, splist2, include_all = FALSE)
splist2_reordered <- splist2[matchLists$Match.Position.2to1]

## -----------------------------------------------------------------------------
# Create data.frame1
splist1 <- sample(LCVP::tab_lcvp$Input.Taxon[2:100])
tbl1 <-
  data.frame("Species" = splist1, "Trait1" = runif(length(splist1)))

# Create data.frame2
splist2 <- sample(LCVP::tab_lcvp$Input.Taxon[98:8])
tbl2 <- data.frame(
  "Species" = splist2,
  "Trait2" = runif(length(splist2)),
  "Trait3" = runif(length(splist2)),
  "Trait4" = sample(c("a", "b"), length(splist2), replace = TRUE),
  "Trait5" = sample(c(TRUE, FALSE), length(splist2), replace = TRUE)
)

## -----------------------------------------------------------------------------
# Full join the two tables
x <- lcvp_join(tbl1, tbl2, c("Species", "Species"), type = "full")

## ---- eval = FALSE------------------------------------------------------------
#  head(x)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(x))

## -----------------------------------------------------------------------------
x <- lcvp_join(tbl1, tbl2, c("Species", "Species"), type = "full",
               solve_duplicated = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  head(x)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(x))

## -----------------------------------------------------------------------------
# Create a data.frame with duplicated names and different traits
splist <- sample(LCVP::tab_lcvp$Input.Taxon[1:100])
search <- lcvp_search(splist)

tbl <- data.frame("Species" = search$Output.Taxon,
"Trait1" = runif(length(splist)),
"Trait2" = sample(c("a", "b"), length(splist), replace = TRUE),
"Trait3" = sample(c(TRUE, FALSE), length(splist), replace = TRUE))

## -----------------------------------------------------------------------------
# Solve with default parameters
x <- lcvp_solve_dups(tbl, 1)

## ---- eval = FALSE------------------------------------------------------------
#  head(x)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(x))

