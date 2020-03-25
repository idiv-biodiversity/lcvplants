## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  library(devtools)
#  devtools::install_github("idiv-biodiversity/lcplants")
#  library(lcplants)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
library(lcplants)

## ---- message = TRUE----------------------------------------------------------
LCP("Hibiscus vitifolius")

## ---- eval = FALSE------------------------------------------------------------
#  resol <- LCP("Hibiscus vitifolius")
#  resol

## ---- eval = FALSE------------------------------------------------------------
#  LCP("Hibiscus abelmoschus var. betulifolius Mast.")

## ---- eval = FALSE------------------------------------------------------------
#  LCP(c("Hibiscus abelmoschus var. betulifolius Mast.",
#        "Hibiscus abutiloides Willd.",
#        "Hibiscus aculeatus",
#        "Hibiscus acuminatus"))

## ---- eval = FALSE------------------------------------------------------------
#  LCP("list")

## -----------------------------------------------------------------------------
# no fuzzy matching does not find misspelled names
fuzz <- LCP("Hibiscus vitifolios")
fuzz$Score

# fuzzy matching does find it
fuzz <- LCP("Hibiscus vitifolios", max.distance = 1)
fuzz$Score

#Also works for larger distances
fuzz <- LCP("Hibiscus vitifulios", max.distance = 2)
fuzz$Score

# But results become less reliable with larger distances
fuzz <- LCP("Hibiscus acetosulla", max.distance = 5)
fuzz

## -----------------------------------------------------------------------------
# no fuzzy matching does not find misspelled names
fuzz <- LCP("Hubiscus vitifolius")
fuzz$Score

# fuzzy matching does find it
fuzz <- LCP("Hubiscus vitifolius", max.distance = 1, genus_search = TRUE)
fuzz$Score

## ---- message = TRUE----------------------------------------------------------
LCP("Hibiscus vitifolius", status = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  LCP("Hibiscus", genus_tab = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  LCP("Hibiscus vitifolius", infraspecies_tab = TRUE)

