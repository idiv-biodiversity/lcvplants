## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  library(devtools)
#  devtools::install_github("idiv-biodiversity/lcvplants")
#  devtools::install_github("idiv-biodiversity/LCVP")
#  library(lcvplants)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
library(lcvplants)

## ---- message = TRUE----------------------------------------------------------
LCVP("Hibiscus vitifolius")

## ---- eval = FALSE------------------------------------------------------------
#  resol <- LCVP("Hibiscus vitifolius")
#  resol

## ---- eval = FALSE------------------------------------------------------------
#  LCVP("Hibiscus abelmoschus var. betulifolius Mast.")

## ---- eval = FALSE------------------------------------------------------------
#  LCVP(c("Hibiscus abelmoschus var. betulifolius Mast.",
#        "Hibiscus abutiloides Willd.",
#        "Hibiscus aculeatus",
#        "Hibiscus acuminatus"))

## ---- eval = FALSE------------------------------------------------------------
#  LCVP("list")

## -----------------------------------------------------------------------------
# no fuzzy matching does not find misspelled names
fuzz <- LCVP("Hibiscus vitifolios")
fuzz$Score

# fuzzy matching does find it
fuzz <- LCVP("Hibiscus vitifolios", max.distance = 1)
fuzz$Score

#Also works for larger distances
fuzz <- LCVP("Hibiscus vitifulios", max.distance = 2)
fuzz$Score

# But results become less reliable with larger distances
fuzz <- LCVP("Hibiscus acetosulla", max.distance = 5)
fuzz

## -----------------------------------------------------------------------------
# no fuzzy matching does not find misspelled names
fuzz <- LCVP("Hubiscus vitifolius")
fuzz$Score

# fuzzy matching does find it
fuzz <- LCVP("Hubiscus vitifolius", max.distance = 1, genus_search = TRUE)
fuzz$Score

## ---- message = TRUE----------------------------------------------------------
LCVP("Hibiscus vitifolius", status = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  LCVP("Hibiscus", genus_tab = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  LCVP("Hibiscus vitifolius", infraspecies_tab = TRUE)

