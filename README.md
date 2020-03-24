# lcplants v1.1.0
[![Build Status](https://travis-ci.com/idiv-biodiversity/lcplants.svg?branch=master)]()
[![codecov.io](https://codecov.io/github/idiv-biodiversity/lcplants/graphs/badge.svg?branch=master)](https://codecov.io/github/idiv-biodiversity/lcplants)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

A package for large-scale taxonomic harmonization of plant names by fuzzy matching and synonymy resolution against the Leipzig Plant Catalogue as taxonomic backbone. Submission of single names or list of species names is possible (for lists with more than 5000 species computation may take some time). The Leipzig Plant Catalogue is an updated taxonomic backbone based on the plant list comprising more than 1,300,000 names and 350,000 accepted names.


See [News](https://github.com/idiv-biodiversity/lcplants/NEWS.md) for update information.

# Installation
```r
devtools::install_github("idiv-biodiversity/lcplants")
```

# Usage
```r
library(lcplants)

# single names
LCP("Hibiscus vitifolius")

# Can also include infra specific names and authorities
LCP("Hibiscus abelmoschus var. betulifolius Mast.")

# Also works on vectors of names
LCP(c("Hibiscus abelmoschus var. betulifolius Mast.", "Hibiscus abutiloides Willd.", 
       "Hibiscus aculeatus", "Hibiscus acuminatus"), max.cores = 1))

# Pick a file from the hard drive
LCP("list")
```

# Documentation
You can find more information on how to use lcplants in the [vignette]()

# Citation
Freiberg M, Winter M, Gentile A, Zizka A, Muellner-Riehl AN, Weigelt A & Wirth C. The Leipzig Catalogue of Plants (LCP) –- An improved taxonomic reference list for all known vascular plants. R package, available at https://github.com/idiv-biodiversity/lcplants.

