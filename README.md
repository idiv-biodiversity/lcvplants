# lcvplants v1.1.0
[![Build Status](https://travis-ci.com/idiv-biodiversity/lcplants.svg?token=Bbiute2RTxuP5ghkjAxb&branch=master)](https://travis-ci.com/idiv-biodiversity/lcplants.svg?token=Bbiute2RTxuP5ghkjAxb&branch=master)
[![codecov.io](https://codecov.io/github/idiv-biodiversity/lcplants/graphs/badge.svg?branch=master)](https://codecov.io/github/idiv-biodiversity/lcplants)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

A package for large-scale taxonomic harmonization of plant names by fuzzy matching and synonymy resolution against the Leipzig Plant Catalogue as taxonomic backbone. Submission of single names or list of species names is possible (for lists with more than 5000 species computation may take some time). The Leipzig Plant Catalogue is an updated taxonomic backbone based on the plant list comprising more than 1,300,000 names and 350,000 accepted names (this data is in the [`LCVP`](https://github.com/idiv-biodiversity/LCVP) package).


See `News.md` for update information.

# Installation
You can install lcvplants directly from R using the `install_github` function of devtools (you may need to install that one first). To use `lcvplants` you also need the data of the [`LCVP` package](https://github.com/idiv-biodiversity/LCVP), which you can install in the same way.

```r
devtools::install_github("idiv-biodiversity/LCVP")
devtools::install_github("idiv-biodiversity/lcvplants")
```

# Usage
```r
library(lcvplants)

# single names
LCVP("Hibiscus vitifolius")

# Can also include infra specific names and authorities
LCVP("Hibiscus abelmoschus var. betulifolius Mast.")

# Also works on vectors of names
LCVP(c("Hibiscus abelmoschus var. betulifolius Mast.", "Hibiscus abutiloides Willd.", 
       "Hibiscus aculeatus", "Hibiscus acuminatus"), max.cores = 1))

# Pick a file from the hard drive
LCVP("list")
```

# Documentation
You can find more information on how to use lcvplants in the [vignette](https://idiv-biodiversity.github.io/lcvplants/articles/taxonomic_resolution_using_lcplants.html)

# Citation

```{r}
library(lcvplants)
citation("lcvplants")
```
