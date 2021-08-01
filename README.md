# lcvplants v1.1.1
[![Build Status](https://api.travis-ci.com/idiv-biodiversity/lcplants.svg?token=Bbiute2RTxuP5ghkjAxb&branch=Lcvp_2)
[![codecov.io](https://codecov.io/github/idiv-biodiversity/lcplants/graphs/badge.svg?branch=Lcvp_2)](https://codecov.io/github/idiv-biodiversity/lcplants)
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

Simple working examples:
```r
library(lcvplants)

# single names
lcvp_search("Hibiscus vitifolius")

# Can also include infra specific names and authorities
lcvp_search("Hibiscus abelmoschus var. betulifolius Mast.")

# Also works on vectors of names
match_result <- lcvp_search(c("Hibiscus abelmoschus var. betulifolius Mast.", "Hibiscus abutiloides Willd.", 
       "Hibiscus aculeatus", "Hibiscus acuminatus"))

# You can see the summary results
lcvp_summary(match_result)
```

The algorithm can also consider misspellings and look for the most similar match given a max.distance argument set by the user:

```r
# Misspeled name
lcvp_search("Hibiscus vitifolia", max.distance = 2)

# Misspelings in a vector of names
res_ex <- lcvp_search(c("Hibiscus abelmoschus var. betulifolius Mast.", "Hibiscus abutiloides Willd.", "Hibiscus aculeatus", "Hibiscus acuminatus", "Hibiscus furcatuis", "Hibiscus error"), max.distance = 1)
lcvp_summary(res_ex)
```

You can also search for species using the Order, Family, Genus or Author name:
```r
# Search for the genus AA
lcvp_group_search("AA", search_by = "Genus")

# It also accepts a vector of names
lcvp_group_search(c("Orchidaceae", "Poaceae", "Phyllanthaceae"), search_by = "Family")
```



# Documentation
You can find more information on how to use lcvplants in the [vignette](https://idiv-biodiversity.github.io/lcvplants/articles/taxonomic_resolution_using_lcplants.html)

# Citation

```{r}
library(lcvplants)
citation("lcvplants")
```
