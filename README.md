# lcvplants 2.1.0
[![R](https://github.com/idiv-biodiversity/lcvplants/actions/workflows/r.yml/badge.svg)](https://github.com/idiv-biodiversity/lcvplants/actions/workflows/r.yml)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

A package for large-scale taxonomic harmonization of plant names by fuzzy matching and synonymy resolution against the Leipzig Plant Catalogue as taxonomic backbone. Submission of single names or list of species names is possible. The Leipzig Plant Catalogue is an updated taxonomic backbone based on the plant list comprising more than 1,300,000 names and 350,000 accepted names (this data is in the [`LCVP`](https://github.com/idiv-biodiversity/LCVP) package). 

# New version (v2.1.0)
* New output table for main functions

# version (v2.0.0)

* Improved speed in processing species search
* Separated function to search by group or authors name
* Separated function to show all results from a fuzzy match
* New function to match two lists of species name
* New function to join two tables based on species name
* New function to combine duplicated rows

See `News.md` for update information.

# Video Tutorial 

[![Video Tutorial](https://img.youtube.com/vi/fAjbRNrLCec/0.jpg)](https://www.youtube.com/watch?v=fAjbRNrLCec)


# Installation
You can install lcvplants directly from R using the `install_github` function of devtools (you may need to install that one first). To use `lcvplants` you also need the data of the [`LCVP` package](https://github.com/idiv-biodiversity/LCVP), which you can install in the same way.

```r
install.packages("devtools")

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
search_result <- lcvp_search(c("Hibiscus abelmoschus var. betulifolius Mast.", "Hibiscus abutiloides Willd.", 
       "Hibiscus aculeatus", "Hibiscus acuminatus"))

# You can see the summary results
lcvp_summary(search_result)
```

The algorithm can also consider misspellings and look for the most similar match given a max_distance argument set by the user:
```r
# Misspeled name
lcvp_search("Hibiscus vitifolia", max_distance = 2)

# Misspelings in a vector of names
res_ex <- lcvp_search(c("Hibiscus abelmoschus var. betulifolius Mast.", "Hibiscus abutiloides Willd.", "Hibiscus aculeatus", "Hibiscus acuminatus", "Hibiscus furcatuis", "Hibiscus error"), max_distance = 1)
lcvp_summary(res_ex)


# You may also want to check all the possible matches 
lcvp_fuzzy_search("Hibiscus vitifolia", max_distance = 2)
```

You can also search for species using the Order, Family, Genus or Author name:
```r
# Search for the genus AA
lcvp_group_search("AA", search_by = "Genus")

# It also accepts a vector of names
lcvp_group_search(c("Orchidaceae", "Poaceae", "Phyllanthaceae"), search_by = "Family")
```

The package also facilitates the comparison between two lists of species vascular plant names:
```r
# Species list 1
splist1 <- sample(apply(LCVP::tab_lcvp[2:10, 2:3], 1, paste, collapse = " "))

# Species list 2
splist2 <- sample(apply(LCVP::tab_lcvp[11:3, 2:3], 1, paste, collapse = " "))

# Compare the two lists
lcvp_match(splist1, splist2)
```

Lastly, you can use lcvplants two join two tables based on vascular plant names:
```r
# data.frame1 
splist1 <- sample(apply(LCVP::tab_lcvp[2:10, 2:3], 1, paste, collapse = " "))
x <- data.frame("Species" = splist1, "Trait1" = runif(length(splist1)))
 
# data.frame2
splist2 <- sample(apply(LCVP::tab_lcvp[11:3, 2:3], 1, paste, collapse = " "))
y <- data.frame("Species" = splist2,  "Trait2" = runif(length(splist2)), "Trait3" = runif(length(splist2)))
 
# Full join the two tables
lcvp_join(x, y, c("Species", "Species"), type = "full")
```

# Function summary

| Name | Description |
|---|---|
|`lcvp_search`|Resolve one or multiple vascular plant names. Return only best matches.|
|`lcvp_summary`| Summarize results of `lcvp_search` query | 
|`lcvp_fuzzy_match`| Same as `lcvp_seach` but returns all matches|
|`lcvp_group_search`| Return all names listed in LCVP for a genus, family, order, or authority|
|`lcvp_match`| Compare and match two lists of species vascular plant names|
|`lcvp_join`| Join two tables based on vascular plant names|
|`lcvp_solve_dups`| Solve duplicated species names by summarizing traits given user provided functions|


# Documentation
You can find more information on how to use lcvplants in the [vignette](https://idiv-biodiversity.github.io/lcvplants/articles/taxonomic_resolution_using_lcvplants.html).

# Citation

```{r}
library(lcvplants)
citation("lcvplants")
```

