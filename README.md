# lcvplants v2.0.999
[![R-CMD-check](https://github.com/idiv-biodiversity/lcvplants/workflows/R-CMD-check/badge.svg)](https://github.com/idiv-biodiversity/lcvplants/actions)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

A package for large-scale taxonomic harmonization of plant names by fuzzy matching and synonymy resolution against the Leipzig Plant Catalogue as taxonomic backbone. Submission of single names or list of species names is possible (for lists with more than 5000 species computation may take some time). The Leipzig Plant Catalogue is an updated taxonomic backbone based on the plant list comprising more than 1,300,000 names and 350,000 accepted names (this data is in the [`LCVP`](https://github.com/idiv-biodiversity/LCVP) package).


See `News.md` for update information.

# Installation
You can install lcvplants directly from R using the `install_github` function of devtools (you may need to install that one first). To use `lcvplants` you also need the data of the [`LCVP` package](https://github.com/idiv-biodiversity/LCVP), which you can install in the same way.

```r
devtools::install_github("idiv-biodiversity/LCVP@LCVP2")
devtools::install_github("idiv-biodiversity/lcvplants@Lcvp_2")
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


# You may also want to check all the possible matches 
lcvp_fuzzy_search("Hibiscus vitifolia", max.distance = 2)
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
splist1 <- sample(LCVP::tab_lcvp$Input.Taxon[1:10])

# Species list 2
splist2 <- sample(LCVP::tab_lcvp$Input.Taxon[1:10])

# Compare the two lists
lcvp_match(splist1, splist2)
```

Lastly, you can use lcvplants two join two tables based on vascular plant names:
```r
# data.frame1 
splist1 <- sample(LCVP::tab_lcvp$Input.Taxon[2:10])
x <- data.frame("Species" = splist1, "Trait1" = runif(length(splist1)))
 
# data.frame2
splist2 <- sample(LCVP::tab_lcvp$Input.Taxon[1:8])
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


# Documentation
You can find more information on how to use lcvplants in the [vignette](https://idiv-biodiversity.github.io/lcvplants/articles/taxonomic_resolution_using_lcplants.html)

# Citation

```{r}
library(lcvplants)
citation("lcvplants")
```

