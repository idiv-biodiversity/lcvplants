# lcplants v1.0.1
[![Build Status](https://travis-ci.org/ropensci/lcplants.svg?branch=master)]()
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

The lcpplants package provides large-scale taxonomic harmonization of plant names by fuzzy matching and synonymy resolution against the Leipzig Plant List as taxonomic backbone. Submission of single names or list of species names is possible (for lists with more tahn 5000 species computaiton may take some time). The Leipzig plant list is an updated taxonomic backbone based on the plant list comprising XXXXX names and YYY accepted names.


See [News](https://github.com/idiv-biodiversity/lcplants/NEWS.md) for update information.

# Installation

```r
devtools::install_github("idiv-biodiversity/lcplants")
library(lcplants)
```

# Usage

## Input
The input list of plant names has to be organized as a list of names each on a separated line and saved in a text (.txt) or comma-separated values (.csv) file. Following the International Code of Nomenclature for algae, fungi, and plants (Shenzhen Code: https://www.iapt-taxon.org/nomen/main.php), which sets the rules to describe plant taxa names, The user has to write the taxon specifying the different terms separated by spaces as the following: genus, epithet, infraspecies rank, infraspecies name and authorship (e.g. *Draba mollissima* var. *kusnezowii* N.Busch). The user must use no special characters at the level of genus, epithet and infraspecies  ranks; but special character can be possible only at the authorship level. The infraspecies name has to be  preceded by the infraspecies rank (e.g. "subsp.", "var.", "forma", "ssp.", "f.", "subvar.", "subf."). f the genus or the epithet names are composed by two words, they have to be separated by a hyphen ("-") character and never by space (e.g. *Hibiscus rosa-sinensis* L.; never as *Hibiscus rosa sinensis* L.).

```r
LCP('Payena lancifolia H.J.Lam', ...)

LCP('list', ...)

```

## Output

# Documentation


# Citation


