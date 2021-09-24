#' The Leipzig Catalogue of Vascular Plants (LCVP) - An Improved Taxonomic Reference List 
#' for All Known Vascular Plants
#' 
#' A package for large-scale taxonomic harmonization of plant names by fuzzy 
#' matching and synonymy resolution against the Leipzig Plant Catalogue as 
#' taxonomic backbone. Submission of single names or list of species names 
#' is possible. The Leipzig Plant Catalogue is an updated taxonomic backbone based on the 
#' plant list comprising more than 1,300,000 names and 350,000 accepted names 
#' (this data is in the [`LCVP`](https://github.com/idiv-biodiversity/LCVP) 
#' package). 
#' 
#' @section lcvplants functions:
#' * \code{\link[lcvplants:lcvp_search]{lcvp_search}}: Standardize plant names
#'  according to the LCVP
#' * \code{\link[lcvplants:lcvp_summary]{lcvp_summary}}: Summarize the results
#'  from lcvp_search
#' * \code{\link[lcvplants:lcvp_group_search]{lcvp_group_search}}: Search 
#' plants by taxa or author names in the LCVP 
#' * \code{\link[lcvplants:lcvp_fuzzy_search]{lcvp_fuzzy_search}}: Fuzzy match 
#' plant names according to the LCVP
#' * \code{\link[lcvplants:lcvp_join]{lcvp_join}}: Join two data.frames using the 
#' LCVP
#' * \code{\link[lcvplants:lcvp_match]{lcvp_match}}: Match two name lists using 
#' the LCVP
#' * \code{\link[lcvplants:lcvp_solve_dups]{lcvp_solve_dups}}: Solve duplicated 
#' names by summarizing traits
#' 
#' @name lcvplants-package
#' @aliases lcvplants-package lcvplants
#' @docType package
#' @author
#' Bruno Vilela 
#' Alessandro Gentile
#' Martin Freiberg
#' Marten Winter
#' Alexander Zizka
#' 
#' Maintainer: Bruno Vilela <bvilela.bv@gmail.com>
#' @seealso https://idata.idiv.de/ddm/Data/ShowData/1806
#' @references The Leipzig Catalogue of Vascular Plants (LCVP) - An improved 
#' taxonomic reference list for all known vascular plants.
#' @keywords nomenclature, taxonomy, vascular plants, R-package
#' 

NULL