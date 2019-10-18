\name{lcplants-package}
\alias{lcplants-package}
\alias{lcplants}
\docType{package}
\title{
\packageTitle{lcplants}
}
\description{
\packageDescription{lcplants}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{lcplants}
\packageIndices{lcplants}
An 'R-package' to allow a taxonomic resolution of plant species names listed in the "Leipzig Catalogue of Plants" (LCP) 
}
\author{
\packageAuthor{lcplants}

Maintainer: \packageMaintainer{lcplants}
}
\references{
Freiberg M. et al., (2019). Leipzig Catalogue of Plants
}
\keyword{ nomenclature, taxonomy, vascular plants, R-package }
\seealso{
https://idata.idiv.de/ddm/Data/ShowData/1806.
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
To submit one single plant taxa name or a list:
> Results <- LCP(“Hibiscus vitifolius”)
> Results <- LCP(“list”)
To submit a plant taxa name to solve some possible orthographic errors:
> Results <- LCP(“Hibiscus vitfolius”, max.distance = 1)
To submit a plant taxa name to solve possible orthographic errors also at the genus level:
> Results <- LCP(“Hibiescus vitifolius”, max.distance = 1, genus_search = TRUE)
To submit a plant taxon looking for all the synonym names:
> Results <- LCP(“Hibiscus vitifolius”, status = FALSE)
To submit a plant taxa name looking for all infraspecies names:
> Results <- LCP(“Hibiscus vitifolius”, infraspecies_tab = TRUE)
To submit a plant taxa name looking for all plant taxa names belonging to the same genus rank:
> Results <- LCP(“Hibiscus”, genus_tab = TRUE)
To submit a plant taxa name looking for all the plant taxa names belonging to the same family rank:
> Results <- LCP(“Malvaceae”, family_tab = TRUE)
To submit a plant taxa name looking for all the plant taxa names belonging to the same order rank:
> Results <- LCP(“Hibiscus Malvales”, order_tab = TRUE)
To execute the searching process booking a fixed number of CPU cores available on the working machine:
> Results <- LCP(“Hibiscus vitifolius”, max.cores = 3)
To save the result to a Comma-Separated Values (.csv) file:
> Results <- LCP(“Hibiscus vitifolius”, save = TRUE)
}
