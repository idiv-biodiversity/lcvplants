\name{LCPspecies_table}
\alias{LCPspecies_table}
\docType{data}
\title{
List of Plant Species Name accordingly with the Leipzig Catalogue of Plants (LCP)
}
\description{
contain 1314778 species names.
}
\usage{data("LCPspecies_table")}
\format{
  A data frame with 1314778 observations on the following 7 variables.
  \describe{
    \item{\code{Input.Taxon}}{a character vector}
    \item{\code{Status}}{a character vector}
    \item{\code{PL.comparison}}{a character vector}
    \item{\code{PL.alternative}}{a character vector}
    \item{\code{Output.Taxon}}{a character vector}
    \item{\code{Family}}{a character vector}
    \item{\code{Order}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(LCPspecies_table)
str(LCPspecies_table)
}
\keyword{datasets}
