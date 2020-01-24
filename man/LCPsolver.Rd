\name{LCPsolver}
\alias{LCPsolver}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search and solve one single plant name according to the Leipzig Catalogue of Plants (LCP).
}
\description{
Allow a taxonomic resolution of plant taxa names listed in the "Leipzig Catalogue of Plants" (LCP). 
It applies a string comparison between the submitted names from the user with the list of taxa listed 
in the 'input.taxon' column of the 'LCPspecies_table.rda' table, applying a fuzzy match algorithm 
for possible orthographic errors solving. This function takes care of one single taxon per time 
for each execution and it applies the search for the submitted plant name trying to match the 
corresponding taxon in the 'LCPspecies_table' table. 
}
\usage{
LCPsolver(sp, genus_search, out_path, LCPposition_table, LCPspecies_table, encoding, 
max.distance, family_tab, order_tab, genus_tab, infraspecies_tab, status, save, 
visualize, version)
}
\arguments{
  \item{sp}{
A character vector specifying the input taxon, each element including genus and specific epithet and, potentially, infraspecific rank, infraspecific name and author name
}
  \item{LCPposition_table}{
A character vector specifying the input taxon, each element including genus and specific epithet and, potentially, infraspecific rank, infraspecific name and author name
}
  \item{LCPspecies_table}{
A character vector specifying the input taxon, each element including genus and specific epithet and, potentially, infraspecific rank, infraspecific name and author name
}
  \item{genus_search}{
Logical, FALSE (default). If TRUE, the function will apply the fuzzy match algorithm also for the search of the genus name, otherwise as default the search is applied only to the epithet, the infraspecies and the name author.
}
  \item{max.distance}{
is an integer value. It represents the maximum distance (number of characters) allowed for a match when comparing the submitted name with the closest name matches in the LCP
}
  \item{encoding}{
character vector, "UTF-8" (default). This value will allow the user to set the specific codification of the strings
}
  \item{family_tab}{
Logical, FALSE (default). If TRUE, the function will return the list of plant taxa names belonging to the same family name submitted by the user
}
  \item{order_tab}{
Logical, FALSE (default). If TRUE, the function will return the list of plant taxa names belonging to the same order name submitted by the user
}
  \item{genus_tab}{
Logical, FALSE (default). If TRUE, the function will return the list of plant taxa names belonging to the same genus name submitted by the user
}
  \item{infraspecies_tab}{
Logical, FALSE (default). If TRUE, the function will return also all the infraspecies names found for a submitted plant name
}
  \item{status}{
Logical, TRUE (default). If FALSE, the function will return not only the valid epithet for a species name but also all the possible synonyms
}
  \item{save}{
Logical. FALSE (default). If TRUE, the function will write the output file as comma-separated format (.csv), saving it into the working directory or in the directory already set through the 'out_path' option
}
  \item{visualize}{
If TRUE the function will visualize the output search on the 'Source Tab' of RStudio. This option has to be turned off (FALSE) if the package is execute in a UNIX environment (from the command line) without having a Graphical User Interface
}
  \item{version}{
A character vector indicating the current version of the package (current version is 1.0). A new version is under development allowing the package to connect the web API that is under construction
}
  \item{out_path}{
A character vector, which allow the user to define the path where the output file has to be saved. The working directory is set as default
}
}
\references{
The Leipzig Catalogue of Plants (LCP) - An improved taxonomic reference list for all known vascular plants
}
\author{
Alessandro Gentile, Martin Freiberg, Marten Winter
}
\seealso{ https://idata.idiv.de/ddm/Data/ShowData/1806 }
