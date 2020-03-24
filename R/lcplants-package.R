

#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_title(\"#1\")}",
#' "lcplants")\Sexpr{tools:::Rd_package_title("lcplants")}
#' 
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_description(\"#1\")}",
#' "lcplants")\Sexpr{tools:::Rd_package_description("lcplants")}
#' 
#' The R-package is able to apply a text-based search of plant names on the LCP
#' using a fuzzy match algorithm, which is the core of the searching engine.
#' The fuzzy match algorithm is applied at genus, epithet, infraspecies and
#' authority level of a plant species and it uses the 'max.distance' argument
#' from the agrep() R-function to assess the comparison between the searched
#' plant name and the closest, in terms of number of similar characters plant
#' name from the LCP. Through the "lcplants" R-package, the user can search
#' either for a single plant name or for a list of names.  There are no
#' limitation on the list length, but we strongly recommend the list to less
#' than 5000 names to ensure a reasonable speed of the execution. The input
#' list of plant names has to be organized as a list of names each on a
#' separated line and saved in a text (.txt) or comma-separated values (.csv)
#' file.  Following the International Code of Nomenclature for algae, fungi,
#' and plants (Shenzhen Code: https://www.iapt-taxon.org/nomen/main.php), which
#' sets the rules to describe plant taxa names, the user has to write the taxon
#' specifying the different terms separated by spaces as the following: genus,
#' epithet, infraspecies rank, infraspecies name and authorship (e.g. Draba
#' mollissima var. kusnezowii N.Busch).  The user must use no special
#' characters at the level of genus, epithet and infraspecies ranks; but
#' special character can be possible only at the authorship level. The
#' infraspecies name has to be preceded by the infraspecies rank (e.g.
#' "subsp.", "var.", "forma", "ssp.", "f.", "subvar.", "subf.").  At least the
#' taxon name has to be written with the two-term naming system, which defines
#' a species (binomial nomenclature system); the infraspecies and authorship
#' are optional for a finer search.  The only possible cases, where the
#' function accepts one single name, are when the user wants to perform the
#' search only for the genus, order or family ranks; but in these cases the
#' options 'genus_tab', 'family_tab' or 'order_tab' have to be activated (e.g.
#' genus_tab = TRUE; FALSE is default). Lastly, if the genus or the epithet
#' names are composed by two words, they have to be separated by a hyphen ("-")
#' character and never by space (e.g. Hibiscus rosa-sinensis L.; never as
#' Hibiscus rosa sinensis L.).  The proper way to submit an hybrid plant taxon
#' is using the characters '_x' in the end of the genus and epithet name (e.g.
#' Spartocytisus_x filipes Webb & Berthel., Lycopodium habereri_x House).
#' However the lcplants R-package is able to accept other forms of submitted
#' plant names such as 'x' or 'x_' before the names both upper or lower case
#' (e.g. x Spartocytisus filipes Webb & Berthel., x_Spartocytisus filipes Webb
#' & Berthel., Lycopodium x habereri House) and change them automatically into
#' the required format. The commonly used special Unicode Character "U+00D7"
#' for indicating hybrids is not accepted. If the user wants to submit a list
#' of plant names to be searched, the user has to write the following sentence
#' on the R/RStudio console: LCP('list', ...); the term 'list' will inform the
#' function that has been submitted a list of plant names stored in a file.
#' Then an external window will appear to search for the path where the file is
#' stored. If the user wants to submit a single taxon the user has to submit
#' the sentence as the following example: "LCP('Payena lancifolia H.J.Lam',
#' ...)", followed by one or more options.
#' 
#' @name lcplants-package
#' @aliases lcplants-package lcplants
#' @docType package
#' @author
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_author(\"#1\")}",
#' "lcplants")\Sexpr{tools:::Rd_package_author("lcplants")}
#' 
#' Maintainer:
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_maintainer(\"#1\")}",
#' "lcplants")\Sexpr{tools:::Rd_package_maintainer("lcplants")}
#' @seealso https://idata.idiv.de/ddm/Data/ShowData/1806
#' @references The Leipzig Catalogue of Plants (LCP) - An improved taxonomic
#' reference list for all known vascular plants.
#' @keywords nomenclature, taxonomy, vascular plants, R-package
#' @examples
#' 
#' # To submit one single plant taxa name or a list:
#' LCP("Hibiscus vitifolius")
#' # To submit a plant taxa name to solve some possible orthographic errors:
#' LCP("Hibiscus vitfolius", max.distance = 1)
#' # To submit a plant taxa name to solve possible orthographic errors also at the genus level:
#' LCP("Hibiescus vitifolius", max.distance = 1, genus_search = TRUE)
#' # To submit a plant taxon looking for all the synonym names:
#' LCP("Hibiscus vitifolius", status = FALSE)
#' # To submit a plant taxa name looking for all infraspecies names:
#' LCP("Hibiscus vitifolius", infraspecies_tab = TRUE)
#' # To submit a plant taxa name looking for all plant taxa names belonging to the same genus rank:
#' LCP("Hibiscus", genus_tab = TRUE)
#' # To execute the searching process booking a fixed number of
#' # CPU cores available on the working machine:
#' LCP("Hibiscus vitifolius", max.cores = 3)
#' # To save the result to a Comma-Separated Values (.csv) file:
#' LCP("Hibiscus vitifolius", save = TRUE)
#' 
NULL





#' List of the number positions of the first 3 letters of the species name in
#' the LCPspecies_table
#' 
#' The 'LCPposition_table' contains 2.676 records each of them reports the
#' position (in term of number of rows) of the first three letters (triphthong)
#' for the 1.314.778 plant names stored in the variable 'Input.Taxon' (which
#' are listed in alphabetical order) of the table 'LCPspecies_table'. This
#' indexing system allows the speeding up of the search on the largest list
#' 'LCPspecies_table.rda'. When the function 'LCPsolver()' begins the search
#' for a plant name (e.g. "Laserpitium eliasii"), it starts to compare the
#' first three letters of the submitted genus name (e.g. 'Las') with the
#' records in the list of Triphthong on the 'LCPposition_table.rda' table. When
#' it is able to find a match, the LCPsolver() function reads and stores in
#' memory the corresponding value (e.g. 688815) in the 'Position' column. This
#' value will be the starting point from where to start the search on the
#' 'LCPspecies_table' table; and the following position value (e.g. 691389) in
#' the 'Position' column will be used as the end point in the
#' 'LCPspecies_table' table where to stop the search process.
#' 
#' 
#' @name LCPposition_table
#' @docType data
#' @format A data frame with 2676 observations on the following 3 variables.
#' \describe{ 
#'  \item{Position}{A character vector. It is the position of
#' the first 3 letters of the species name in the LCPspecies_table.}
#' \item{Triphthong}{A character vector. First 3 letters of the species
#' name in the LCPspecies_table.} \item{Genus}{A character vector.
#' Corresponding Genus name.} }
#' @references The Leipzig Catalogue of Plants (LCP) - An improved taxonomic
#' reference list for all known vascular plants
#' @source https://idata.idiv.de/ddm/Data/ShowData/1806
#' @keywords datasets
#' @examples
#' 
#' data(LCPposition_table)
#' str(LCPposition_table)
#' 
NULL





#' List of Plant Species Name accordingly with the Leipzig Catalogue of Plants
#' (LCP)
#' 
#' The 'LCPspecies_table' contains 1.314.778 records belonging to all the
#' vascular plant taxa names checked and listed from the "Leipzig Catalogue of
#' Plants" (LCP).
#' 
#' 
#' @name LCPspecies_table
#' @docType data
#' @format A data frame with 1314778 observations on the following 7 variables.
#' \describe{ \item{Input.Taxon}{A character vector. This variable
#' contains the list of all 1.314.778 vascular plant taxa names listed by the
#' authors of the Leipzig Catalogue of Plants.} \item{Status}{A
#' character vector. This variable provides a description if a taxon is
#' classified as 'valid', 'synonym', 'unresolved' or 'external'. The
#' 'unresolved' rank means that the status of the plant name could be either
#' valid or synonym, but the information available does not allow a definitive
#' decision (see the main text Freiberg et al. for more details).}
#' \item{PL.comparison}{A character vector. This field provides a
#' direct comparison with 'The Plant List' (TPL; The Plant List
#' http://www.theplantlist.org/ accessed: 1.1. 2013) reporting further
#' information such as 'identical', 'synonym', 'other synonym', 'different
#' authors', 'missing', 'misspelling', 'unresolved'.}
#' \item{PL.alternative}{A character vector. This field provides a
#' possible alternative name suggested by TPL.} \item{Output.Taxon}{A
#' character vector. This variable contains the list of the accepted plant taxa
#' names (350.766) according to the LCP.} \item{Family}{A character
#' vector. It provides the corresponding family name of the 'Input.Taxon',
#' staying empty if the Status is unresolved.} \item{Order}{A character
#' vector. The Order column provides the corresponding order name of the
#' 'Input.Taxon', staying empty if the Status is unresolved.} }
#' @references The Leipzig Catalogue of Plants (LCP) - An improved taxonomic
#' reference list for all known vascular plants.
#' @source https://idata.idiv.de/ddm/Data/ShowData/1806
#' @keywords datasets
#' @examples
#' 
#' data(LCPspecies_table)
#' str(LCPspecies_table)
#' 
NULL



