\name{LCP}
\alias{LCP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardize plant names according to the Leipzig Catalogue of Plants (LCP)
}
\description{
Allow a taxonomic resolution of plant taxa names listed in the “Leipzig Catalogue of Plants” (LCP) 
}
\usage{
LCP(splist, genus_search = FALSE, max.distance = 0, encoding = "UTF-8", family_tab = FALSE, order_tab = FALSE, genus_tab = FALSE, infraspecies_tab = FALSE, status = TRUE, save = FALSE, visualize = TRUE, version = "1.1", max.cores = (detectCores() - 1), out_path = getwd(), verbose = 0)
}
\arguments{
  \item{splist}{
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
Logical. FALSE (default). If TRUE, the function will write the output file as comma-separated format (.csv), saving it into the working directory or in the directory already set through the ‘out_path’ option
}
  \item{visualize}{
If TRUE the function will visualize the output search on the 'Source Tab' of RStudio. This option has to be turned off (FALSE) if the package is execute in a UNIX environment (from the command line) without having a Graphical User Interface
}
  \item{version}{
A character vector indicating the current version of the package (current version is 1.0). A new version is under development allowing the package to connect the web API that is under construction
}
  \item{max.cores}{
integer value, indicating the number of CPU cores to be used for the parallelization of the plant name search when a list of plant taxa names is submitted. As default, the maximum number of CPU cores available on the working machine menus one is set
}
  \item{out_path}{
a character vector, which allow the user to define the path where the output file has to be saved. The working directory is set as default
}
  \item{verbose}{
Logical. If FALSE (default). If TRUE the function will write on the 'console of RStudio' information on the execution of the function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
LCP(“Hibiscus vitifolius”)
LCP(“list”)
LCP(“Hibiscus vitfolius”, max.distance = 1)
LCP(“Hibiescus vitifolius”, max.distance = 1, genus_search = TRUE)
LCP(“Hibiscus vitifolius”, status = FALSE)
LCP(“Hibiscus vitifolius”, infraspecies_tab = TRUE)
LCP(“Hibiscus”, genus_tab = TRUE)
LCP(“Malvaceae”, family_tab = TRUE)
LCP(“Hibiscus Malvales”, order_tab = TRUE)
LCP(“Hibiscus vitifolius”, max.cores = 3)
LCP(“Hibiscus vitifolius”, save = TRUE)

## The function is currently defined as
function (splist, genus_search = FALSE, max.distance = 0, encoding = "UTF-8", 
    family_tab = FALSE, order_tab = FALSE, genus_tab = FALSE, 
    infraspecies_tab = FALSE, status = TRUE, save = FALSE, visualize = TRUE, 
    version = "1.1", max.cores = (detectCores() - 1), out_path = getwd(), 
    verbose = 0) 
{
    start.time <- Sys.time()
    if (.Platform$OS.type == "unix") {
        encoding = "UTF-8"
    }
    if (Sys.getenv("RSTUDIO") != "1") {
        visualize = FALSE
    }
    if (length(splist) == 1 && tolower(splist) == "list") {
        pathstring <- file.choose()
        splist <- read_data(pathstring, encoding)
    }
    out_path <- getwd()
    data(LCPposition_table)
    data(LCPspecies_table)
    if (length(splist) < 2) {
        if (verbose == 1) {
            print("serial path")
        }
        results <- do.call("rbind", lapply(splist, genus_search = genus_search, 
            wd_path = wd_path, out_path = out_path, LCPposition_table = LCPposition_table, 
            LCPspecies_table = LCPspecies_table, max.distance = max.distance, 
            encoding = encoding, status = status, save = save, 
            visualize = visualize, family_tab = family_tab, order_tab = order_tab, 
            genus_tab = genus_tab, infraspecies_tab = infraspecies_tab, 
            version = version, verbose = verbose, LCPsolver))
    }
    else {
        if (verbose == 1) {
            print("parallel path")
        }
        library(parallel)
        cl <- makeCluster(max.cores)
        results <- do.call("rbind", parLapply(cl, splist, genus_search = genus_search, 
            wd_path = wd_path, out_path = out_path, LCPposition_table = LCPposition_table, 
            LCPspecies_table = LCPspecies_table, max.distance = max.distance, 
            encoding = encoding, status = status, save = save, 
            visualize = FALSE, family_tab = family_tab, order_tab = order_tab, 
            genus_tab = genus_tab, infraspecies_tab = infraspecies_tab, 
            version = version, verbose = verbose, LCPsolver))
        stopCluster(cl)
    }
    Output_Table_tmp <- data.frame(ID = NULL, Submitted_Name = NULL, 
        Order = NULL, Family = NULL, Genus = NULL, Species = NULL, 
        Infrasp = NULL, Infraspecies = NULL, Authors = NULL, 
        Status = NULL, LCP_Accepted_Taxon = NULL, PL_Comparison = NULL, 
        PL_Alternative = NULL, Score = NULL, Insertion = NULL, 
        Deletion = NULL, Substitution = NULL)
    Output_Table <- data.frame(ID = NULL, Submitted_Name = NULL, 
        Order = NULL, Family = NULL, Genus = NULL, Species = NULL, 
        Infrasp = NULL, Infraspecies = NULL, Authors = NULL, 
        Status = NULL, LCP_Accepted_Taxon = NULL, PL_Comparison = NULL, 
        PL_Alternative = NULL, Score = NULL, Insertion = NULL, 
        Deletion = NULL, Substitution = NULL)
    iter = 0
    namefirst = " "
    col <- dim(results)[2]
    row <- dim(results)[1]
    for (i in 1:row) {
        name <- results[i, 1]
        if (name != namefirst) 
            iter = iter + 1
        namefirst = name
        Output_Table_tmp <- data.frame(ID = iter, Submitted_Name = results[i, 
            1], Order = results[i, 2], Family = results[i, 3], 
            Genus = results[i, 4], Species = results[i, 5], Infrasp = results[i, 
                6], Infraspecies = results[i, 7], Authors = results[i, 
                8], Status = results[i, 9], LCP_Accepted_Taxon = results[i, 
                10], PL_Comparison = results[i, 11], PL_Alternative = results[i, 
                12], Score = results[i, 13], Insertion = results[i, 
                14], Deletion = results[i, 15], Substitution = results[i, 
                16])
        Output_Table <- rbind(Output_Table, Output_Table_tmp)
    }
    if (!is.null(Output_Table$Species[1])) {
        if (save == TRUE) {
            exec_date <- paste(substring(Sys.time(), 1, 4), substring(Sys.time(), 
                6, 7), substring(Sys.time(), 9, 10), sep = "")
            exec_time <- paste(substring(Sys.time(), 12, 13), 
                substring(Sys.time(), 15, 16), substring(Sys.time(), 
                  18, 19), sep = "")
            out_code <- paste(exec_date, "_", exec_time, sep = "")
            table_name <- paste("/LCP_results_", out_code, ".csv", 
                sep = "")
            pathstring <- paste(wd_path, out_path, table_name, 
                sep = "")
            write.table(Output_Table, pathstring, sep = ",", 
                row.names = FALSE)
        }
    }
    else {
        print(paste("the Leipzig Catalogue of Plants (LCP) was not able to identify any species name, try to type it differently or use the max.distance option"))
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    print("---------------------------------------------------------")
    print("-    End of Leipzig Catalogue of Plants (LCP) search    -")
    print("---------------------------------------------------------")
    if (visualize == TRUE) {
        View(Output_Table)
    }
    return(Output_Table)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
