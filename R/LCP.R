#' Standardize plant names according to the Leipzig Catalogue of Plants (LCP)
#' 
#' Allow a taxonomic resolution of plant taxa names listed in the "Leipzig
#' Catalogue of Plants" (LCP). Connects to the LCP table and validates the
#' names of a vector of plant taxa, replacing synonyms by accepted names and
#' removing orthographical errors in plant names. 
#' The LCP data package must be installed. It is available from 
#' https://github.com/idiv-biodiversity/LCP.
#' 
#' 
#' @param splist A character vector specifying the input taxon, each element
#' including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name and author name
#' @param genus_search Logical. If TRUE, the function will
#' apply the fuzzy match algorithm also for the search of the genus name,
#' otherwise as default the search is applied only to the epithet, the
#' infraspecies and the name author.
#' @param max.distance is an integer value. It represents the maximum distance
#' (number of characters) allowed for a match when comparing the submitted name
#' with the closest name matches in the LCP
#' @param encoding character vector. This value will allow
#' the user to set the specific codification of the strings
#' @param family_tab Logical. If TRUE, the function will
#' return the list of plant taxa names belonging to the same family name
#' submitted by the user
#' @param order_tab Logical. If TRUE, the function will return
#' the list of plant taxa names belonging to the same order name submitted by
#' the user
#' @param genus_tab Logical. If TRUE, the function will return
#' the list of plant taxa names belonging to the same genus name submitted by
#' the user
#' @param infraspecies_tab Logical. If TRUE, the function will
#' return also all the infraspecies names found for a submitted plant name
#' @param status Logical. If FALSE, the function will return
#' not only the valid epithet for a species name but also all the possible
#' synonyms
#' @param save Logical. If TRUE, the function will write the
#' output file as comma-separated format (.csv), saving it into the working
#' directory or in the directory already set through the 'out_path' option
#' @param visualize If TRUE the function will visualize the output search on
#' the 'Source Tab' of RStudio. This option has to be turned off (FALSE) if the
#' package is execute in a UNIX environment (from the command line) without
#' having a Graphical User Interface
#' @param version A character vector indicating the current version of the
#' package. A new version is under development
#' allowing the package to connect the web API that is under construction
#' @param max.cores integer value, indicating the number of CPU cores to be
#' used for the parallelization of the plant name search when a list of plant
#' taxa names is submitted. As default, the maximum number of CPU cores
#' available on the working machine menus one is set
#' @param out_path a character vector. Defines the path
#' where the output file has to be saved.
#' @author Alessandro Gentile, Alexander Zizka
#' @seealso https://idata.idiv.de/ddm/Data/ShowData/1806
#' @references The Leipzig Catalogue of Plants (LCP) - An improved taxonomic
#' reference list for all known vascular plants
#' @keywords R-package nomenclature taxonomy vascular plants
#' @examples
#' 
#' LCP("Hibiscus vitifolius")
#' LCP("Hibiscus abelmoschus var. betulifolius Mast.")
#' LCP(c("Hibiscus abelmoschus var. betulifolius Mast.", 
#'      "Hibiscus abutiloides Willd.", 
#'       "Hibiscus aculeatus", "Hibiscus acuminatus"), max.cores = 1)
#' 
#' @importFrom utils View adist data write.table
#' @importFrom parallel detectCores makeCluster parLapply stopCluster
#' 
#' @export LCP
#' 

LCP <-
function(splist, 
         genus_search = FALSE,
         max.distance = 0, 
         encoding = "UTF-8", 
         family_tab = FALSE, 
         order_tab = FALSE, 
         genus_tab = FALSE, 
         infraspecies_tab = FALSE, 
         status = TRUE, 
         save = FALSE, 
         visualize = FALSE, 
         version = "1.1", 
         max.cores = (detectCores() -1),
         out_path = getwd()) {

  start.time <- Sys.time()
  
  if(.Platform$OS.type == "unix"){
    encoding <- "UTF-8"
  }
  if(Sys.getenv("RSTUDIO") != "1"){
    visualize <- FALSE
  }
  
  if(length(splist) == 1 && tolower(splist) == "list"){
    pathstring <- file.choose()
    splist <- read_data(pathstring, encoding)
  }
  
# Check for the LCP package and if not installed, asked to install
    if (!requireNamespace("LCP", quietly = TRUE)) {
      stop("Install the 'LCP' package or provide a custom reference. See the details section in ?LCP for help.",
           call. = FALSE
      )
    }else{
      LCPposition_table <- LCP::tab_position
      LCPspecies_table <- LCP::tab_lcp
    }
    



# query LCP list ----------------------------------------------------
  
  #ASK ALESSANDRO ABOUT THIS SECTION, IF it is necessary
  
  #data(LCPposition_table)
  #data(LCPspecies_table)
  # pkgEnv <- new.env(parent=emptyenv())
  # 
  # if(!exists("LCPposition_table", pkgEnv)) {
  #   data("LCPposition_table", package="lcplants", envir=pkgEnv)
  # }
  # if(!exists("LCPspecies_table", pkgEnv)) {
  #   data("LCPspecies_table", package="lcplants", envir=pkgEnv)
  # }
  # 
  # get_position <- function() {
  #   pkgEnv[["LCPposition_table"]]
  # }
  # get_species <- function() {
  #   pkgEnv[["LCPspecies_table"]]
  # }
  # 
  
  
# run the function
  if (length(splist) < 2) {
    message("serial path, for searching a single taxon")
    results <- do.call("rbind", lapply(splist, 
                                       genus_search = genus_search, 
                                       out_path = out_path, 
                                       LCPposition_table = LCPposition_table, 
                                       LCPspecies_table = LCPspecies_table, 
                                       max.distance = max.distance, 
                                       encoding = encoding, 
                                       status = status, 
                                       save = save, 
                                       visualize = visualize, 
                                       family_tab = family_tab, 
                                       order_tab = order_tab, 
                                       genus_tab = genus_tab, 
                                       infraspecies_tab = infraspecies_tab, 
                                       version = version, 
                                       LCPsolver))
    
  } else {
    message("parallel path, for searching a list of taxa")
    
    # Set up parallel environment
    cl <- makeCluster(max.cores)
    results <- do.call("rbind", parLapply(cl, 
                                          splist, 
                                          genus_search = genus_search, 
                                          out_path = out_path, 
                                          LCPposition_table = LCPposition_table, 
                                          LCPspecies_table = LCPspecies_table, 
                                          max.distance = max.distance, 
                                          encoding = encoding, 
                                          status = status, 
                                          save = save, 
                                          visualize = FALSE, 
                                          family_tab = family_tab, 
                                          order_tab = order_tab, 
                                          genus_tab = genus_tab, 
                                          infraspecies_tab = infraspecies_tab, 
                                          version = version, 
                                          LCPsolver))
    stopCluster(cl)
  }
  
  Output_Table_tmp <- data.frame(ID = NULL, 
                                 Submitted_Name = NULL, 
                                 Order = NULL, 
                                 Family = NULL, 
                                 Genus = NULL, 
                                 Species = NULL, 
                                 Infrasp = NULL, 
                                 Infraspecies = NULL, 
                                 Authors = NULL, 
                                 Status = NULL, 
                                 LCP_Accepted_Taxon = NULL, 
                                 PL_Comparison = NULL,
                                 PL_Alternative = NULL, 
                                 Score = NULL, 
                                 Insertion = NULL,
                                 Deletion = NULL, 
                                 Substitution = NULL)
  Output_Table <- data.frame(ID = NULL, 
                             Submitted_Name = NULL, 
                             Order = NULL,
                             Family = NULL, 
                             Genus = NULL, 
                             Species = NULL, 
                             Infrasp = NULL, 
                             Infraspecies = NULL, 
                             Authors = NULL, 
                             Status = NULL, 
                             LCP_Accepted_Taxon = NULL, 
                             PL_Comparison = NULL, 
                             PL_Alternative = NULL, 
                             Score = NULL, 
                             Insertion = NULL, 
                             Deletion = NULL, 
                             Substitution = NULL)
  
  iter <- 0
  namefirst <- " "
  col <- dim(results)[2]
  row <- dim(results)[1]
  for (i in 1:row)  {
    name <- results[i,1]
    if(name != namefirst)
      iter <- iter +1
    namefirst <- name
    Output_Table_tmp <- data.frame(ID = iter, Submitted_Name = results[i,1], 
                                   Order = results[i,2], 
                                   Family = results[i,3], 
                                   Genus = results[i,4],
                                   Species = results[i,5], 
                                   Infrasp = results[i,6], 
                                   Infraspecies = results[i,7],
                                   Authors = results[i,8],
                                   Status = results[i,9],
                                   LCP_Accepted_Taxon = results[i,10], 
                                   PL_Comparison = results[i,11], 
                                   PL_Alternative = results[i,12], 
                                   Score = results[i,13], 
                                   Insertion = results[i,14], 
                                   Deletion = results[i,15], 
                                   Substitution = results[i,16],
                                   stringsAsFactors = FALSE)
    
    Output_Table <- rbind(Output_Table, Output_Table_tmp)
  }
  if (!is.null(Output_Table$Species[1])) {
    if (save == TRUE) {
      exec_date <- paste(substring(Sys.time(),1,4),
                         substring(Sys.time(),6,7),
                         substring(Sys.time(),9,10),sep="")
      exec_time <- paste(substring(Sys.time(),12,13),
                         substring(Sys.time(),15,16),
                         substring(Sys.time(),18,19),sep="")
      out_code <- paste(exec_date, "_", exec_time, sep="")
      table_name <- paste("/LCP_results_",out_code,".csv",sep = "")
      pathstring <- paste(out_path, table_name, sep = "")
      write.table(Output_Table,pathstring,sep=",",row.names=FALSE)
    }
  } else {message("no match found, check typing or change 'max.distance'")}
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  message("---------------------------------------------------------")
  message("-    End of Leipzig Catalogue of Plants (LCP) search    -")
  message("---------------------------------------------------------")
  
  if (visualize){
    View(Output_Table)
  }
  return(Output_Table)
}
