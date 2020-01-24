## Author: Alessandro Gentile
## Last Version: 2020/01/20

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
         visualize = TRUE, 
         version = "1.1", 
         max.cores = (detectCores() -1),
         out_path = getwd()) {

  start.time <- Sys.time()
  
  if(.Platform$OS.type == "unix"){
    encoding = "UTF-8"
  }
  if(Sys.getenv("RSTUDIO") != "1"){
    visualize = FALSE
  }
  
  if(length(splist) == 1 && tolower(splist) == "list"){
    pathstring <- file.choose()
    splist <- read_data(pathstring, encoding)
    }
# query LCP list ----------------------------------------------------
  
  #data(LCPposition_table)
  #data(LCPspecies_table)
  pkgEnv <- new.env(parent=emptyenv())
  
  if(!exists("LCPposition_table", pkgEnv)) {
    data("LCPposition_table", package="lcplants", envir=pkgEnv)
  }
  if(!exists("LCPspecies_table", pkgEnv)) {
    data("LCPspecies_table", package="lcplants", envir=pkgEnv)
  }
  
  get_position <- function() {
    pkgEnv[["LCPposition_table"]]
  }
  get_species <- function() {
    pkgEnv[["LCPspecies_table"]]
  }
  
# run the function
  if (length(splist) < 2) {
    message("serial path, for searching a single taxon")
    results <- do.call("rbind", lapply(splist, 
                                       genus_search = genus_search, 
                                       out_path = out_path, 
                                       LCPposition_table = get_position(), 
                                       LCPspecies_table = get_species(), 
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
                                          LCPposition_table = get_position(), 
                                          LCPspecies_table = get_species(), 
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
  
  Output_Table_tmp <- data.frame(ID = NULL, Submitted_Name = NULL, Order = NULL, Family = NULL, Genus = NULL, Species = NULL, Infrasp = NULL, Infraspecies = NULL, Authors = NULL, Status = NULL, LCP_Accepted_Taxon = NULL, PL_Comparison = NULL, PL_Alternative = NULL, Score = NULL, Insertion = NULL, Deletion = NULL, Substitution = NULL)
  Output_Table <- data.frame(ID = NULL, Submitted_Name = NULL, Order = NULL, Family = NULL, Genus = NULL, Species = NULL, Infrasp = NULL, Infraspecies = NULL, Authors = NULL, Status = NULL, LCP_Accepted_Taxon = NULL, PL_Comparison = NULL, PL_Alternative = NULL, Score = NULL, Insertion = NULL, Deletion = NULL, Substitution = NULL)
  
  iter = 0
  namefirst = " "
  col <- dim(results)[2]
  row <- dim(results)[1]
  for (i in 1:row)  {
    name <- results[i,1]
    if(name != namefirst)
      iter = iter +1
    namefirst = name
    Output_Table_tmp <- data.frame(ID = iter, Submitted_Name = results[i,1], Order = results[i,2], Family = results[i,3], Genus = results[i,4], Species = results[i,5], Infrasp = results[i,6], Infraspecies = results[i,7], Authors = results[i,8], Status = results[i,9], LCP_Accepted_Taxon = results[i,10], PL_Comparison = results[i,11], PL_Alternative = results[i,12], Score = results[i,13], Insertion = results[i,14], Deletion = results[i,15], Substitution = results[i,16], stringsAsFactors = FALSE)
    Output_Table <- rbind(Output_Table, Output_Table_tmp)
  }
  if (!is.null(Output_Table$Species[1])) {
    if (save == TRUE) {
      exec_date <- paste(substring(Sys.time(),1,4),substring(Sys.time(),6,7),substring(Sys.time(),9,10),sep="")
      exec_time <- paste(substring(Sys.time(),12,13),substring(Sys.time(),15,16),substring(Sys.time(),18,19),sep="")
      out_code <- paste(exec_date, "_", exec_time, sep="")
      table_name <- paste("/LCP_results_",out_code,".csv",sep = "")
      pathstring <- paste(out_path, table_name, sep = "")
      write.table(Output_Table,pathstring,sep=",",row.names=FALSE)
    }
  } else {message(paste("the Leipzig Catalogue of Plants (LCP) was not able to identify any species name, try to type it differently or use the max.distance option"))}
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  message("---------------------------------------------------------")
  message("-    End of Leipzig Catalogue of Plants (LCP) search    -")
  message("---------------------------------------------------------")
  
  if (visualize == TRUE){
    View(Output_Table)
  }
  return(Output_Table)
}

read_data <-
function(pathstring, encoding) {
    search_table.sp <- try(read.csv(pathstring,
                                    header = TRUE,
                                    sep = "\t",
                                    fill = TRUE,
                                    colClasses = "character",
                                    as.is = TRUE,
                                    encoding = encoding
    ), silent = TRUE)
    
    N_rows <- dim(search_table.sp)[1]
    lista <- NULL
    for (i in 1:N_rows){
      row <- (search_table.sp[i,1])
      lista <- c(lista, row)
    }
    return(lista)
  }

#utils::globalVariables(c('LCPposition_table', 'LCPspecies_table'))