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
         out_path = getwd(), 
         verbose = 0) {

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
  
# query LPL List ----------------------------------------------------
  out_path <- getwd()
  
  data(LCPposition_table)
  data(LCPspecies_table)
  
# run the function
  if (length(splist) < 2) {
    if (verbose == 1) {print("serial path")}
    results <- do.call("rbind", lapply(splist, 
                                       genus_search = genus_search, 
                                       wd_path = wd_path, 
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
                                       verbose = verbose, 
                                       LCPsolver))
    
  } else {
    if (verbose == 1) {print("parallel path")}
    
    library(parallel)
    
    # Set up parallel environment
    cl <- makeCluster(max.cores)
    results <- do.call("rbind", parLapply(cl, 
                                          splist, 
                                          genus_search = genus_search, 
                                          wd_path = wd_path, 
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
                                          verbose = verbose, 
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
    Output_Table_tmp <- data.frame(ID = iter, Submitted_Name = results[i,1], Order = results[i,2], Family = results[i,3], Genus = results[i,4], Species = results[i,5], Infrasp = results[i,6], Infraspecies = results[i,7], Authors = results[i,8], Status = results[i,9], LCP_Accepted_Taxon = results[i,10], PL_Comparison = results[i,11], PL_Alternative = results[i,12], Score = results[i,13], Insertion = results[i,14], Deletion = results[i,15], Substitution = results[i,16])
    Output_Table <- rbind(Output_Table, Output_Table_tmp)
  }
  if (!is.null(Output_Table$Species[1])) {
    if (save == TRUE) {
      exec_date <- paste(substring(Sys.time(),1,4),substring(Sys.time(),6,7),substring(Sys.time(),9,10),sep="")
      exec_time <- paste(substring(Sys.time(),12,13),substring(Sys.time(),15,16),substring(Sys.time(),18,19),sep="")
      out_code <- paste(exec_date, "_", exec_time, sep="")
      table_name <- paste("/LCP_results_",out_code,".csv",sep = "")
      pathstring <- paste(wd_path, out_path, table_name, sep = "")
      write.table(Output_Table,pathstring,sep=",",row.names=FALSE)
    }
  } else {print(paste("the Leipzig Catalogue of Plants (LCP) was not able to identify any species name, try to type it differently or use the max.distance option"))}
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  print("---------------------------------------------------------")
  print("-    End of Leipzig Catalogue of Plants (LCP) search    -")
  print("---------------------------------------------------------")
  
  if (visualize == TRUE){
    View(Output_Table)
  }
  return(Output_Table)
}
