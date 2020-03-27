## Author: Alessandro Gentile
## Last Version: 2020/01/20

#' Search and solve one single plant name according to the Leipzig Catalogue of
#' Plants (LCVP).
#' 
#' Allow a taxonomic resolution of plant taxa names listed in the "Leipzig
#' Catalogue of Plants" (LCVP).  It applies a string comparison between the
#' submitted names from the user with the list of taxa listed in the
#' 'input.taxon' column of the 'LCVPspecies_table.rda' table, applying a fuzzy
#' match algorithm for possible orthographic errors solving. This function
#' takes care of one single taxon per time for each execution and it applies
#' the search for the submitted plant name trying to match the corresponding
#' taxon in the 'LCVPspecies_table' table.
#' 
#' 
#' @param sp A character vector specifying the input taxon, each element
#' including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name and author name
#' @param LCVPposition_table A character vector specifying the input taxon, each
#' element including genus and specific epithet and, potentially, infraspecific
#' rank, infraspecific name and author name
#' @param LCVPspecies_table A character vector specifying the input taxon, each
#' element including genus and specific epithet and, potentially, infraspecific
#' rank, infraspecific name and author name
#' @param genus_search Logical, FALSE (default). If TRUE, the function will
#' apply the fuzzy match algorithm also for the search of the genus name,
#' otherwise as default the search is applied only to the epithet, the
#' infraspecies and the name author.
#' @param max.distance is an integer value. It represents the maximum distance
#' (number of characters) allowed for a match when comparing the submitted name
#' with the closest name matches in the LCVP
#' @param encoding character vector, "UTF-8" (default). This value will allow
#' the user to set the specific codification of the strings
#' @param family_tab Logical, FALSE (default). If TRUE, the function will
#' return the list of plant taxa names belonging to the same family name
#' submitted by the user
#' @param order_tab Logical, FALSE (default). If TRUE, the function will return
#' the list of plant taxa names belonging to the same order name submitted by
#' the user
#' @param genus_tab Logical, FALSE (default). If TRUE, the function will return
#' the list of plant taxa names belonging to the same genus name submitted by
#' the user
#' @param infraspecies_tab Logical, FALSE (default). If TRUE, the function will
#' return also all the infraspecies names found for a submitted plant name
#' @param status Logical, TRUE (default). If FALSE, the function will return
#' not only the valid epithet for a species name but also all the possible
#' synonyms
#' @param save Logical. FALSE (default). If TRUE, the function will write the
#' output file as comma-separated format (.csv), saving it into the working
#' directory or in the directory already set through the 'out_path' option
#' @param visualize If TRUE the function will visualize the output search on
#' the 'Source Tab' of RStudio. This option has to be turned off (FALSE) if the
#' package is execute in a UNIX environment (from the command line) without
#' having a Graphical User Interface
#' @param version A character vector indicating the current version of the
#' package (current version is 1.0). A new version is under development
#' allowing the package to connect the web API that is under construction
#' @param out_path A character vector, which allow the user to define the path
#' where the output file has to be saved. The working directory is set as
#' default
#' @author Alessandro Gentile, Martin Freiberg, Marten Winter
#' @seealso https://idata.idiv.de/ddm/Data/ShowData/1806
#' @references The Leipzig Catalogue of Vascular Plants (LCVP) - An improved taxonomic
#' reference list for all known vascular plants

LCVPsolver <-
function(sp, 
         genus_search, 
         out_path, 
         LCVPposition_table, 
         LCVPspecies_table, 
         encoding, 
         max.distance, 
         family_tab, 
         order_tab, 
         genus_tab, 
         infraspecies_tab, 
         status, 
         save, 
         visualize, 
         version) {

# extract genus, species, infrasp, author from the typed name
  Infrasp_cat <- c("subsp.", "var.", "forma", "ssp.", "f.", "subvar.", "subf.")
  
  
  # correction of the hybrids for the genus name 
  # into the correct form for the LCVP standards
  sp_terms <- unlist(strsplit(sp, " "))
  N_terms <- length(sp_terms)
  if (toupper(sp_terms[1]) == 'X') {
    genus <- paste(toupper(substring(sp_terms[2], 1, 1)), 
                   tolower(substring(sp_terms[2], 2)), 
                   "_x", sep = "", collapse = " ")
    sp_terms <- paste(genus, paste(sp_terms[3:N_terms], 
                      sep = "", collapse = " "), sep = " ", collapse = " ")
    N_terms <- N_terms -1
  } else if(paste(unlist((strsplit(toupper(sp_terms[1]), "")))[1:2], 
                  sep = "", collapse = "") == 'X_') {
    # Translate characters in character vectors, 
    # in particular from upper to lower case or vice versa
    genus <- paste(toupper(substring(sp_terms[1], 3, 3)), 
                   tolower(substring(sp_terms[1], 4)), 
                   "_x", sep = "", collapse = " ")
    sp_terms <- paste(genus, paste(sp_terms[2:N_terms], 
                      sep = "", collapse = " "), 
                      sep = " ", collapse = " ")
  } else {genus <- paste(toupper(substring(sp_terms[1], 1, 1)), 
                         tolower(substring(sp_terms[1], 2)), 
                         sep = "", collapse = " ")}
  
  # correction of the hybrids for the epithet name 
  # into the correct form for the LCVP standards
  sp_terms <- unlist(strsplit(sp_terms, " "))
  N_terms <- length(sp_terms)
  if (N_terms > 1){
    if (tolower(sp_terms[2]) == 'x') {
      epithet <- paste(tolower(sp_terms[3]), "_x", 
                       sep = "", collapse = " ")
      autority <- paste(sp_terms[4:N_terms], 
                        sep = " ", collapse = " ")
      sp_terms <-  paste(genus, epithet, autority, sep = " ", collapse = " ")
      N_terms <- N_terms -1
    } else if(paste(unlist((strsplit(tolower(sp_terms[2]), "")))[1:2], 
                    sep = "", collapse = "") == 'x_') {
      # Translate characters in character vectors, 
      # in particular from upper to lower case or vice versa
      epithet <- paste(tolower(substring(sp_terms[2], 3)), 
                       "_x", sep = "", collapse = " ")
      autority <- paste(sp_terms[3:N_terms], 
                        sep = " ", collapse = " ")
      sp_terms <-  paste(genus, epithet, autority, 
                         sep = " ", collapse = " ")
    } else {genus <- paste(toupper(substring(sp_terms[1], 1, 1)), 
                           tolower(substring(sp_terms[1], 2)), 
                           sep = "", collapse = " ")}
  }
  
  sp_terms <- unlist(strsplit(sp_terms, " "))
  N_terms <- length(sp_terms)
  if (N_terms == 1) {
    species <- NULL
    infrasp <- NULL
    infrasp_name <- NULL
    author <- FALSE
    auth_name <- NULL
    full_name <- genus
  } else if (N_terms == 2) {
    species <- sp_terms[2]
    species <- tolower(species)
    infrasp <- NULL
    infrasp_name <- NULL
    author <- FALSE
    auth_name <- NULL
    full_name <- paste(genus,species, sep = " ")
    message('Search based on genus and epithet name')
  } else if (N_terms > 2 && 
             ((tolower(sp_terms[3]) %in% Infrasp_cat) == FALSE)) { 
    species <- sp_terms[2]
    species <- tolower(species)
    infrasp <- NULL
    infrasp_name <- NULL
    author <- TRUE
    auth_name <- paste0(sp_terms[3:N_terms], collapse = " ")
    full_name <- paste(genus,species,auth_name, sep = " ")
    message('Search based on genus, epithet and author name')
  } else if (N_terms <= 4 && ((tolower(sp_terms[3]) %in% Infrasp_cat) == TRUE)) {
    species <- sp_terms[2]
    species <- tolower(species)
    infrasp <- sp_terms[3]
    infrasp <- tolower(infrasp)
    infrasp_name <- sp_terms[4]
    infrasp_name <- tolower(infrasp_name)
    author <- FALSE
    auth_name <- NULL
    full_name <- paste(genus,species,infrasp,infrasp_name, sep = " ")
    message('Search based on genus, epithet and infraspecies name')
  } else if (N_terms > 4 && ((tolower(sp_terms[3]) %in% Infrasp_cat) == TRUE)) {
    species <- sp_terms[2]
    species <- tolower(species)
    infrasp <- sp_terms[3]
    infrasp <- tolower(infrasp)
    infrasp_name <- sp_terms[4]
    infrasp_name <- tolower(infrasp_name)
    author <- TRUE
    auth_name <- paste0(sp_terms[5:N_terms], collapse = " ")
    full_name <- paste(genus,species,infrasp,infrasp_name,auth_name, sep = " ")
    message('Search based on genus, epithet, infraspecies and author name')
  }
  
  # Genus_table_tmp     <- data.frame(Submitted_Name = NULL, Order = NULL, Family = NULL, Genus = NULL, Species = NULL, Infrasp = NULL, Infraspecies = NULL, Authors = NULL, Status = NULL, LCVP_Accepted_Taxon = NULL, PL_Comparison = NULL, PL_Alternative = NULL, Score = NULL, Insertion = NULL, Deletion = NULL, Substitution = NULL)
  # Genus_table_final   <- data.frame(Submitted_Name = NULL, Order = NULL, Family = NULL, Genus = NULL, Species = NULL, Infrasp = NULL, Infraspecies = NULL, Authors = NULL, Status = NULL, LCVP_Accepted_Taxon = NULL, PL_Comparison = NULL, PL_Alternative = NULL, Score = NULL, Insertion = NULL, Deletion = NULL, Substitution = NULL)         
  # Species_table_tmp   <- data.frame(Submitted_Name = NULL, Order = NULL, Family = NULL, Genus = NULL, Species = NULL, Infrasp = NULL, Infraspecies = NULL, Authors = NULL, Status = NULL, LCVP_Accepted_Taxon = NULL, PL_Comparison = NULL, PL_Alternative = NULL, Score = NULL, Insertion = NULL, Deletion = NULL, Substitution = NULL)
  # Species_table_final <- data.frame(Submitted_Name = NULL, Order = NULL, Family = NULL, Genus = NULL, Species = NULL, Infrasp = NULL, Infraspecies = NULL, Authors = NULL, Status = NULL, LCVP_Accepted_Taxon = NULL, PL_Comparison = NULL, PL_Alternative = NULL, Score = NULL, Insertion = NULL, Deletion = NULL, Substitution = NULL)
  # Infrasp_table_tmp   <- data.frame(Submitted_Name = NULL, Order = NULL, Family = NULL, Genus = NULL, Species = NULL, Infrasp = NULL, Infraspecies = NULL, Authors = NULL, Status = NULL, LCVP_Accepted_Taxon = NULL, PL_Comparison = NULL, PL_Alternative = NULL, Score = NULL, Insertion = NULL, Deletion = NULL, Substitution = NULL)
  # Infrasp_table_final <- data.frame(Submitted_Name = NULL, Order = NULL, Family = NULL, Genus = NULL, Species = NULL, Infrasp = NULL, Infraspecies = NULL, Authors = NULL, Status = NULL, LCVP_Accepted_Taxon = NULL, PL_Comparison = NULL, PL_Alternative = NULL, Score = NULL, Insertion = NULL, Deletion = NULL, Substitution = NULL)
  # Matched_table_tmp   <- data.frame(Submitted_Name = NULL, Order = NULL, Family = NULL, Genus = NULL, Species = NULL, Infrasp = NULL, Infraspecies = NULL, Authors = NULL, Status = NULL, LCVP_Accepted_Taxon = NULL, PL_Comparison = NULL, PL_Alternative = NULL, Score = NULL, Insertion = NULL, Deletion = NULL, Substitution = NULL)
  # Matched_table_final <- data.frame(Submitted_Name = NULL, Order = NULL, Family = NULL, Genus = NULL, Species = NULL, Infrasp = NULL, Infraspecies = NULL, Authors = NULL, Status = NULL, LCVP_Accepted_Taxon = NULL, PL_Comparison = NULL, PL_Alternative = NULL, Score = NULL, Insertion = NULL, Deletion = NULL, Substitution = NULL)
  
  # SIMPLIFICATION ALEX 
  Genus_table_tmp     <- data.frame(Submitted_Name = NULL, Order = NULL, Family = NULL, Genus = NULL, 
                                    Species = NULL, Infrasp = NULL, Infraspecies = NULL, 
                                    Authors = NULL, Status = NULL, LCVP_Accepted_Taxon = NULL, 
                                    PL_Comparison = NULL, PL_Alternative = NULL, Score = NULL, 
                                    Insertion = NULL, Deletion = NULL, Substitution = NULL)
  
  Genus_table_final   <- 
    Species_table_tmp   <-
    Species_table_final <- 
    Infrasp_table_tmp   <- 
    Infrasp_table_final <-
    Matched_table_tmp   <- 
    Matched_table_final <-
    Genus_table_tmp
  genus_found <- FALSE
  author_found <- FALSE
  
# run the fuzzy match search engine for the genus ----------------------------------------------------
  
  genus_ini <- paste(toupper(substring(genus, 1, 1)), tolower(substring(genus, 2, 3)), sep = "")
  col <- dim(LCVPposition_table)[2]
  row <- dim(LCVPposition_table)[1]
  Start_Position <- 1
  End_Position <- row
  for (i in 1:row)  {
    LCVP_genus_ini <- LCVPposition_table[i,2]
    if (LCVP_genus_ini == genus_ini) {
      Start_Position <- LCVPposition_table[i,1]
      End_Position <- LCVPposition_table[i+1,1]
      if (is.na(End_Position)){End_Position <- dim(LCVPspecies_table)[1]}
    }
  }
  # option to select all the name belonging to the searched order, family or genus name
  if (N_terms < 2) {
    if (N_terms == 1 && (order_tab == TRUE || family_tab == TRUE || genus_tab == TRUE)) {
      message('search for the only genus, family or order name')
      if (genus_tab == TRUE && family_tab == FALSE && order_tab == FALSE) {
        matched_name <- agrep(genus, LCVPspecies_table$Input.Taxon, value = TRUE, max.distance = max.distance)
        matched_pos <- agrep(genus, LCVPspecies_table$Input.Taxon, value = FALSE, max.distance = max.distance)
      } else if (genus_tab == FALSE && family_tab == TRUE && order_tab == FALSE){
        matched_name <- agrep(genus, LCVPspecies_table$Family, value = TRUE, max.distance = max.distance)
        matched_pos <- agrep(genus, LCVPspecies_table$Family, value = FALSE, max.distance = max.distance)
      } else if (genus_tab == FALSE && family_tab == FALSE && order_tab == TRUE){
        matched_name <- agrep(genus, LCVPspecies_table$Order, value = TRUE, max.distance = max.distance)
        matched_pos <- agrep(genus, LCVPspecies_table$Order, value = FALSE, max.distance = max.distance)
      } else {
        matched_name <- NULL
        matched_pos <- NULL
      }
      if (length(matched_pos) > 0) {
        matched_genus <- NULL
        
        #for (i in 1:length(matched_pos)) {
        for (i in seq_along(matched_pos)) {
          matched_genus <- rbind(matched_genus, unlist(strsplit(matched_name[i], " "))[1])
        }
        
        ddf <- abs(nchar(matched_genus) - nchar(genus))
        if (length(matched_genus) > 0) {
          matched_genus <- matched_genus[ddf == min(ddf)]
          matched_pos <- matched_pos[ddf == min(ddf)]
        }
        result_name <- NULL
        result_pos <- NULL
        
        for (i in seq_along(matched_pos)) {
          result_name <- rbind(result_name, matched_name[i])
          iter <- matched_pos[i]
          LCVP_genus <- unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " "))[1]
          LCVP_species <- unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " "))[2]
          if (genus %in% matched_genus) {
            score <- drop(attr(adist(matched_genus[i], genus, counts = TRUE), "counts"))
            if (length(unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " "))) < 3) {   # option there is only the genus and epithet name
              LCVP_Infrasp <- "species"
              LCVP_Infrasp_name <- " "
              LCVP_Author_name <- " "
            } else {
              LCVP_Infrasp <- "species"
              LCVP_Infrasp_name <- " "
              LCVP_Author_name <- paste(unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " "))[3:length(unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " ")))],sep="", collapse = " ")
              for (n in 1:3){
                for (m in seq_along(Infrasp_cat)){
                  if(unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " "))[n] == Infrasp_cat[m]){
                    LCVP_Infrasp <- unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " "))[3]
                    LCVP_Infrasp_name <- unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " "))[4]
                    LCVP_Author_name <- paste(unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " "))[5:length(unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " ")))],sep="", collapse = " ")
                  }
                }
              }
            }
            Genus_table_tmp <- data.frame(Submitted_Name = full_name, 
                                          Order = LCVPspecies_table[matched_pos[i],7], 
                                          Family = LCVPspecies_table[matched_pos[i],6], 
                                          Genus = unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " "))[1], 
                                          Species = unlist(strsplit(LCVPspecies_table[matched_pos[i],1], " "))[2], 
                                          Infrasp = LCVP_Infrasp, 
                                          Infraspecies = LCVP_Infrasp_name, 
                                          Authors = LCVP_Author_name, 
                                          Status = LCVPspecies_table[matched_pos[i],2], 
                                          LCVP_Accepted_Taxon = LCVPspecies_table[matched_pos[i],5], 
                                          PL_Comparison = LCVPspecies_table[matched_pos[i],3], 
                                          PL_Alternative = LCVPspecies_table[matched_pos[i],4], 
                                          Score = 'matched', 
                                          Insertion = score[1], 
                                          Deletion = score[2], 
                                          Substitution = score[3])
            Genus_table_final <- rbind(Genus_table_final, Genus_table_tmp)
          }
        }
      }
    }
  } else {
    col <- dim(LCVPspecies_table)[2]
    row <- dim(LCVPspecies_table)[1]
    # loop to select all the name with the same genus name and save this list into Genus_table_final table
    for (i in Start_Position:End_Position)  {
          LCVP_genus <- unlist(strsplit(LCVPspecies_table[i,1], " "))[1]
          LCVP_species <- unlist(strsplit(LCVPspecies_table[i,1], " "))[2]
          if (LCVP_genus == genus) {
            genus_found <- TRUE
            # option when the first letter of the ephitet name is correct or all the letters epithet name are correct but not the first
            if ((substring(LCVP_species, 1, 1) == substring(species, 1, 1)) || (substring(LCVP_species, 2, 100) == substring(species, 2, 100))) {
              if (length(unlist(strsplit(LCVPspecies_table[i,1], " "))) < 3) {
                LCVP_Infrasp <- "species"
                LCVP_Infrasp_name <- " "
                LCVP_Author_name <- " "
              } else {
                LCVP_Infrasp <- "species"
                LCVP_Infrasp_name <- " "
                LCVP_Author_name <- paste(unlist(strsplit(LCVPspecies_table[i,1], " "))[3:length(unlist(strsplit(LCVPspecies_table[i,1], " ")))],sep="", collapse = " ")
                for (n in 1:3){
                  for (m in seq_along(Infrasp_cat)){
                    if(unlist(strsplit(LCVPspecies_table[i,1], " "))[n] == Infrasp_cat[m]){
                      LCVP_Infrasp <- unlist(strsplit(LCVPspecies_table[i,1], " "))[3]
                      LCVP_Infrasp_name <- unlist(strsplit(LCVPspecies_table[i,1], " "))[4]
                      LCVP_Author_name <- paste(unlist(strsplit(LCVPspecies_table[i,1], " "))[5:length(unlist(strsplit(LCVPspecies_table[i,1], " ")))],sep="", collapse = " ")
                    }
                  }
                }
              }
              Genus_table_tmp <- data.frame(Submitted_Name = full_name, 
                                            Order = LCVPspecies_table[i,7], 
                                            Family = LCVPspecies_table[i,6], 
                                            Genus = unlist(strsplit(LCVPspecies_table[i,1], " "))[1], 
                                            Species = unlist(strsplit(LCVPspecies_table[i,1], " "))[2], 
                                            Infrasp = LCVP_Infrasp, 
                                            Infraspecies = LCVP_Infrasp_name, 
                                            Authors = LCVP_Author_name, 
                                            Status = LCVPspecies_table[i,2], 
                                            LCVP_Accepted_Taxon = LCVPspecies_table[i,5], 
                                            PL_Comparison = LCVPspecies_table[i,3], 
                                            PL_Alternative = LCVPspecies_table[i,4], 
                                            Score = 'matched', 
                                            Insertion = 0, 
                                            Deletion = 0, 
                                            Substitution = 0)
              Genus_table_final <- rbind(Genus_table_final, Genus_table_tmp)
            }
          }
    }
    # option when the ephitet has a mispelling in the first letter of the name 
    if (!is.null(Genus_table_final$Genus[1]) && (species %in% Genus_table_final$Species) == FALSE) {
      if (substring(species, 2, 100) != substring(Genus_table_final$Species[length(Genus_table_final$Species)], 2, 100)) {
        if ((substring(species, 1, 1) == substring(Genus_table_final$Species[length(Genus_table_final$Species)], 1, 1)) &&
          (substring(species, 1, 1) == substring(Genus_table_final$Species[1], 1, 1))) {
        
          Genus_table_final <- data.frame(Submitted_Name = NULL, 
                                          Order = NULL, 
                                          Family = NULL, 
                                          Genus = NULL, 
                                          Species = NULL, 
                                          Infrasp = NULL, 
                                          Infraspecies = NULL, 
                                          Authors = NULL, 
                                          Status = NULL, 
                                          LCVP_Accepted_Taxon = NULL, 
                                          PL_Comparison = NULL, 
                                          PL_Alternative = NULL, 
                                          Score = NULL, 
                                          Insertion = NULL, 
                                          Deletion = NULL, 
                                          Substitution = NULL)         
          matched_name <- agrep(paste(genus,species,sep = " "), LCVPspecies_table$Input.Taxon, value = TRUE, max.distance = max.distance)
          matched_pos <- agrep(paste(genus,species,sep = " "), LCVPspecies_table$Input.Taxon, value = FALSE, max.distance = max.distance)
          if (length(matched_pos) > 0) {
            for (i in seq_along(matched_pos)) {
              LCVP_genus <- unlist(strsplit(matched_name[i], " "))[1]
              LCVP_species <- unlist(strsplit(matched_name[i], " "))[2]
              score <- drop(attr(adist(unlist(strsplit(matched_name[i], " "))[2], species, counts = TRUE), "counts"))
              if (length(unlist(strsplit(matched_name[i], " "))) < 3) {   # condizione in cui c'e` solo il genere e specie
                LCVP_Infrasp <- "species"
                LCVP_Infrasp_name <- " "
                LCVP_Author_name <- " "
              } else {
                LCVP_Infrasp <- "species"
                LCVP_Infrasp_name <- " "
                LCVP_Author_name <- paste(unlist(strsplit(matched_name[i], " "))[3:length(unlist(strsplit(matched_name[i], " ")))],sep="", collapse = " ")
                for (n in 1:3){
                  for (m in seq_along(Infrasp_cat)){
                    if(unlist(strsplit(matched_name[i], " "))[n] == Infrasp_cat[m]){
                      LCVP_Infrasp <- unlist(strsplit(matched_name[i], " "))[3]
                      LCVP_Infrasp_name <- unlist(strsplit(matched_name[i], " "))[4]
                      LCVP_Author_name <- paste(unlist(strsplit(matched_name[i], " "))[5:length(unlist(strsplit(matched_name[i], " ")))],sep="", collapse = " ")
                    }
                  }
                }
              }
              Genus_table_tmp <- data.frame(Submitted_Name = full_name, 
                                            Order = LCVPspecies_table[matched_pos[i],7], 
                                            Family = LCVPspecies_table[matched_pos[i],6], 
                                            Genus = LCVP_genus, 
                                            Species = LCVP_species, 
                                            Infrasp = LCVP_Infrasp, 
                                            Infraspecies = LCVP_Infrasp_name, 
                                            Authors = LCVP_Author_name, 
                                            Status = LCVPspecies_table[matched_pos[i],2], 
                                            LCVP_Accepted_Taxon = LCVPspecies_table[matched_pos[i],5], 
                                            PL_Comparison = LCVPspecies_table[matched_pos[i],3], 
                                            PL_Alternative = LCVPspecies_table[matched_pos[i],4], 
                                            Score = 'matched', 
                                            Insertion = 0, 
                                            Deletion = 0, 
                                            Substitution = 0)
              Genus_table_final <- rbind(Genus_table_final, Genus_table_tmp)
            }
          } else {Genus_table_final <- data.frame(Submitted_Name = full_name, 
                                                  Order = "", 
                                                  Family = "", 
                                                  Genus = "", 
                                                  Species = "", 
                                                  Infrasp = "", 
                                                  Infraspecies = "", 
                                                  Authors = "", 
                                                  Status = "", 
                                                  LCVP_Accepted_Taxon = "", 
                                                  PL_Comparison = "", 
                                                  PL_Alternative = "", 
                                                  Score = 'Epithet name not found', 
                                                  Insertion = 0, 
                                                  Deletion = 0, 
                                                  Substitution = 0)}
        }
      }
    } else if (is.null(Genus_table_final$Genus[1]) && (genus_found == TRUE || genus_search == TRUE)) {
          matched_name <- agrep(genus, LCVPspecies_table$Input.Taxon, value = TRUE, max.distance = max.distance)
          matched_pos <- agrep(genus, LCVPspecies_table$Input.Taxon, value = FALSE, max.distance = max.distance)
          matched_name2 <- agrep(species, matched_name, value = TRUE, max.distance = max.distance)
          matched_pos2 <- agrep(species, matched_name, value = FALSE, max.distance = max.distance)
          matched_genus <- NULL
          for (i in seq_along(matched_pos2)) {
            matched_genus <- rbind(matched_genus, unlist(strsplit(matched_name2[i], " "))[1])
          }
          ddf <- abs(nchar(matched_genus) - nchar(genus))
          if (length(matched_genus) > 0) {
            matched_genus <- matched_genus[ddf == min(ddf)]
            ddf <- abs(nchar(matched_genus) - nchar(genus))
          }
          result_name <- NULL
          result_pos <- NULL
          if (length(matched_pos) > 0 && length(matched_pos2) > 0) {
            for (i in seq_along(matched_pos2)) {
              result_name <- rbind(result_name, matched_name2[i])
              iter <- matched_pos2[i]
              result_pos <- rbind(result_pos, matched_pos[matched_pos2[i]])
              LCVP_genus <- unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " "))[1]
              LCVP_species <- unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " "))[2]
              if (LCVP_genus %in% matched_genus) {
                score <- drop(attr(adist(LCVP_genus, genus, counts = TRUE), "counts"))
                if (score[1] == 0 && score[2] == 0 && score[3] == 0){score_name <- 'matched'} else {score_name <- 'misspelling: Genus'}
                if (length(unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " "))) < 3) {   # condizione in cui c'e` solo il genere e specie
                  LCVP_Infrasp <- "species"
                  LCVP_Infrasp_name <- " "
                  LCVP_Author_name <- " "
                } else {
                  LCVP_Infrasp <- "species"
                  LCVP_Infrasp_name <- " "
                  LCVP_Author_name <- paste(unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " "))[3:length(unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " ")))],sep="", collapse = " ")
                  for (n in 1:3){
                    for (m in seq_along(Infrasp_cat)){
                      if(unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " "))[n] == Infrasp_cat[m]){
                        LCVP_Infrasp <- unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " "))[3]
                        LCVP_Infrasp_name <- unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " "))[4]
                        LCVP_Author_name <- paste(unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " "))[5:length(unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " ")))],sep="", collapse = " ")
                      }
                    }
                  }
                }
                Genus_table_tmp <- data.frame(Submitted_Name = full_name, 
                                              Order = LCVPspecies_table[matched_pos[iter],7], 
                                              Family = LCVPspecies_table[matched_pos[iter],6], 
                                              Genus = unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " "))[1], 
                                              Species = unlist(strsplit(LCVPspecies_table[matched_pos[iter],1], " "))[2], 
                                              Infrasp = LCVP_Infrasp, 
                                              Infraspecies = LCVP_Infrasp_name, 
                                              Authors = LCVP_Author_name, 
                                              Status = LCVPspecies_table[matched_pos[iter],2], 
                                              LCVP_Accepted_Taxon = LCVPspecies_table[matched_pos[iter],5], 
                                              PL_Comparison = LCVPspecies_table[matched_pos[iter],3], 
                                              PL_Alternative = LCVPspecies_table[matched_pos[iter],4], 
                                              Score = score_name, 
                                              Insertion = score[1], 
                                              Deletion = score[2], 
                                              Substitution = score[3])
                Genus_table_final <- rbind(Genus_table_final, Genus_table_tmp)
              }
            }
          } else if (length(matched_pos) > 0 && length(matched_pos2) == 0) {
            Genus_table_tmp <- data.frame(Submitted_Name = full_name, 
                                          Order = "", Family = "", 
                                          Genus = unlist(strsplit(matched_name[1], " "))[1], 
                                          Species = "", 
                                          Infrasp = "", 
                                          Infraspecies = "", 
                                          Authors = "", 
                                          Status = "", 
                                          LCVP_Accepted_Taxon = "", 
                                          PL_Comparison = "", 
                                          PL_Alternative = "", 
                                          Score = 'Epithet name not found', 
                                          Insertion = 0, 
                                          Deletion = 0, 
                                          Substitution = 0)
            Genus_table_final <- rbind(Genus_table_final, Genus_table_tmp)
          }
    }
  }
  if (is.null(Genus_table_final$Genus[1])){
    Matched_table_final <- data.frame(Submitted_Name = full_name, 
                                      Order = "", 
                                      Family = "", 
                                      Genus = "", 
                                      Species = "", 
                                      Infrasp = "", 
                                      Infraspecies = "", 
                                      Authors = "", 
                                      Status = "", 
                                      LCVP_Accepted_Taxon = "", 
                                      PL_Comparison = "", 
                                      PL_Alternative = "", 
                                      Score = 'Genus name not found', 
                                      Insertion = 0, 
                                      Deletion = 0, 
                                      Substitution = 0)
  } else {
  # run the fuzzy match search engine for the species ----------------------------------------------------
    if (!is.null(species)) {
        matched_name <- agrep(species, Genus_table_final$Species, value = TRUE, max.distance = max.distance)
        matched_pos <- agrep(species, Genus_table_final$Species, value = FALSE, max.distance = max.distance)
        ddf <- abs(nchar(matched_name) - nchar(species))
        if (length(matched_name) > 0) {
          result <- matched_name[ddf == min(ddf)]
          ddf <- abs(nchar(result) - nchar(species))
        }
        if (length(matched_name) > 0 && (species %in% result) == FALSE) {
          for (j in seq_along(matched_name)) {
            if (matched_name[j] %in% result || matched_name[j] %in% result){
              Gen_Tab_genus <- (Genus_table_final$Genus)[matched_pos[j]]
              Gen_Tab_species <- (Genus_table_final$Species)[matched_pos[j]]
              Gen_Tab_Infrasp <- (Genus_table_final$Infrasp)[matched_pos[j]]
              Gen_Tab_Infrasp_name <- (Genus_table_final$Infraspecies)[matched_pos[j]]
              Gen_Tab_Author_name <- (Genus_table_final$Authors)[matched_pos[j]]
              Gen_Tab_Score <- (Genus_table_final$Score)[matched_pos[j]]
              score <- drop(attr(adist(matched_name[j], species, counts = TRUE), "counts"))
              ins <- as.numeric((Genus_table_final$Insertion)[matched_pos[j]]) + score[1]
              del <- as.numeric((Genus_table_final$Deletion)[matched_pos[j]]) + score[2]
              sub <- as.numeric((Genus_table_final$Substitution)[matched_pos[j]]) + score[3]
              if((score[1] > 0 || score[2] > 0 || score[3] > 0) && Gen_Tab_Score != 'matched'){
                  NEW_Tab_Score <- paste(Gen_Tab_Score,', Epithet', sep="")
              } else if ((score[1] > 0 || score[2] > 0 || score[3] > 0) && Gen_Tab_Score == 'matched'){
                  NEW_Tab_Score <- 'misspelling: Epithet'
              } else {
                  NEW_Tab_Score <- 'misspelling: Epithet'
              }
              Species_table_tmp <- data.frame(Submitted_Name = Genus_table_final$Submitted_Name[matched_pos[j]], 
                                              Order = Genus_table_final$Order[matched_pos[j]], 
                                              Family = Genus_table_final$Family[matched_pos[j]], 
                                              Genus = Genus_table_final$Genus[matched_pos[j]], 
                                              Species = Genus_table_final$Species[matched_pos[j]], 
                                              Infrasp = Gen_Tab_Infrasp, 
                                              Infraspecies = Gen_Tab_Infrasp_name, 
                                              Authors = Gen_Tab_Author_name, 
                                              Status = Genus_table_final$Status[matched_pos[j]], 
                                              LCVP_Accepted_Taxon = Genus_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                                              PL_Comparison = Genus_table_final$PL_Comparison[matched_pos[j]], 
                                              PL_Alternative = Genus_table_final$PL_Alternative[matched_pos[j]], 
                                              Score = NEW_Tab_Score, 
                                              Insertion = ins, 
                                              Deletion = del, 
                                              Substitution = sub)
              Species_table_final <- rbind(Species_table_final, Species_table_tmp)
            }
          }
        } else if (length(matched_name) > 0 && (species %in% result) == TRUE) {
          for (j in seq_along(matched_name)) {
            if (matched_name[j] == species || matched_name[j] == paste(species,'_x', sep = "")){
              Gen_Tab_genus <- (Genus_table_final$Genus)[matched_pos[j]]
              Gen_Tab_species <- (Genus_table_final$Species)[matched_pos[j]]
              Gen_Tab_Infrasp <- (Genus_table_final$Infrasp)[matched_pos[j]]
              Gen_Tab_Infrasp_name <- (Genus_table_final$Infraspecies)[matched_pos[j]]
              Gen_Tab_Author_name <- (Genus_table_final$Authors)[matched_pos[j]]
              Gen_Tab_Score <- (Genus_table_final$Score)[matched_pos[j]]
              score <- drop(attr(adist(matched_name[j], species, counts = TRUE), "counts"))
              ins <- as.numeric((Genus_table_final$Insertion)[matched_pos[j]]) + score[1]
              del <- as.numeric((Genus_table_final$Deletion)[matched_pos[j]]) + score[2]
              sub <- as.numeric((Genus_table_final$Substitution)[matched_pos[j]]) + score[3]
              NEW_Tab_Score <- Gen_Tab_Score
              Species_table_tmp <- data.frame(Submitted_Name = Genus_table_final$Submitted_Name[matched_pos[j]], 
                                              Order = Genus_table_final$Order[matched_pos[j]], 
                                              Family = Genus_table_final$Family[matched_pos[j]], 
                                              Genus = Genus_table_final$Genus[matched_pos[j]], 
                                              Species = Genus_table_final$Species[matched_pos[j]], 
                                              Infrasp = Gen_Tab_Infrasp, 
                                              Infraspecies = Gen_Tab_Infrasp_name, 
                                              Authors = Gen_Tab_Author_name, 
                                              Status = Genus_table_final$Status[matched_pos[j]], 
                                              LCVP_Accepted_Taxon = Genus_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                                              PL_Comparison = Genus_table_final$PL_Comparison[matched_pos[j]], 
                                              PL_Alternative = Genus_table_final$PL_Alternative[matched_pos[j]], 
                                              Score = NEW_Tab_Score, 
                                              Insertion = ins, 
                                              Deletion = del, 
                                              Substitution = sub)
              Species_table_final <- rbind(Species_table_final, Species_table_tmp)
            }
          }
        }
        
# run the fuzzy match search engine for the infraspecies ----------------------------------------------------
        if (!is.null(Species_table_final$Genus[1])) {
          if (!is.null(infrasp)) {
            matched_name <- agrep(infrasp_name, Species_table_final$Infraspecies, value = TRUE, max.distance = max.distance)
            matched_pos <- agrep(infrasp_name, Species_table_final$Infraspecies, value = FALSE, max.distance = max.distance)
            ddf <- abs(nchar(matched_name) - nchar(infrasp_name))
            if (length(matched_name) > 0) {
              result <- matched_name[ddf == min(ddf)]
              ddf <- abs(nchar(result) - nchar(species))
            }
            if (length(matched_name) > 0 && (infrasp_name %in% result) == FALSE) {
              for (j in seq_along(matched_name)) {
                if (matched_name[j] %in% result){
                  Spec_Tab_genus <- (Species_table_final$Genus)[matched_pos[j]]
                  Spec_Tab_species <- (Species_table_final$Species)[matched_pos[j]]
                  Spec_Tab_Infrasp <- (Species_table_final$Infrasp)[matched_pos[j]]
                  Spec_Tab_Infrasp_name <- (Species_table_final$Infraspecies)[matched_pos[j]]
                  Spec_Tab_Author_name <- (Species_table_final$Authors)[matched_pos[j]]
                  Spec_Tab_Score <- (Species_table_final$Score)[matched_pos[j]]
                  score <- drop(attr(adist(matched_name[j], infrasp_name, counts = TRUE), "counts"))
                  ins <- as.numeric((Species_table_final$Insertion)[matched_pos[j]]) + score[1]
                  del <- as.numeric((Species_table_final$Deletion)[matched_pos[j]]) + score[2]
                  sub <- as.numeric((Species_table_final$Substitution)[matched_pos[j]]) + score[3]
                  if((score[1] > 0 || score[2] > 0 || score[3] > 0) && Spec_Tab_Score != 'matched'){
                      NEW_Tab_Score <- paste(Spec_Tab_Score,', Infrasp.', sep="")
                  } else if ((score[1] > 0 || score[2] > 0 || score[3] > 0) && Spec_Tab_Score == 'matched'){
                    NEW_Tab_Score <- 'misspelling: Infrasp.'
                  } else {
                      NEW_Tab_Score <- Spec_Tab_Score
                  }
                  Infrasp_table_tmp <- data.frame(Submitted_Name = Species_table_final$Submitted_Name[matched_pos[j]], 
                                                  Order = Species_table_final$Order[matched_pos[j]], 
                                                  Family = Species_table_final$Family[matched_pos[j]], 
                                                  Genus = Species_table_final$Genus[matched_pos[j]], 
                                                  Species = Species_table_final$Species[matched_pos[j]], 
                                                  Infrasp = Spec_Tab_Infrasp, 
                                                  Infraspecies = Spec_Tab_Infrasp_name, 
                                                  Authors = Spec_Tab_Author_name, 
                                                  Status = Species_table_final$Status[matched_pos[j]], 
                                                  LCVP_Accepted_Taxon = Species_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                                                  PL_Comparison = Species_table_final$PL_Comparison[matched_pos[j]], 
                                                  PL_Alternative = Species_table_final$PL_Alternative[matched_pos[j]], 
                                                  Score = NEW_Tab_Score, 
                                                  Insertion = ins, 
                                                  Deletion = del, 
                                                  Substitution = sub)
                  Infrasp_table_final <- rbind(Infrasp_table_final, Infrasp_table_tmp)
                }
              }
            } else if (length(matched_name) > 0 && (infrasp_name %in% result) == TRUE) {
              for (j in seq_along(matched_name)) {
                if (matched_name[j] == infrasp_name){
                  Spec_Tab_genus <- (Species_table_final$Genus)[matched_pos[j]]
                  Spec_Tab_species <- (Species_table_final$Species)[matched_pos[j]]
                  Spec_Tab_Infrasp <- (Species_table_final$Infrasp)[matched_pos[j]]
                  Spec_Tab_Infrasp_name <- (Species_table_final$Infraspecies)[matched_pos[j]]
                  Spec_Tab_Author_name <- (Species_table_final$Authors)[matched_pos[j]]
                  Spec_Tab_Score <- (Species_table_final$Score)[matched_pos[j]]
                  score <- drop(attr(adist(matched_name[j], infrasp_name, counts = TRUE), "counts"))
                  ins <- as.numeric((Species_table_final$Insertion)[matched_pos[j]]) + score[1]
                  del <- as.numeric((Species_table_final$Deletion)[matched_pos[j]]) + score[2]
                  sub <- as.numeric((Species_table_final$Substitution)[matched_pos[j]]) + score[3]
                  NEW_Tab_Score <- Spec_Tab_Score
                  Infrasp_table_tmp <- data.frame(Submitted_Name = Species_table_final$Submitted_Name[matched_pos[j]], 
                                                  Order = Species_table_final$Order[matched_pos[j]], 
                                                  Family = Species_table_final$Family[matched_pos[j]], 
                                                  Genus = Species_table_final$Genus[matched_pos[j]], 
                                                  Species = Species_table_final$Species[matched_pos[j]], 
                                                  Infrasp = Spec_Tab_Infrasp, 
                                                  Infraspecies = Spec_Tab_Infrasp_name, 
                                                  Authors = Spec_Tab_Author_name, 
                                                  Status = Species_table_final$Status[matched_pos[j]], 
                                                  LCVP_Accepted_Taxon = Species_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                                                  PL_Comparison = Species_table_final$PL_Comparison[matched_pos[j]], 
                                                  PL_Alternative = Species_table_final$PL_Alternative[matched_pos[j]], 
                                                  Score = NEW_Tab_Score, 
                                                  Insertion = ins, 
                                                  Deletion = del, 
                                                  Substitution = sub)
                  Infrasp_table_final <- rbind(Infrasp_table_final, Infrasp_table_tmp)
                }
              }
            }
# run the fuzzy match search engine for the author when infrasp is already specified ----------------------------------------------------
            if (!is.null(Infrasp_table_final$Genus[1]) && author == TRUE) {
                matched_name <- agrep(auth_name, Infrasp_table_final$Authors, value = TRUE, max.distance = max.distance)
                matched_pos <- agrep(auth_name, Infrasp_table_final$Authors, value = FALSE, max.distance = max.distance)
                ddf <- abs(nchar(matched_name) - nchar(auth_name))
                if (length(matched_name) > 0) {
                  result <- matched_name[ddf == min(ddf)]
                  ddf <- abs(nchar(result) - nchar(auth_name))
                }
                if (length(matched_name) > 0 && (auth_name %in% result) == FALSE) {
                  for (j in seq_along(matched_name)) {
                    if (matched_name[j] %in% result){
                      Infrasp_Tab_genus <- (Infrasp_table_final$Genus)[matched_pos[j]]
                      Infrasp_Tab_species <- (Infrasp_table_final$Species)[matched_pos[j]]
                      Infrasp_Tab_Infrasp <- (Infrasp_table_final$Infrasp)[matched_pos[j]]
                      Infrasp_Tab_Infrasp_name <- (Infrasp_table_final$Infraspecies)[matched_pos[j]]
                      Infrasp_Tab_Author_name <- (Infrasp_table_final$Authors)[matched_pos[j]]
                      Infrasp_Tab_Score <- (Infrasp_table_final$Score)[matched_pos[j]]
                      Infrasp_Tab_Insertion <- (Infrasp_table_final$Insertion)[matched_pos[j]]
                      Infrasp_Tab_Deletion <- (Infrasp_table_final$Deletion)[matched_pos[j]]
                      Infrasp_Tab_Substitution <- (Infrasp_table_final$Substitution)[matched_pos[j]]
                      Matched_table_tmp <- data.frame(Submitted_Name = Infrasp_table_final$Submitted_Name[matched_pos[j]], 
                                                      Order = Infrasp_table_final$Order[matched_pos[j]], 
                                                      Family = Infrasp_table_final$Family[matched_pos[j]], 
                                                      Genus = Infrasp_table_final$Genus[matched_pos[j]], 
                                                      Species = Infrasp_table_final$Species[matched_pos[j]], 
                                                      Infrasp = Infrasp_table_final$Infrasp[matched_pos[j]], 
                                                      Infraspecies = Infrasp_table_final$Infraspecies[matched_pos[j]], 
                                                      Authors = Infrasp_table_final$Authors[matched_pos[j]], 
                                                      Status = Infrasp_table_final$Status[matched_pos[j]], 
                                                      LCVP_Accepted_Taxon = Infrasp_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                                                      PL_Comparison = Infrasp_table_final$PL_Comparison[matched_pos[j]], 
                                                      PL_Alternative = Infrasp_table_final$PL_Alternative[matched_pos[j]], 
                                                      Score = Infrasp_Tab_Score, 
                                                      Insertion = Infrasp_Tab_Insertion, 
                                                      Deletion = Infrasp_Tab_Deletion, 
                                                      Substitution = Infrasp_Tab_Substitution)
                      Matched_table_final <- rbind(Matched_table_final, Matched_table_tmp)
                    }
                  }
                } else if (length(matched_name) > 0 && (auth_name %in% result) == TRUE) {
                  for (j in seq_along(matched_name)) {
                    if (matched_name[j] == auth_name){
                      Infrasp_Tab_genus <- (Infrasp_table_final$Genus)[matched_pos[j]]
                      Infrasp_Tab_species <- (Infrasp_table_final$Species)[matched_pos[j]]
                      Infrasp_Tab_Infrasp <- (Infrasp_table_final$Infrasp)[matched_pos[j]]
                      Infrasp_Tab_Infrasp_name <- (Infrasp_table_final$Infraspecies)[matched_pos[j]]
                      Infrasp_Tab_Author_name <- (Infrasp_table_final$Authors)[matched_pos[j]]
                      Infrasp_Tab_Score <- (Infrasp_table_final$Score)[matched_pos[j]]
                      Infrasp_Tab_Insertion <- (Infrasp_table_final$Insertion)[matched_pos[j]]
                      Infrasp_Tab_Deletion <- (Infrasp_table_final$Deletion)[matched_pos[j]]
                      Infrasp_Tab_Substitution <- (Infrasp_table_final$Substitution)[matched_pos[j]]
                      Matched_table_tmp <- data.frame(Submitted_Name = Infrasp_table_final$Submitted_Name[matched_pos[j]], 
                                                      Order = Infrasp_table_final$Order[matched_pos[j]], 
                                                      Family = Infrasp_table_final$Family[matched_pos[j]], 
                                                      Genus = Infrasp_table_final$Genus[matched_pos[j]], 
                                                      Species = Infrasp_table_final$Species[matched_pos[j]], 
                                                      Infrasp = Infrasp_table_final$Infrasp[matched_pos[j]], 
                                                      Infraspecies = Infrasp_table_final$Infraspecies[matched_pos[j]], 
                                                      Authors = Infrasp_table_final$Authors[matched_pos[j]], 
                                                      Status = Infrasp_table_final$Status[matched_pos[j]], 
                                                      LCVP_Accepted_Taxon = Infrasp_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                                                      PL_Comparison = Infrasp_table_final$PL_Comparison[matched_pos[j]], 
                                                      PL_Alternative = Infrasp_table_final$PL_Alternative[matched_pos[j]], 
                                                      Score = Infrasp_Tab_Score, 
                                                      Insertion = Infrasp_Tab_Insertion, 
                                                      Deletion = Infrasp_Tab_Deletion, 
                                                      Substitution = Infrasp_Tab_Substitution)
                      Matched_table_final <- rbind(Matched_table_final, Matched_table_tmp)
                    }
                  }
                }
                if (is.null(Matched_table_final$Genus[1])) {Matched_table_final <- Infrasp_table_final}
            } else if (!is.null(Infrasp_table_final$Genus[1]) && author == FALSE){
              Matched_table_final <- Infrasp_table_final
# run the fuzzy match search engine for the author when infrasp is not specified ----------------------------------------------------
            } else if (is.null(Infrasp_table_final$Genus[1]) && author == TRUE) {
              col5 <- dim(Species_table_final)[2]
              row5 <- dim(Species_table_final)[1]
              for (k in 1:row5) {
                Spec_Tab_genus <- (Species_table_final$Genus)[k]
                Spec_Tab_species <- (Species_table_final$Species)[k]
                Spec_Tab_Infrasp <- (Species_table_final$Infrasp)[k]
                Spec_Tab_Infrasp_name <- (Species_table_final$Infraspecies)[k]
                Spec_Tab_Author_name <- (Species_table_final$Authors)[k]
                Spec_Tab_Score <- (Species_table_final$Score)[k]
                Spec_Tab_Insertion <- (Species_table_final$Insertion)[k]
                Spec_Tab_Deletion <- (Species_table_final$Deletion)[k]
                Spec_Tab_Substitution <- (Species_table_final$Substitution)[k]
                if (Spec_Tab_Author_name == auth_name) {
                  author_found <- TRUE
                  Matched_table_tmp <- data.frame(Submitted_Name = Species_table_final$Submitted_Name[k], 
                                                  Order = Species_table_final$Order[k], 
                                                  Family = Species_table_final$Family[k], 
                                                  Genus = Species_table_final$Genus[k], 
                                                  Species = Species_table_final$Species[k], 
                                                  Infrasp = Spec_Tab_Infrasp, 
                                                  Infraspecies = Spec_Tab_Infrasp_name, 
                                                  Authors = Spec_Tab_Author_name, 
                                                  Status = Species_table_final$Status[k], 
                                                  LCVP_Accepted_Taxon = Species_table_final$LCVP_Accepted_Taxon[k], 
                                                  PL_Comparison = Species_table_final$PL_Comparison[k], 
                                                  PL_Alternative = Species_table_final$PL_Alternative[k], 
                                                  Score = Spec_Tab_Score, 
                                                  Insertion = Spec_Tab_Insertion, 
                                                  Deletion = Spec_Tab_Deletion, 
                                                  Substitution = Spec_Tab_Substitution)
                  Matched_table_final <- rbind(Matched_table_final, Matched_table_tmp)
                }
              }
              if (is.null(Matched_table_final$Genus[1])) {Matched_table_final <- Species_table_final}
            } else {
              Species_table_final$Score<-as.character(Species_table_final$Score)
              Species_table_final$Score[Species_table_final$Score == 'matched'] <- 'Infrasp. name not found'
              Species_table_final$Score<-as.factor(Species_table_final$Score)
              Matched_table_final <- Species_table_final}
# run the fuzzy match search engine only for the author ----------------------------------------------------
          } else if (is.null(infrasp) && author == TRUE) {    # research option only for author
            matched_name <- agrep(auth_name, Species_table_final$Authors, value = TRUE, max.distance = max.distance)
            matched_pos <- agrep(auth_name, Species_table_final$Authors, value = FALSE, max.distance = max.distance)
            ddf <- abs(nchar(matched_name) - nchar(auth_name))
            if (length(matched_name) > 0) {
              result <- matched_name[ddf == min(ddf)]
              ddf <- abs(nchar(result) - nchar(auth_name))
            }
            if (length(matched_name) > 0 && (auth_name %in% result) == FALSE) {
              for (j in seq_along(matched_name)) {
                if (matched_name[j] %in% result){
                  Species_Tab_genus <- (Species_table_final$Genus)[matched_pos[j]]
                  Species_Tab_species <- (Species_table_final$Species)[matched_pos[j]]
                  Species_Tab_Infrasp <- (Species_table_final$Infrasp)[matched_pos[j]]
                  Species_Tab_Infrasp_name <- (Species_table_final$Infraspecies)[matched_pos[j]]
                  Species_Tab_Author_name <- (Species_table_final$Authors)[matched_pos[j]]
                  Species_Tab_Score <- (Species_table_final$Score)[matched_pos[j]]
                  Species_Tab_Insertion <- (Species_table_final$Insertion)[matched_pos[j]]
                  Species_Tab_Deletion <- (Species_table_final$Deletion)[matched_pos[j]]
                  Species_Tab_Substitution <- (Species_table_final$Substitution)[matched_pos[j]]
                  Matched_table_tmp <- data.frame(Submitted_Name = Species_table_final$Submitted_Name[matched_pos[j]], 
                                                  Order = Species_table_final$Order[matched_pos[j]], 
                                                  Family = Species_table_final$Family[matched_pos[j]], 
                                                  Genus = Species_table_final$Genus[matched_pos[j]], 
                                                  Species = Species_table_final$Species[matched_pos[j]], 
                                                  Infrasp = Species_table_final$Infrasp[matched_pos[j]], 
                                                  Infraspecies = Species_table_final$Infraspecies[matched_pos[j]], 
                                                  Authors = Species_table_final$Authors[matched_pos[j]], 
                                                  Status = Species_table_final$Status[matched_pos[j]], 
                                                  LCVP_Accepted_Taxon = Species_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                                                  PL_Comparison = Species_table_final$PL_Comparison[matched_pos[j]], 
                                                  PL_Alternative = Species_table_final$PL_Alternative[matched_pos[j]], 
                                                  Score = Species_Tab_Score, 
                                                  Insertion = Species_Tab_Insertion, 
                                                  Deletion = Species_Tab_Deletion, 
                                                  Substitution = Species_Tab_Substitution)
                  Matched_table_final <- rbind(Matched_table_final, Matched_table_tmp)
                }
              }
            } else if (length(matched_name) > 0 && (auth_name %in% result) == TRUE) {
              for (j in seq_along(matched_name)) {
                if (matched_name[j] == auth_name){
                  Species_Tab_genus <- (Species_table_final$Genus)[matched_pos[j]]
                  Species_Tab_species <- (Species_table_final$Species)[matched_pos[j]]
                  Species_Tab_Infrasp <- (Species_table_final$Infrasp)[matched_pos[j]]
                  Species_Tab_Infrasp_name <- (Species_table_final$Infraspecies)[matched_pos[j]]
                  Species_Tab_Author_name <- (Species_table_final$Authors)[matched_pos[j]]
                  Species_Tab_Score <- (Species_table_final$Score)[matched_pos[j]]
                  Species_Tab_Insertion <- (Species_table_final$Insertion)[matched_pos[j]]
                  Species_Tab_Deletion <- (Species_table_final$Deletion)[matched_pos[j]]
                  Species_Tab_Substitution <- (Species_table_final$Substitution)[matched_pos[j]]
                  Matched_table_tmp <- data.frame(Submitted_Name = Species_table_final$Submitted_Name[matched_pos[j]], 
                                                  Order = Species_table_final$Order[matched_pos[j]], 
                                                  Family = Species_table_final$Family[matched_pos[j]], 
                                                  Genus = Species_table_final$Genus[matched_pos[j]], 
                                                  Species = Species_table_final$Species[matched_pos[j]], 
                                                  Infrasp = Species_table_final$Infrasp[matched_pos[j]], 
                                                  Infraspecies = Species_table_final$Infraspecies[matched_pos[j]], 
                                                  Authors = Species_table_final$Authors[matched_pos[j]], 
                                                  Status = Species_table_final$Status[matched_pos[j]], 
                                                  LCVP_Accepted_Taxon = Species_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                                                  PL_Comparison = Species_table_final$PL_Comparison[matched_pos[j]], 
                                                  PL_Alternative = Species_table_final$PL_Alternative[matched_pos[j]], 
                                                  Score = Species_Tab_Score, 
                                                  Insertion = Species_Tab_Insertion, 
                                                  Deletion = Species_Tab_Deletion, 
                                                  Substitution = Species_Tab_Substitution)
                  Matched_table_final <- rbind(Matched_table_final, Matched_table_tmp)
                }
              }
            } else {Matched_table_final <- Species_table_final}
# option to search only for the epithet (Genus and epithet) and eventually only for the status 'valid'  ----------------------------------------------------
          } else if (is.null(infrasp) && author == FALSE && infraspecies_tab == FALSE) {
            matched_name <- agrep('species', Species_table_final$Infrasp, 
                                  value = TRUE, max.distance = 0)
            matched_pos <- agrep('species', Species_table_final$Infrasp, 
                                 value = FALSE, max.distance = 0)
            matched_name2 <- agrep('valid', Species_table_final$Status, 
                                   value = TRUE, max.distance = 0)
            if (length(matched_pos) > 0 && status == TRUE && 
                length(matched_name2) > 0) {
              for (j in seq_along(matched_pos)) {
                Spec_Tab_genus <- (Species_table_final$Genus)[matched_pos[j]]
                Spec_Tab_species <- (Species_table_final$Species)[matched_pos[j]]
                Spec_Tab_Infrasp <- (Species_table_final$Infrasp)[matched_pos[j]]
                Spec_Tab_Infrasp_name <- (Species_table_final$Infraspecies)[matched_pos[j]]
                Spec_Tab_Author_name <- (Species_table_final$Authors)[matched_pos[j]]
                Spec_Tab_status <- (Species_table_final$Status)[matched_pos[j]]
                Spec_Tab_Score <- (Species_table_final$Score)[matched_pos[j]]
                Spec_Tab_Insertion <- (Species_table_final$Insertion)[matched_pos[j]]
                Spec_Tab_Deletion <- (Species_table_final$Deletion)[matched_pos[j]]
                Spec_Tab_Substitution <- (Species_table_final$Substitution)[matched_pos[j]]
                if (Spec_Tab_status == 'valid'){
                  Matched_table_tmp <- data.frame(Submitted_Name = Species_table_final$Submitted_Name[matched_pos[j]], 
                                                  Order = Species_table_final$Order[matched_pos[j]], 
                                                  Family = Species_table_final$Family[matched_pos[j]], 
                                                  Genus = Species_table_final$Genus[matched_pos[j]], 
                                                  Species = Species_table_final$Species[matched_pos[j]], 
                                                  Infrasp = Spec_Tab_Infrasp, 
                                                  Infraspecies = Spec_Tab_Infrasp_name, 
                                                  Authors = Spec_Tab_Author_name, 
                                                  Status = Species_table_final$Status[matched_pos[j]], 
                                                  LCVP_Accepted_Taxon = Species_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                                                  PL_Comparison = Species_table_final$PL_Comparison[matched_pos[j]], 
                                                  PL_Alternative = Species_table_final$PL_Alternative[matched_pos[j]], 
                                                  Score = Spec_Tab_Score, 
                                                  Insertion = Spec_Tab_Insertion, 
                                                  Deletion = Spec_Tab_Deletion, 
                                                  Substitution = Spec_Tab_Substitution)
                  Matched_table_final <- rbind(Matched_table_final, 
                                               Matched_table_tmp)
                }
              }
              if (is.null(Matched_table_final$Genus)) {
                for (j in seq_along(matched_pos)) {
                  Spec_Tab_genus <- (Species_table_final$Genus)[matched_pos[j]]
                  Spec_Tab_species <- (Species_table_final$Species)[matched_pos[j]]
                  Spec_Tab_Infrasp <- (Species_table_final$Infrasp)[matched_pos[j]]
                  Spec_Tab_Infrasp_name <- (Species_table_final$Infraspecies)[matched_pos[j]]
                  Spec_Tab_Author_name <- (Species_table_final$Authors)[matched_pos[j]]
                  Spec_Tab_status <- (Species_table_final$Status)[matched_pos[j]]
                  Spec_Tab_Score <- (Species_table_final$Score)[matched_pos[j]]
                  Spec_Tab_Insertion <- (Species_table_final$Insertion)[matched_pos[j]]
                  Spec_Tab_Deletion <- (Species_table_final$Deletion)[matched_pos[j]]
                  Spec_Tab_Substitution <- (Species_table_final$Substitution)[matched_pos[j]]
                  if (Spec_Tab_Infrasp == 'species'){
                    Matched_table_tmp <- data.frame(Submitted_Name = 
                                                    Species_table_final$Submitted_Name[matched_pos[j]], 
                                                    Order = Species_table_final$Order[matched_pos[j]], 
                                                    Family = Species_table_final$Family[matched_pos[j]], 
                                                    Genus = Species_table_final$Genus[matched_pos[j]], 
                                                    Species = Species_table_final$Species[matched_pos[j]], 
                                                    Infrasp = Spec_Tab_Infrasp, 
                                                    Infraspecies = Spec_Tab_Infrasp_name, 
                                                    Authors = Spec_Tab_Author_name, 
                                                    Status = Species_table_final$Status[matched_pos[j]], 
                                                    LCVP_Accepted_Taxon = Species_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                                                    PL_Comparison = Species_table_final$PL_Comparison[matched_pos[j]], 
                                                    PL_Alternative = Species_table_final$PL_Alternative[matched_pos[j]], 
                                                    Score = Spec_Tab_Score, 
                                                    Insertion = Spec_Tab_Insertion, 
                                                    Deletion = Spec_Tab_Deletion, 
                                                    Substitution = Spec_Tab_Substitution)
                    Matched_table_final <- rbind(Matched_table_final, 
                                                 Matched_table_tmp)
                  }
                }
              }
            # option for the cases where there are species only 'synonym', 
            # without any 'valid' status
            } else if (length(matched_pos) > 0 && 
                       status == TRUE && length(matched_name2) == 0){
              for (j in seq_along(matched_pos)) {
                Spec_Tab_genus <- (Species_table_final$Genus)[matched_pos[j]]
                Spec_Tab_species <- (Species_table_final$Species)[matched_pos[j]]
                Spec_Tab_Infrasp <- (Species_table_final$Infrasp)[matched_pos[j]]
                Spec_Tab_Infrasp_name <- (Species_table_final$Infraspecies)[matched_pos[j]]
                Spec_Tab_Author_name <- (Species_table_final$Authors)[matched_pos[j]]
                Spec_Tab_Score <- (Species_table_final$Score)[matched_pos[j]]
                Spec_Tab_Insertion <- (Species_table_final$Insertion)[matched_pos[j]]
                Spec_Tab_Deletion <- (Species_table_final$Deletion)[matched_pos[j]]
                Spec_Tab_Substitution <- (Species_table_final$Substitution)[matched_pos[j]]
                Matched_table_tmp <- data.frame(Submitted_Name = 
                                                Species_table_final$Submitted_Name[matched_pos[j]], 
                                                Order = Species_table_final$Order[matched_pos[j]], 
                                                Family = Species_table_final$Family[matched_pos[j]], 
                                                Genus = Species_table_final$Genus[matched_pos[j]], 
                                                Species = Species_table_final$Species[matched_pos[j]], 
                                                Infrasp = Spec_Tab_Infrasp, 
                                                Infraspecies = Spec_Tab_Infrasp_name, 
                                                Authors = Spec_Tab_Author_name, 
                                                Status = Species_table_final$Status[matched_pos[j]], 
                                                LCVP_Accepted_Taxon = Species_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                                                PL_Comparison = Species_table_final$PL_Comparison[matched_pos[j]], 
                                                PL_Alternative = Species_table_final$PL_Alternative[matched_pos[j]], 
                                                Score = Spec_Tab_Score, 
                                                Insertion = Spec_Tab_Insertion, 
                                                Deletion = Spec_Tab_Deletion, 
                                                Substitution = 
                                                  Spec_Tab_Substitution)
                Matched_table_final <- rbind(Matched_table_final, 
                                             Matched_table_tmp)
              }
            } else if (length(matched_pos) > 0 && status == FALSE) {
              for (j in seq_along(matched_pos)) {
                Spec_Tab_genus <- 
                  (Species_table_final$Genus)[matched_pos[j]]
                Spec_Tab_species <- 
                  (Species_table_final$Species)[matched_pos[j]]
                Spec_Tab_Infrasp <- 
                  (Species_table_final$Infrasp)[matched_pos[j]]
                Spec_Tab_Infrasp_name <- 
                  (Species_table_final$Infraspecies)[matched_pos[j]]
                Spec_Tab_Author_name <- 
                  (Species_table_final$Authors)[matched_pos[j]]
                Spec_Tab_Score <- 
                  (Species_table_final$Score)[matched_pos[j]]
                Spec_Tab_Insertion <- 
                  (Species_table_final$Insertion)[matched_pos[j]]
                Spec_Tab_Deletion <- 
                  (Species_table_final$Deletion)[matched_pos[j]]
                Spec_Tab_Substitution <- 
                  (Species_table_final$Substitution)[matched_pos[j]]
                Matched_table_tmp <- 
                  data.frame(
                    Submitted_Name = 
                      Species_table_final$Submitted_Name[matched_pos[j]], 
                    Order = Species_table_final$Order[matched_pos[j]], 
                    Family = Species_table_final$Family[matched_pos[j]], 
                    Genus = Species_table_final$Genus[matched_pos[j]], 
                    Species = Species_table_final$Species[matched_pos[j]], 
                    Infrasp = Spec_Tab_Infrasp, 
                    Infraspecies = Spec_Tab_Infrasp_name, 
                    Authors = Spec_Tab_Author_name, 
                    Status = Species_table_final$Status[matched_pos[j]], 
                    LCVP_Accepted_Taxon = Species_table_final$LCVP_Accepted_Taxon[matched_pos[j]], 
                    PL_Comparison = Species_table_final$PL_Comparison[matched_pos[j]], 
                    PL_Alternative = Species_table_final$PL_Alternative[matched_pos[j]], 
                    Score = Spec_Tab_Score, 
                    Insertion = Spec_Tab_Insertion, 
                    Deletion = Spec_Tab_Deletion, 
                    Substitution = Spec_Tab_Substitution)
                Matched_table_final <- rbind(Matched_table_final, 
                                             Matched_table_tmp)
              }
            } else {Matched_table_final <- 
              data.frame(Submitted_Name = full_name, 
                         Order = "",
                         Family = "", 
                         Genus = "", 
                         Species = "", 
                         Infrasp = "", 
                         Infraspecies = "", 
                         Authors = "", 
                         Status = "", 
                         LCVP_Accepted_Taxon = "", 
                         PL_Comparison = "", 
                         PL_Alternative = "", 
                         Score = 'Epithet name not found',
                         Insertion = 0, 
                         Deletion = 0, 
                         Substitution = 0)}
          } else {Matched_table_final <- Species_table_final}
        } else {Matched_table_final <- 
          data.frame(Submitted_Name = full_name,
                     Order = "", 
                     Family = "", 
                     Genus = "",
                     Species = "", 
                     Infrasp = "", 
                     Infraspecies = "", 
                     Authors = "", 
                     Status = "", 
                     LCVP_Accepted_Taxon = "", 
                     PL_Comparison = "", 
                     PL_Alternative = "", 
                     Score = 'Epithet name not found', 
                     Insertion = 0, 
                     Deletion = 0, 
                     Substitution = 0)}
        if (visualize == TRUE){
          Matched_table <- Matched_table_final
        }
    } else {Matched_table_final <- Genus_table_final}
  }
  return(Matched_table_final)
}
