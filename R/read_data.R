
#' Standardize plant names according to the Leipzig Catalogue of Vascular Plants (LCVP)
#' 
#' Allow a taxonomic resolution of plant taxa names listed in the "Leipzig
#' Catalogue of Vascular Plants" (LCVP)
#' 
#' 
#' @param pathstring A character vector, which allow the user to define the
#' path where the input text file where is saved the input list of taxa is
#' stored.
#' @param encoding character vector, "UTF-8" (default). This value will allow
#' the user to set the specific codification of the strings.
#' @author Alessandro Gentile, Marten Winter, Martin Freiberg
#' @seealso https://idata.idiv.de/ddm/Data/ShowData/1806
#' @references The Leipzig Catalogue of Vascular Plants (LCVP) - An improved taxonomic
#' reference list for all known vascular plants
#' @examples
#' 
#' \dontrun{read_data(file.choose(), 'UTF-8')}
#' @importFrom  utils read.csv
#' @export read_data
read_data <-
  function(pathstring, encoding) {
    search_table.sp <- try(read.csv(pathstring,
                                    header = TRUE,
                                    sep = "\t",
                                    fill = TRUE,
                                    colClasses = "character",
                                    as.is = TRUE,
                                    stringsAsFactors = FALSE
    ), silent = TRUE)
    
    N_rows <- dim(search_table.sp)[1]
    lista <- NULL
    for (i in 1:N_rows){
      row <- (search_table.sp[i,1])
      lista <- c(lista, row)
    }
    return(lista)
  }