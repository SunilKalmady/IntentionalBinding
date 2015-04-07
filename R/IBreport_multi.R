#' @title IBreport_multi
#' @description Generate detailed HTML markdown report from Intentional Binding summary data for multiple subjects.
#' @details IBsummary should be run first and more than one SummaryData.csv files should be available. 
#' Table of combined dataset is also generated.
#' @references \url{www.transpsychlab.org}
#' @author Sunil V Kalmady
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import markdown
#' @import knitr
#' @export 
#' @param fname Filename of unicode tab-delimited text from e-prime experiment output.
#'  @examples
#'\dontrun{
#' # A general example
#' IBsummary("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' IBsummary("Brain2010-IB-0_50_75-H-3-1.edat2.txt")
#' IBreport_multi()
#' 
#' # Try out the example data provided with the package 
#' # copy multiple txt files - A.edat2.txt and H.edat2.txt to working directory (manually or using codes below)
#' srcpath <- system.file("extdata/", package = "IntentionalBinding")
#' flist <- list.files(srcpath, ".txt$", full.names = TRUE)
#' destpath <- getwd()
#' file.copy(flist, destpath)
#' 
#' #Run multisubject Intentional binding report 
#' IBsummary("A.edat2.txt")
#' IBsummary("H.edat2.txt")
#' IBreport_multi()
#'}

IBreport_multi <- function() {
  file_list <- grep('SummaryData.csv$',list.files(),value=TRUE)
  dataset <- do.call("rbind", lapply(file_list, FUN = function(files) {
    read.table(files, header = TRUE, sep = ",")
  }))
  write.csv(file = paste0(length(file_list), "_SummaryData_Combined.csv"), x = dataset, row.names = FALSE)
} 
