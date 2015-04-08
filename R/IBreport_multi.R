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
#' @param flist List of SummaryData filenames to be combined. 
#' @param all A boolean that indicates whether to all combine all SummaryData csv in the current directory (FALSE by Default) 
#'  @examples
#'\dontrun{
#' # A general example
#' IBsummary("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' IBsummary("Brain2010-IB-0_50_75-H-3-1.edat2.txt")
#' IBreport_multi(all = TRUE)
#' # OR
#' IBreport_multi(flist = c("Brain2010-IB-0_50_75-A-3-1.edat2.txt","Brain2010-IB-0_50_75-H-3-1.edat2.txt"))
#' 
#' # Try out the example data provided with the package 
#' # copy multiple txt files - A.edat2.txt and H.edat2.txt to working directory (manually or using codes below)
#' srcpath <- system.file("extdata/", package = "IntentionalBinding")
#' flst <- list.files(srcpath, ".txt$", full.names = TRUE)
#' destpath <- getwd()
#' file.copy(flst, destpath)
#' 
#' #Run multisubject Intentional binding report 
#' IBsummary("A.edat2.txt")
#' IBsummary("H.edat2.txt")
#' IBreport_multi(all = TRUE)
#' # OR
#' IBreport_multi(flist = c("Brain2010-IB-0_50_75-A-3-1.edat2.txt","Brain2010-IB-0_50_75-H-3-1.edat2.txt"))
#'}

IBreport_multi <- function(all=FALSE, flist = NULL) {
  if (all){
    file_list <- grep('SummaryData.csv$',list.files(),value=TRUE)
  } else {
    file_list <- flist
    }
  dataset <- do.call("rbind", lapply(file_list, FUN = function(files) {
    read.table(files, header = TRUE, sep = ",")
  }))
  write.csv(file = paste0(length(file_list), "files_SummaryData_Combined.csv"), x = dataset, row.names = FALSE)
#call aggregate
#call report
}
  


