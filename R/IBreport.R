#' @title IBreport
#' @description Generate detailed markdown HTML report from Intentional Binding summary data for single subject.
#' @details IBsummary should be run first and SummaryData.csv should be available. 
#' @references \url{www.transpsychlab.org}
#' @author Sunil V Kalmady
#' @import ggplot2
#' @import markdown
#' @import knitr
#' @export 
#' @param fname Filename of unicode tab-delimited text from e-prime experiment output.
#' @examples
#'\dontrun{
#' # A general example
#' IBsummary("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' IBreport("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' 
#' # Try out the example data provided with the package 
#' # copy A.edat2.txt or H.edat2.txt to working directory (manually or using codes below)
#' srcpath <- system.file("extdata/A.edat2.txt", package = "IntentionalBinding")
#' destpath <- getwd()
#' file.copy(srcpath, destpath)
#' 
#' #Run Intentional binding report for single subject
#' IBsummary("A.edat2.txt")
#' IBreport("A.edat2.txt")
#'}

IBreport <- function(fname) {
  sumdata <- read.table(paste0(fname, "_SummaryData.csv"), sep = ",", header = TRUE)
  #rmarkdown::render(input = "IBreport.R", clean = FALSE, quiet = TRUE, output_file = paste0(fname, "_IBreport_SingleSubject.html"))
  # o = spin('IBreport.R') 
  # knit2html(o,quiet = TRUE, output = paste0(fname,'_IBreport_SingleSubject.html')) 
  #rmarkdown::render('IBreport_SingleSubject.Rmd', 'html_document', quiet = TRUE, output_file = paste0(fname,'_IBreport_SingleSubject.html'))
  rmd_path = system.file("extdata/IBreport_SingleSubject.Rmd", package = "IntentionalBinding")
  out_path = paste0(getwd())
  rmarkdown::render(input = rmd_path, output_format = 'html_document', quiet = TRUE, output_dir = out_path, output_file = paste0(fname,'_IBreport_SingleSubject.html'))
}
