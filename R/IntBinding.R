#' @title IntBinding
#' @description Complete data analysis and reporting for e-prime data of Intentional Binding cognitive paradigm.
#' @details 
#' This master function runs all the required functions within 'IntentionalBinding' package required for complete processing pipeline for single subject e-prime data.
#' It first creates summary data for each trial using IBsummary including baseline corrected judgement errors. 
#' Then, outputs descriptives per experimental condition and computes the predictive and retrospective binding metrics using IBresulttable. 
#' Trial wise time series plots with detailed supplementary information are generated using IBplot. 
#' Plots of step periods are produced to detect any timing issues during administration of experiment using IBstepplot. 
#' Finally, Rmarkdown HTML report is generated with basic statistical tests and pretty plots using IBreport.
#' @references \url{www.transpsychlab.org}
#' @aliases IntBinding
#' @author Sunil V Kalmady
#' @import MASS
#' @import plyr
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import markdown
#' @import knitr
#' @export IntBinding
#' @param fname Filename of unicode tab-delimited text from e-prime experiment output.
#' @examples
#'\dontrun{
#' # A general example
#' IntBinding("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' 
#' # Try out the example data provided with the package 
#' # copy A.edat2.txt or H.edat2.txt to working directory (manually or using codes below)
#' srcpath <- system.file("extdata/A.edat2.txt", package = "IntentionalBinding")
#' destpath <- getwd()
#' file.copy(srcpath, destpath)
#' 
#' #Run complete Intentional binding processing pipleine with one command
#' IntBinding("A.edat2.txt")
#'}

IntBinding <- function(fname) {
    IBsummary(fname)
    IBresult(fname)
    IBresulttable(fname)
    IBstepplot(fname)
    IBplot(fname)
    IBresultplot(fname)
    IBreport(fname)
}