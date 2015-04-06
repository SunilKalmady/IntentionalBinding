#' @title IBresultplot
#' @description Generate box-plots for results from Intentional Binding summary data.
#' @details IBsummary should be run first and SummaryData.csv should be available. 
#' Box Plots are output as two pdf files. One with all the clean trials, another after removal of outliers. 
#' @references \url{www.transpsychlab.org}
#' @author Sunil V Kalmady
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @export 
#' @param fname Filename of unicode tab-delimited text from e-prime experiment output.
#' @examples
#'\dontrun{
#' # A general example
#' IBsummary("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' IBresultplot("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' 
#' # Try out the example data provided with the package 
#' # copy A.edat2.txt or H.edat2.txt to working directory (manually or using codes below)
#' srcpath <- system.file("extdata/A.edat2.txt", package = "IntentionalBinding")
#' destpath <- getwd()
#' file.copy(srcpath, destpath)
#' 
#' #Run Intentional binding condition wise box-plots
#' IBsummary("A.edat2.txt")
#' IBresultplot("A.edat2.txt")
#'}


IBresultplot <- function(fname) {
  
  sumdata <- read.table(paste0(fname, "_SummaryData.csv"), sep = ",", header = TRUE)
  
  ## results plot
  rfig <- ggplot(data = subset(sumdata, Block != "Tone0Trials"), aes(x = TonePresence, y = JudgeErrorBL, fill = Block)) + stat_boxplot(geom = "errorbar") + geom_boxplot(outlier.size = 0) + xlab("Presence of Tone") + 
    ylab("Mean Shift from Baseline [ms]") + geom_point(position = position_jitter(w = 0.03, h = 0), pch = 21, col = "black", cex = 2, alpha = 0.8, aes(colour = Block)) + scale_fill_brewer(palette = "Set1", 
                                                                                                                                                                                            name = "Experimental Condition", breaks = c("Tone50Trials", "Tone75Trials"), labels = c("50% Tone probability", "75% Tone probability")) + theme(axis.title = element_text(face = "bold", size = 12), 
                                                                                                                                                                                                                                                                                                                                             axis.text = element_text(face = "bold", size = 10), axis.ticks = element_blank())
  ggsave(filename = paste0(fname, "_Trials_results.pdf"), plot = rfig, width = 8, height = 6)
  
  ## results plot - outliers removed
  rfigOut <- ggplot(data = subset(sumdata, Block != "Tone0Trials"), aes(x = TonePresence, y = JudgeErrorOutBL, fill = Block)) + stat_boxplot(geom = "errorbar") + geom_boxplot(outlier.size = 0) + xlab("Presence of Tone") + 
    ylab("Mean Shift from Baseline [ms]") + geom_point(position = position_jitter(w = 0.03, h = 0), pch = 21, col = "black", cex = 2, alpha = 0.8, aes(colour = Block)) + scale_fill_brewer(palette = "Set1", 
                                                                                                                                                                                            name = "Experimental Condition", breaks = c("Tone50Trials", "Tone75Trials"), labels = c("50% Tone probability", "75% Tone probability")) + theme(axis.title = element_text(face = "bold", size = 12), 
                                                                                                                                                                                                                                                                                                                                             axis.text = element_text(face = "bold", size = 10), axis.ticks = element_blank())
  ggsave(filename = paste0(fname, "_Trials_results_outliers_removed.pdf"), plot = rfigOut, width = 8, height = 6)
}