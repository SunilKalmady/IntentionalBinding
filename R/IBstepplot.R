#' @title IBstepplot
#' @description Generate plots of step periods for each trial in Intentional Binding summary data and save into single pdf with multiple pages.
#' @details IBsummary should be run first and SummaryData.csv should be available.
#' This will be helpful in detecting any timing issues during administration of experiment.
#' Plots are annotated with critical time points such as key press.
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
#' IBstepplot("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' 
#' # Try out the example data provided with the package 
#' # copy A.edat2.txt or H.edat2.txt to working directory (manually or using codes below)
#' srcpath <- system.file("extdata/A.edat2.txt", package = "IntentionalBinding")
#' destpath <- getwd()
#' file.copy(srcpath, destpath)
#' 
#' #Run Intentional binding step duration plots
#' IBsummary("A.edat2.txt")
#' IBstepplot("A.edat2.txt")
#'}

IBstepplot <- function(fname) {
  
  sumdata <- read.table(paste0(fname, "_SummaryData.csv"), sep = ",", header = TRUE)
  clockdata <- read.table(paste0(fname, "_ClockData.csv"), sep = ",", header = TRUE)
  
  stepfig <- list()
  for (n in 1:max(clockdata$Trial)) {
    stepfig[[n]] <- ggplot(data = subset(clockdata, Trial == n), aes(x = Time, y = StepPeriod))
    stepfig[[n]] <- stepfig[[n]] + geom_line()
    stepfig[[n]] <- stepfig[[n]] + geom_point(size = 3, pch = 21, col = "black", aes(fill = as.factor(Counter)))
    stepfig[[n]] <- stepfig[[n]] + geom_vline(xintercept = c((subset(sumdata, Trial == n)$KeyStart), (subset(sumdata, Trial == n)$KeyEnd)), colour = "#e41a1c", linetype = "longdash")
    stepfig[[n]] <- stepfig[[n]] + geom_hline(yintercept = c(93.3, 106.7, 120), colour = "black", linetype = "longdash")
    stepfig[[n]] <- stepfig[[n]] + geom_vline(xintercept = subset(sumdata, Trial == n)$AudioEnd, colour = "#4daf4a", linetype = "longdash")
    n107 <- nrow(subset(subset(clockdata, Trial == n), StepPeriod >= 105 & StepPeriod <= 109))
    n93 <- nrow(subset(subset(clockdata, Trial == n), StepPeriod >= 91 & StepPeriod <= 95))
    n120 <- nrow(subset(subset(clockdata, Trial == n), StepPeriod >= 118 & StepPeriod <= 122))
    ntot <- subset(sumdata, Trial == n)$TotSteps - 1
    nother <- ntot - (n107 + n93 + n120)
    step_details <- c()
    step_details[1] <- paste0("Average Step period = ", round(subset(sumdata, Trial == n)$TimeAvgStep, 2), " msec")
    step_details[2] <- paste0("N, percentage of 93.3 ms period = ", n93, ", ", round(n93 * 100/ntot, 2), "%")
    step_details[3] <- paste0("N, percentage of 106.7 ms period = ", n107, ", ", round(n107 * 100/ntot, 2), "%")
    step_details[4] <- paste0("N, percentage of 120 ms period = ", n120, ", ", round(n120 * 100/ntot, 2), "%")
    stepfig[[n]] <- stepfig[[n]] + ggtitle(paste0(step_details[1], "\n", step_details[2], "\n", step_details[3], "\n", step_details[4], "\n"))
    stepfig[[n]] <- stepfig[[n]] + xlab("Time [millisecs]") + ylab("Next Step Period")
    stepfig[[n]] <- stepfig[[n]] + scale_x_continuous(breaks = (subset(clockdata, Trial == n)$Time)) + scale_y_continuous(breaks = seq(90, 130, 2.5))
    stepfig[[n]] <- stepfig[[n]] + theme(plot.title = element_text(size = 12, face = "bold"), legend.position = "none", axis.title = element_text(face = "bold", size = 12), axis.text = element_text(size = 10), 
                                         axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  pdf(paste0(fname, "_Trials-", length(stepfig), "_Step_Periods.pdf"), onefile = TRUE, width = 16, height = 6.5)
  for (i in seq(length(stepfig))) {
    grid.arrange(stepfig[[i]])
  }
  dev.off()
}
