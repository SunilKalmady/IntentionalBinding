#' @title IBplot
#' @description Generate time-series plots for all clean trials individually and save into single pdf with multiple pages. 
#' @details IBsummary should be run first and SummaryData.csv should be available. 
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
#' IBplot("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' 
#' # Try out the example data provided with the package 
#' # copy A.edat2.txt or H.edat2.txt to working directory (manually or using codes below)
#' srcpath <- system.file("extdata/A.edat2.txt", package = "IntentionalBinding")
#' destpath <- getwd()
#' file.copy(srcpath, destpath)
#' 
#' #Run Intentional binding time-series plots
#' IBsummary("A.edat2.txt")
#' IBplot("A.edat2.txt")
#'}


IBplot <- function(fname) {
  # for all clean trials individually and save into single pdf with multiple pages
  
  sumdata <- read.table(paste0(fname, "_SummaryData.csv"), sep = ",", header = TRUE)
  clockdata <- read.table(paste0(fname, "_ClockData.csv"), sep = ",", header = TRUE)
  JudgeTimemat <- read.table(paste0(fname, "_JudgeTime_Matrix.csv"), sep = ",", header = FALSE, na.strings = c("NA", " NA", "NULL"), fill = TRUE)
  
  fig <- list()
  k <- 0
  for (n in subset(sumdata, CleanTrial == TRUE)$Trial) {
    k <- k + 1
    fig[[k]] <- ggplot((subset(clockdata, Trial == n)), aes(Time, HandPos))
    fig[[k]] <- fig[[k]] + geom_line(size = 1)
    fig[[k]] <- fig[[k]] + geom_point(size = 3, pch = 21, col = "black", aes(fill = as.factor(Counter)))
    fig[[k]] <- fig[[k]] + geom_vline(xintercept = c((subset(sumdata, Trial == n)$KeyStart), (subset(sumdata, Trial == n)$KeyEnd)), colour = "#e41a1c", linetype = "longdash")
    fig[[k]] <- fig[[k]] + annotate("text", x = (subset(sumdata, Trial == n)$KeyStart) - 10, y = 60, label = "Key Press", color = "#e41a1c", angle = 90, hjust = 1, vjust = 0, size = 4)
    if (subset(sumdata, Trial == n)$TonePresence) {
      fig[[k]] <- fig[[k]] + geom_vline(xintercept = c((subset(sumdata, Trial == n)$ToneStartmean), ((subset(sumdata, Trial == n)$ToneStartmean) + 100)), colour = "#4daf4a", linetype = "longdash")
      fig[[k]] <- fig[[k]] + annotate("text", x = (subset(sumdata, Trial == n)$ToneStartmean) - 10, y = 60, label = "Tone", color = "#4daf4a", angle = 90, hjust = 1, vjust = 0, size = 4)
    } else {
      fig[[k]] <- fig[[k]] + annotate("text", x = (subset(sumdata, Trial == n)$ToneStartmean) - 10, y = 60, label = "No Tone", color = "#4daf4a", angle = 90, hjust = 1, vjust = 0, size = 4)
    }
    fig[[k]] <- fig[[k]] + geom_hline(yintercept = sumdata$JudgeHandPos[n], colour = "#377eb8", linetype = "longdash")
    fig[[k]] <- fig[[k]] + annotate("text", x = (sumdata$TrialBegin[n] + 100), y = (sumdata$JudgeHandPos[n] + 0.5), label = "Judgement", color = "#377eb8", hjust = 0, vjust = 0, size = 4)
    if (!(all(is.na(JudgeTimemat[n, ])))) {
      fig[[k]] <- fig[[k]] + geom_vline(xintercept = JudgeTimemat[n, ][!is.na(JudgeTimemat[n, ])], colour = "#377eb8", linetype = "longdash")
    }
    fig[[k]] <- fig[[k]] + annotate("text", x = (subset(sumdata, Trial == n)$JudgeTime) - 10, y = 60, label = "Judgement Time", color = "#377eb8", angle = 90, hjust = 1, vjust = 0, size = 4)
    fig[[k]] <- fig[[k]] + geom_segment(x = (subset(sumdata, Trial == n)$KeyStart), y = 60, xend = (subset(sumdata, Trial == n)$JudgeTime), yend = 60, arrow = arrow(length = unit(0.2, "cm")))
    trial_details <- c()
    trial_details[1] <- paste0("Block Name/Trial No = ", subset(sumdata, Trial == n)$Block, "/", n)
    trial_details[2] <- paste0("Judgement error = ", round(subset(sumdata, Trial == n)$JudgeError, 2), " msec")
    trial_details[3] <- paste0("Average Step duration = ", round(subset(sumdata, Trial == n)$TimeAvgStep, 2), " msec")
    trial_details[4] <- paste0("Key Press duration = ", subset(sumdata, Trial == n)$KeyDuration, " msec")
    trial_details[5] <- paste0("Key Press Precision QC = ", subset(sumdata, Trial == n)$KeyRTdiffQC, " msec")
    if (subset(sumdata, Trial == n)$TonePresence) {
      trial_details[6] <- paste0("Tone duration Precision QC = ", subset(sumdata, Trial == n)$ToneDurationQC, " msec")
    } else {
      trial_details[6] <- paste0("No Audio Tone Trial")
    }
    trial_details[7] <- paste0("Subject Confidence = ", subset(sumdata, Trial == n)$Confidence)
    fig[[k]] <- fig[[k]] + ggtitle(paste0(trial_details[1], "\n", trial_details[2], "\n", trial_details[3], "\n", trial_details[4], "\n", trial_details[5], "\n", trial_details[6], "\n", trial_details[7], 
                                          "\n"))
    fig[[k]] <- fig[[k]] + xlab("Time [millisecs]") + ylab("Clock Hand Position")
    fig[[k]] <- fig[[k]] + scale_x_continuous(breaks = (subset(clockdata, Trial == n)$Time)) + scale_y_continuous(breaks = seq(0, 57.5, 2.5))
    fig[[k]] <- fig[[k]] + theme(plot.title = element_text(size = 12, face = "bold"), legend.position = "none", axis.title = element_text(face = "bold", size = 12), axis.text = element_text(size = 10), 
                                 axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  pdf(paste0(fname, "_Trials-", length(fig), "_plots.pdf"), onefile = TRUE, width = 16, height = 6.5)
  for (i in seq(length(fig))) {
    grid.arrange(fig[[i]])
  }
  dev.off()
}
