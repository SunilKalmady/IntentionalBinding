#' @title IBresulttable
#' @description Generate result tables with summary statistics for Intentional Binding summary data. 
#' @details IBsummary should be run first and SummaryData.csv should be available. 
#' Two tables are generated with mean, N, SD, CV for each experimental condition. One with all the clean trials, another after removal of outliers. 
#' Predictive and retrospective components of intentional binding are also generated.
#' @references \url{www.transpsychlab.org}
#' @author Sunil V Kalmady
#' @export 
#' @param fname Filename of unicode tab-delimited text from e-prime experiment output.
#' @examples
#'\dontrun{
#' # A general example
#' IBsummary("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' IBresulttable("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' 
#' # Try out the example data provided with the package 
#' # copy A.edat2.txt or H.edat2.txt to working directory (manually or using codes below)
#' srcpath <- system.file("extdata/A.edat2.txt", package = "IntentionalBinding")
#' destpath <- getwd()
#' file.copy(srcpath, destpath)
#' 
#' #Run Intentional binding results table
#' IBsummary("A.edat2.txt")
#' IBresulttable("A.edat2.txt")
#'}

IBresulttable <- function(fname) {
  
  sumdata <- read.table(paste0(fname, "_SummaryData.csv"), sep = ",", header = TRUE)

  restable <- data.frame(Condition = c("Baseline", rep("50% effect probability", 2), rep("75% effect probability", 2)), Trial_Type = c("Action Only", rep(c("Action Only", "Action and Tone"), 2)))
  Mean_Judgement_0 <- round(mean(subset(sumdata, Block == "Tone0Trials")$JudgeError, na.rm = TRUE), 2)
  N_Judgement_0 <- sum(!is.na(subset(sumdata, Block == "Tone0Trials")$JudgeError))
  SD_Judgement_0 <- round(sd(subset(sumdata, Block == "Tone0Trials")$JudgeError, na.rm = TRUE), 2)
  Mean_Judgement_50A <- round(mean(subset(sumdata, Block == "Tone50Trials" & TonePresence == "FALSE")$JudgeError, na.rm = TRUE), 2)
  N_Judgement_50A <- sum(!is.na(subset(sumdata, Block == "Tone50Trials" & TonePresence == "FALSE")$JudgeError))
  SD_Judgement_50A <- round(sd(subset(sumdata, Block == "Tone50Trials" & TonePresence == "FALSE")$JudgeError, na.rm = TRUE), 2)
  Mean_Judgement_50AT <- round(mean(subset(sumdata, Block == "Tone50Trials" & TonePresence == "TRUE")$JudgeError, na.rm = TRUE), 2)
  N_Judgement_50AT <- sum(!is.na(subset(sumdata, Block == "Tone50Trials" & TonePresence == "TRUE")$JudgeError))
  SD_Judgement_50AT <- round(sd(subset(sumdata, Block == "Tone50Trials" & TonePresence == "TRUE")$JudgeError, na.rm = TRUE), 2)
  Mean_Judgement_75A <- round(mean(subset(sumdata, Block == "Tone75Trials" & TonePresence == "FALSE")$JudgeError, na.rm = TRUE), 2)
  N_Judgement_75A <- sum(!is.na(subset(sumdata, Block == "Tone75Trials" & TonePresence == "FALSE")$JudgeError))
  SD_Judgement_75A <- round(sd(subset(sumdata, Block == "Tone75Trials" & TonePresence == "FALSE")$JudgeError, na.rm = TRUE), 2)
  Mean_Judgement_75AT <- round(mean(subset(sumdata, Block == "Tone75Trials" & TonePresence == "TRUE")$JudgeError, na.rm = TRUE), 2)
  N_Judgement_75AT <- sum(!is.na(subset(sumdata, Block == "Tone75Trials" & TonePresence == "TRUE")$JudgeError))
  SD_Judgement_75AT <- round(sd(subset(sumdata, Block == "Tone75Trials" & TonePresence == "TRUE")$JudgeError, na.rm = TRUE), 2)
  Mean_Judgement_50A_BL <- round(mean(subset(sumdata, Block == "Tone50Trials" & TonePresence == "FALSE")$JudgeErrorBL, na.rm = TRUE), 2)
  SD_Judgement_50A_BL <- round(sd(subset(sumdata, Block == "Tone50Trials" & TonePresence == "FALSE")$JudgeErrorBL, na.rm = TRUE), 2)
  Mean_Judgement_50AT_BL <- round(mean(subset(sumdata, Block == "Tone50Trials" & TonePresence == "TRUE")$JudgeErrorBL, na.rm = TRUE), 2)
  SD_Judgement_50AT_BL <- round(sd(subset(sumdata, Block == "Tone50Trials" & TonePresence == "TRUE")$JudgeErrorBL, na.rm = TRUE), 2)
  Mean_Judgement_75A_BL <- round(mean(subset(sumdata, Block == "Tone75Trials" & TonePresence == "FALSE")$JudgeErrorBL, na.rm = TRUE), 2)
  SD_Judgement_75A_BL <- round(sd(subset(sumdata, Block == "Tone75Trials" & TonePresence == "FALSE")$JudgeErrorBL, na.rm = TRUE), 2)
  Mean_Judgement_75AT_BL <- round(mean(subset(sumdata, Block == "Tone75Trials" & TonePresence == "TRUE")$JudgeErrorBL, na.rm = TRUE), 2)
  SD_Judgement_75AT_BL <- round(sd(subset(sumdata, Block == "Tone75Trials" & TonePresence == "TRUE")$JudgeErrorBL, na.rm = TRUE), 2)
  CV_Judgement_50A_BL <- SD_Judgement_50A_BL/Mean_Judgement_50A_BL
  CV_Judgement_50AT_BL <- SD_Judgement_50AT_BL/Mean_Judgement_50AT_BL
  CV_Judgement_75A_BL <- SD_Judgement_75A_BL/Mean_Judgement_75A_BL
  CV_Judgement_75AT_BL <- SD_Judgement_75AT_BL/Mean_Judgement_75AT_BL
  Pred_Con <- Mean_Judgement_75A_BL - Mean_Judgement_50A_BL
  Retro_Con <- Mean_Judgement_50AT_BL - Mean_Judgement_50A_BL
  
  restable$N_Judgement <- c(N_Judgement_0, N_Judgement_50A, N_Judgement_50AT, N_Judgement_75A, N_Judgement_75AT)
  restable$Mean_Judgement <- c(Mean_Judgement_0, Mean_Judgement_50A, Mean_Judgement_50AT, Mean_Judgement_75A, Mean_Judgement_75AT)
  restable$SD_Judgement <- c(SD_Judgement_0, SD_Judgement_50A, SD_Judgement_50AT, SD_Judgement_75A, SD_Judgement_75AT)
  restable$Mean_Judgement_BL <- c("", Mean_Judgement_50A_BL, Mean_Judgement_50AT_BL, Mean_Judgement_75A_BL, Mean_Judgement_75AT_BL)
  restable$SD_Judgement_BL <- c("", SD_Judgement_50A_BL, SD_Judgement_50AT_BL, SD_Judgement_75A_BL, SD_Judgement_75AT_BL)
  restable$CV_Judgement_BL <- c("", CV_Judgement_50A_BL, CV_Judgement_50AT_BL, CV_Judgement_75A_BL, CV_Judgement_75AT_BL)
  restable$Predictive_contribution <- c("", "", "", "", Pred_Con)
  restable$Retrospective_contribution <- c("", "", "", "", Retro_Con)
  
  write.csv(file = paste0(fname, "_Trials-", max(sumdata$Trial), "_Result_Table.csv"), x = restable, row.names = FALSE)
  
  restableOut <- data.frame(Condition = c("Baseline", rep("50% effect probability", 2), rep("75% effect probability", 2)), Trial_Type = c("Action Only", rep(c("Action Only", "Action and Tone"), 2)))
  Mean_Judgement_0 <- round(mean(subset(sumdata, Block == "Tone0Trials")$JudgeErrorOut, na.rm = TRUE), 2)
  N_Judgement_0 <- sum(!is.na(subset(sumdata, Block == "Tone0Trials")$JudgeErrorOut))
  SD_Judgement_0 <- round(sd(subset(sumdata, Block == "Tone0Trials")$JudgeErrorOut, na.rm = TRUE), 2)
  Mean_Judgement_50A <- round(mean(subset(sumdata, Block == "Tone50Trials" & TonePresence == "FALSE")$JudgeErrorOut, na.rm = TRUE), 2)
  N_Judgement_50A <- sum(!is.na(subset(sumdata, Block == "Tone50Trials" & TonePresence == "FALSE")$JudgeErrorOut))
  SD_Judgement_50A <- round(sd(subset(sumdata, Block == "Tone50Trials" & TonePresence == "FALSE")$JudgeErrorOut, na.rm = TRUE), 2)
  Mean_Judgement_50AT <- round(mean(subset(sumdata, Block == "Tone50Trials" & TonePresence == "TRUE")$JudgeErrorOut, na.rm = TRUE), 2)
  N_Judgement_50AT <- sum(!is.na(subset(sumdata, Block == "Tone50Trials" & TonePresence == "TRUE")$JudgeErrorOut))
  SD_Judgement_50AT <- round(sd(subset(sumdata, Block == "Tone50Trials" & TonePresence == "TRUE")$JudgeErrorOut, na.rm = TRUE), 2)
  Mean_Judgement_75A <- round(mean(subset(sumdata, Block == "Tone75Trials" & TonePresence == "FALSE")$JudgeErrorOut, na.rm = TRUE), 2)
  N_Judgement_75A <- sum(!is.na(subset(sumdata, Block == "Tone75Trials" & TonePresence == "FALSE")$JudgeErrorOut))
  SD_Judgement_75A <- round(sd(subset(sumdata, Block == "Tone75Trials" & TonePresence == "FALSE")$JudgeErrorOut, na.rm = TRUE), 2)
  Mean_Judgement_75AT <- round(mean(subset(sumdata, Block == "Tone75Trials" & TonePresence == "TRUE")$JudgeErrorOut, na.rm = TRUE), 2)
  N_Judgement_75AT <- sum(!is.na(subset(sumdata, Block == "Tone75Trials" & TonePresence == "TRUE")$JudgeErrorOut))
  SD_Judgement_75AT <- round(sd(subset(sumdata, Block == "Tone75Trials" & TonePresence == "TRUE")$JudgeErrorOut, na.rm = TRUE), 2)
  Mean_Judgement_50A_BL <- round(mean(subset(sumdata, Block == "Tone50Trials" & TonePresence == "FALSE")$JudgeErrorOutBL, na.rm = TRUE), 2)
  SD_Judgement_50A_BL <- round(sd(subset(sumdata, Block == "Tone50Trials" & TonePresence == "FALSE")$JudgeErrorOutBL, na.rm = TRUE), 2)
  Mean_Judgement_50AT_BL <- round(mean(subset(sumdata, Block == "Tone50Trials" & TonePresence == "TRUE")$JudgeErrorOutBL, na.rm = TRUE), 2)
  SD_Judgement_50AT_BL <- round(sd(subset(sumdata, Block == "Tone50Trials" & TonePresence == "TRUE")$JudgeErrorOutBL, na.rm = TRUE), 2)
  Mean_Judgement_75A_BL <- round(mean(subset(sumdata, Block == "Tone75Trials" & TonePresence == "FALSE")$JudgeErrorOutBL, na.rm = TRUE), 2)
  SD_Judgement_75A_BL <- round(sd(subset(sumdata, Block == "Tone75Trials" & TonePresence == "FALSE")$JudgeErrorOutBL, na.rm = TRUE), 2)
  Mean_Judgement_75AT_BL <- round(mean(subset(sumdata, Block == "Tone75Trials" & TonePresence == "TRUE")$JudgeErrorOutBL, na.rm = TRUE), 2)
  SD_Judgement_75AT_BL <- round(sd(subset(sumdata, Block == "Tone75Trials" & TonePresence == "TRUE")$JudgeErrorOutBL, na.rm = TRUE), 2)
  Pred_Con <- Mean_Judgement_75A_BL - Mean_Judgement_50A_BL
  Retro_Con <- Mean_Judgement_50AT_BL - Mean_Judgement_50A_BL
  
  restableOut$N_Judgement <- c(N_Judgement_0, N_Judgement_50A, N_Judgement_50AT, N_Judgement_75A, N_Judgement_75AT)
  restableOut$Mean_Judgement <- c(Mean_Judgement_0, Mean_Judgement_50A, Mean_Judgement_50AT, Mean_Judgement_75A, Mean_Judgement_75AT)
  restableOut$SD_Judgement <- c(SD_Judgement_0, SD_Judgement_50A, SD_Judgement_50AT, SD_Judgement_75A, SD_Judgement_75AT)
  restableOut$Mean_Judgement_BL <- c("", Mean_Judgement_50A_BL, Mean_Judgement_50AT_BL, Mean_Judgement_75A_BL, Mean_Judgement_75AT_BL)
  restableOut$SD_Judgement_BL <- c("", SD_Judgement_50A_BL, SD_Judgement_50AT_BL, SD_Judgement_75A_BL, SD_Judgement_75AT_BL)
  restableOut$Predictive_contribution <- c("", "", "", "", Pred_Con)
  restableOut$Retrospective_contribution <- c("", "", "", "", Retro_Con)
  
  write.csv(file = paste0(fname, "_Trials-", max(sumdata$Trial), "_Result_Table_outliers_removed.csv"), x = restableOut, row.names = FALSE)
}
