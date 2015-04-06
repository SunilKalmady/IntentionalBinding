#' @title IBsummary
#' @description Generate critical metrics and trial wise summary for raw eprime data of Intentional Binding paradigm.
#' @details Generates SummaryData.csv which is critical for other functions in package which carryout downstream data processing. 
#' First, the function strips off trials without keypress, subject judgement or audio failure. 
#' Then, prepares table with clock positions and their timings in order to reconstruct every trial.
#' Timing corresponding to judged hand position is obtained. This is done by linear interpolation of timing corresponding to judged hand position for six rotations and then selecting the timepoint closest to key press.
#' Additional Columns with the first trial and outliers removed [based on tukey's criterion] as well as average clock step period per trial are appended.
#' These values along with other critical parameters obtained from raw eprime data is output as single CSV table.
#' @references \url{www.transpsychlab.org}
#' @author Sunil V Kalmady
#' @export 
#' @param fname Filename of unicode tab-delimited text from e-prime experiment output.
#' @examples
#'\dontrun{
#' # A general example
#' IBsummary("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' 
#' # Try out the example data provided with the package 
#' # copy A.edat2.txt or H.edat2.txt to working directory (manually or using codes below)
#' srcpath <- system.file("extdata/A.edat2.txt", package = "IntentionalBinding")
#' destpath <- getwd()
#' file.copy(srcpath, destpath)
#' 
#' #Run Intentional binding summary function
#' IBsummary("A.edat2.txt")
#'}

IBsummary <- function(fname) {
  data <- read.table(fname, sep = "\t", header = TRUE, fileEncoding = "UCS-2LE")
  
  ## Strip off trials without keypress, subject judgement or audio failure
  cleandata <- completeFun(data, c("ActionJudgeHandPos", "KeyStart", "AudioStart"))
  if (nrow(data) == nrow(cleandata)) {
    cat(paste0("Data in ", fname, " looks clean and good to go!!\n"))
    cat("Processing Brain 2010 experiment data.. \n \n")
  } else {
    cat(paste0("Alert: Judgement and Keypress data are missing in ", fname, "!!\n"))
    cat(paste0("No Key press recorded. Trial No:", data$Block[which(is.na(data$KeyStart))], "\n"))
    cat(paste0("No Judgement recorded. Trial No:", data$Block[which(is.na(data$ActionJudgeHandPos))], "\n"))
    cat(paste0("Audio failure. Trial No:", data$Block[which(is.na(data$AudioStart))], "\n"))
    cat(paste0("Total trials to be omitted: ", (nrow(data) - nrow(cleandata))))
    # stop('Exiting function')
  }
  CleanTrials <- (!(is.na(data$KeyStart) | is.na(data$ActionJudgeHandPos) | is.na(data$AudioStart)))
  
  ## Prepare table with clock positions and timings for every trial
  Trial <- c()
  Counter <- c()
  Time <- c()
  HandPos <- c()
  for (j in 1:max(data$Block)) {
    for (i in 1:data$TotSteps[data$Block == j]) {
      Trial <- append(Trial, j)
      Counter <- append(Counter, i)
      Time <- append(Time, eval(parse(text = paste0("data$Time.", i, "[data$Block==", j, "]"))))
      HandPos <- append(HandPos, eval(parse(text = paste0("data$HandPos.", i, "[data$Block==", j, "]"))))
    }
  }
  clockdata <- data.frame(Trial, Counter, Time, HandPos)
  
  ## Codes to get timing corresponding to judged hand position Add continous hand position variable to clockdata
  ContHandPos <- c()
  StepPeriod <- c()
  for (j in 1:max(data$Block)) {
    for (i in 1:data$TotSteps[data$Block == j]) {
      ContHandPos <- append(ContHandPos, clockdata$HandPos[clockdata$Trial == j & clockdata$Counter == 1] + (2.5 * (i - 1)))
      if (i != data$TotSteps[data$Block == j]) {
        StepPeriod <- append(StepPeriod, clockdata$Time[clockdata$Trial == j & clockdata$Counter == (i + 1)] - clockdata$Time[clockdata$Trial == j & clockdata$Counter == i])
      } else {
        StepPeriod <- append(StepPeriod, NA)
      }
    }
  }
  clockdata$ContHandPos <- ContHandPos
  clockdata$StepPeriod <- StepPeriod
  
  write.csv(file = paste0(fname, "_ClockData.csv"), x = clockdata, row.names = FALSE)
  
  ## Calculate average clock step period per trial
  TimeFirstStep <- c()
  TimeEndStep <- c()
  TimeAvgStep <- c()
  for (j in 1:max(clockdata$Trial)) {
    TimeFirstStep[j] <- clockdata$Time[clockdata$Trial == j & clockdata$Counter == 1]
    TimeEndStep[j] <- clockdata$Time[clockdata$Trial == j & clockdata$Counter == data$TotSteps[data$Block == j]]
    TimeAvgStep[j] <- (TimeEndStep[j] - TimeFirstStep[j])/((data$TotSteps[data$Block == j]) - 1)
  }
  
  ## linearly interpolate timing for judged hand position for six rotations and then select the timepoint closest to key press/tone depending on block type
  ContJudgeHandPos <- c()
  JudgeTimemat <- matrix(list(), nrow = max(data$Block), ncol = 1)
  JudgeTimeIndex <- c()
  JudgeTime <- c()
  for (k in 1:max(data$Block)) {
    ContJudgeHandPos <- data$ActionJudgeHandPos[data$Block == k] + (60 * 0:5)
    if (!is.na(data$KeyStart[data$Block == k])) {
      JudgeTimemat[[k, 1]] <- (approx(x = clockdata$ContHandPos[clockdata$Trial == k], y = clockdata$Time[clockdata$Trial == k], xout = ContJudgeHandPos, method = "linear", ties = mean))$y
      JudgeTimeIndex[k] <- which(abs(JudgeTimemat[[k, 1]] - data$KeyStart[data$Block == k]) == min(abs(JudgeTimemat[[k, 1]] - data$KeyStart[data$Block == k]), na.rm = TRUE))
      JudgeTime <- append(JudgeTime, JudgeTimemat[[k, 1]][JudgeTimeIndex[k]])
    } else {
      JudgeTime <- append(JudgeTime, NA)
    }
  }
  
  write.matrix(JudgeTimemat, file = paste0(fname, "_JudgeTime_Matrix.csv"), sep = ",")
  
  ## Prepare summary table of key variables for every trial
  sumdata <- data.frame(
    Name = data$Name,
    Status = data$Status,
    Subject =data$Subject,
    Session =data$Session,
    SessionDate = data$SessionDate,
    ExperimentName = data$ExperimentName,
    Procedure =data$Procedure,
    Block = data$Running,
    BlockCode = data$Running,
    Trial = data$Block,
    CleanTrial = CleanTrials,
    TrialBegin = data$TrialBegin,
    StartHandPos=data$StartHandPos,
    TotSteps=data$TotSteps,
    TimeFirstStep = TimeFirstStep,
    TimeEndStep= TimeEndStep,
    TimeAvgStep= TimeAvgStep,
    KeyCounter=data$KeyCounter,
    KeyCounterDiv=data$KeyCounterDiv,
    KeyCounterFullDiv = data$KeyCounter+(data$KeyCounterDiv/10),
    KeyHandPos = data$KeyHandPos,
    KeyStart=data$KeyStart,
    KeyEnd=data$KeyEnd,
    KeyDuration = data$KeyEnd - data$KeyStart,
    KeyRT=data$KeyRT,
    KeyRTTime=data$KeyRTTime,
    KeyRTdiffQC = data$KeyStart - data$KeyRTTime,
    Soundfile=data$Sound,
    TonePresence = (data$Sound == "sin_1000Hz_250_100ms.wav"),
    ToneCode = (data$Sound == "sin_1000Hz_250_100ms.wav"),
    ToneStartlow = (data$AudioStart + 250),
    ToneStarthigh = (data$AudioEnd - 100),
    ToneDurationQC = (data$AudioEnd - 100)  - (data$AudioStart + 250),
    ToneDurationPerQC = ((data$AudioEnd - 100)  - (data$AudioStart + 250))*100/350,
    ToneStartavg = ((data$AudioEnd - 100)  + (data$AudioStart + 250))/2,
    ToneStartmean = data$AudioStart + (250 * (1 + (((data$AudioEnd - 100)  - (data$AudioStart + 250))/350))),
    AudioStart=data$AudioStart,
    AudioEnd=data$AudioEnd,
    AudioDuration = (data$AudioEnd - data$AudioStart),
    Confidence=c(rep(data$Confidence1[1],(nrow(data)/3)),rep(data$Confidence2[1],(nrow(data)/3)),rep(data$Confidence3[1],(nrow(data)/3))),
    JudgeHandPos = data$ActionJudgeHandPos,
    JudgeTime = JudgeTime,
    JudgeError = JudgeTime - data$KeyStart)
  
  levels(sumdata$BlockCode)[levels(sumdata$BlockCode) == "Tone0Trials"] <- 1
  levels(sumdata$BlockCode)[levels(sumdata$BlockCode) == "Tone50Trials"] <- 2
  levels(sumdata$BlockCode)[levels(sumdata$BlockCode) == "Tone75Trials"] <- 3
  sumdata$ToneCode[(sumdata$ToneCode) == TRUE] <- 1
  sumdata$ToneCode[(sumdata$ToneCode) == FALSE] <- 0
  sumdata$JudgeErrorBL <- sumdata$JudgeError - (mean(subset(sumdata, Block == "Tone0Trials")$JudgeError, na.rm = TRUE))
  
  # Remove the first trial and outliers
  
  sumdata$JudgeErrorOut <- sumdata$JudgeError
  sumdata$JudgeErrorOut[1] <- NA
  sumdata$JudgeErrorOut[sumdata$Trial[sumdata$Block == "Tone0Trials"]] <- removeOut(subset(sumdata, Block == "Tone0Trials")$JudgeErrorOut)
  sumdata$JudgeErrorOut[sumdata$Trial[sumdata$Block == "Tone50Trials" & sumdata$TonePresence == "TRUE"]] <- removeOut(subset(sumdata, Block == "Tone50Trials" & TonePresence == "TRUE")$JudgeErrorOut)
  sumdata$JudgeErrorOut[sumdata$Trial[sumdata$Block == "Tone75Trials" & sumdata$TonePresence == "TRUE"]] <- removeOut(subset(sumdata, Block == "Tone75Trials" & TonePresence == "TRUE")$JudgeErrorOut)
  sumdata$JudgeErrorOut[sumdata$Trial[sumdata$Block == "Tone50Trials" & sumdata$TonePresence == "FALSE"]] <- removeOut(subset(sumdata, Block == "Tone50Trials" & TonePresence == "FALSE")$JudgeErrorOut)
  sumdata$JudgeErrorOut[sumdata$Trial[sumdata$Block == "Tone75Trials" & sumdata$TonePresence == "FALSE"]] <- removeOut(subset(sumdata, Block == "Tone75Trials" & TonePresence == "FALSE")$JudgeErrorOut)
  sumdata$JudgeErrorOutBL <- sumdata$JudgeErrorOut - (mean(subset(sumdata, Block == "Tone0Trials")$JudgeErrorOut, na.rm = TRUE))
  
  write.csv(file = paste0(fname, "_SummaryData.csv"), x = sumdata, row.names = FALSE)
}
