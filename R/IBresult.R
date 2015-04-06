#' @title IBresult
#' @description Generate ANOVA results for Intentional Binding summary data.
#' @details IBsummary should be run first and SummaryData.csv should be available. 
#' Output is only displayed on the console.
#' @references \url{www.transpsychlab.org}
#' @author Sunil V Kalmady
#' @export 
#' @param fname Filename of unicode tab-delimited text from e-prime experiment output.
#' @examples
#'\dontrun{
#' # A general example
#' IBsummary("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' IBresult("Brain2010-IB-0_50_75-A-3-1.edat2.txt")
#' 
#' # Try out the example data provided with the package 
#' # copy A.edat2.txt or H.edat2.txt to working directory (manually or using codes below)
#' srcpath <- system.file("extdata/A.edat2.txt", package = "IntentionalBinding")
#' destpath <- getwd()
#' file.copy(srcpath, destpath)
#' 
#' #Run Intentional binding result function
#' IBsummary("A.edat2.txt")
#' IBresult("A.edat2.txt")
#'}

IBresult <- function(fname) {
  
  sumdata <- read.table(paste0(fname, "_SummaryData.csv"), sep = ",", header = TRUE)
  
  cat("\n ----- Baseline corrected judgement error ----- \n \n")
  cat("\n Independent sample t test - effect of tone presence \n")
  print(t.test(JudgeErrorBL ~ TonePresence, data = sumdata))
  cat("\n ANOVA - effect of tone probability [all 3 blocks - 0%, 50%, 75%] \n")
  print(summary(aov(JudgeErrorBL ~ Block, data = sumdata)))
  cat("\n Independent sample t test - effect of tone probability [2 blocks - 50%, 75%] \n")
  print(t.test(JudgeErrorBL ~ Block, data = subset(sumdata, Block != "Tone0Trials")))
  
  cat("\n ----- Baseline corrected judgement error in 'without tone' trials ----- \n \n")
  cat("\n ANOVA - effect of tone probability [all 3 blocks - 0%, 50%, 75%] \n")
  print(summary(aov(JudgeErrorBL ~ Block, data = subset(sumdata, TonePresence == FALSE))))
  cat("\n Independent sample t test - effect of tone probability [2 blocks - 50%, 75%]")
  print(t.test(JudgeErrorBL ~ Block, data = subset(sumdata, (TonePresence == FALSE & Block != "Tone0Trials"))))
  
  cat("\n ----- Baseline corrected judgement error in 'with tone' trials ----- \n \n")
  cat("\n ANOVA - effect of tone probability [all 3 blocks - 0%, 50%, 75%]")
  print(summary(aov(JudgeErrorBL ~ Block, data = subset(sumdata, TonePresence == TRUE))))
  cat("\n Independent sample t test - effect of tone probability [2 blocks - 50%, 75%]")
  print(t.test(JudgeErrorBL ~ Block, data = subset(sumdata, (TonePresence == TRUE & Block != "Tone0Trials"))))
  
  cat("\n ----- Outliers removed - Baseline corrected judgement error ----- \n \n")
  cat("\n Independent sample t test - effect of tone presence \n")
  print(t.test(JudgeErrorOutBL ~ TonePresence, data = sumdata))
  cat("\n ANOVA - effect of tone probability [all 3 blocks - 0%, 50%, 75%] \n")
  print(summary(aov(JudgeErrorOutBL ~ Block, data = sumdata)))
  cat("\n Independent sample t test - effect of tone probability [2 blocks - 50%, 75%] \n")
  print(t.test(JudgeErrorOutBL ~ Block, data = subset(sumdata, Block != "Tone0Trials")))
  
  cat("\n ----- Outliers removed - Baseline corrected judgement error in 'without tone' trials ----- \n \n")
  cat("\n ANOVA - effect of tone probability [all 3 blocks - 0%, 50%, 75%] \n")
  print(summary(aov(JudgeErrorOutBL ~ Block, data = subset(sumdata, TonePresence == FALSE))))
  cat("\n Independent sample t test - effect of tone probability [2 blocks - 50%, 75%]")
  print(t.test(JudgeErrorOutBL ~ Block, data = subset(sumdata, (TonePresence == FALSE & Block != "Tone0Trials"))))
  
  cat("\n ----- Outliers removed - Baseline corrected judgement error in 'with tone' trials ----- \n \n")
  cat("\n ANOVA - effect of tone probability [all 3 blocks - 0%, 50%, 75%]")
  print(summary(aov(JudgeErrorOutBL ~ Block, data = subset(sumdata, TonePresence == TRUE))))
  cat("\n Independent sample t test - effect of tone probability [2 blocks - 50%, 75%]")
  print(t.test(JudgeErrorOutBL ~ Block, data = subset(sumdata, (TonePresence == TRUE & Block != "Tone0Trials"))))
}
