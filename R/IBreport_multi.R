#' @title IBreport_multi
#' @description Generate detailed HTML markdown report from Intentional Binding summary data for multiple subjects.
#' @details Combined dataset with more than one SummaryData.csv files should be available (use IBcombine to achieve this). 
#' Atleast 2 subjects in each group should be available for statisical analysis.
#' In addition to detailed report, a dataset aggregated by Subject/Block/Tone-presence is also generated along with column indicating potential outliers.
#' @references \url{www.transpsychlab.org}
#' @author Sunil V Kalmady
#' @import ggplot2
#' @import markdown
#' @import knitr
#' @export 
#' @param flname Filename of the dataset combining more than one SummaryData.csv files. 
#' @note The plots use factor level of Status: 'Cntrl' & 'SCZ' and labels them as  Healthy controls & Schizophrenia patients respectively.  
#'  @examples
#'\dontrun{
#'# Try out the example data provided with the package 
#' # copy multiple txt files - A.edat2.txt, H.edat2.txt etc to working directory (manually or using codes below)
#' srcpath <- system.file("extdata/", package = "IntentionalBinding")
#' flst <- list.files(srcpath, ".txt$", full.names = TRUE)
#' destpath <- getwd()
#' file.copy(flst, destpath)
#' 
#' #Run Intentional binding summary function 
#' IBsummary("A.edat2.txt")
#' IBsummary("H.edat2.txt")
#' IBsummary("F.edat2.txt")
#' IBsummary("C.edat2.txt")
#' 
#'# Combine the SummaryData output from all four files
#' IBcombine(all = TRUE)
#' 
#'# Now you are ready to run multisubject report on combined summary data.
#' IBreport_multi("4files_SummaryData_Combined.csv")
#'}

IBreport_multi <- function(flname){
  
  dataset <- read.table(flname, sep = ",", header = TRUE)
  
  #Dataset 1 - aggregate by Subject, status and tone presence
  dataSE1 <- summarySE(dataset, measurevar="JudgeErrorOutBL", groupvars=c("Name","Status","TonePresence"), na.rm = TRUE)
  
  #Dataset 2 - aggregate by Subject, status and tone probability
  dataSE2 <- summarySE(dataset, measurevar="JudgeErrorOutBL", groupvars=c("Name","Status","Block"), na.rm = TRUE)
  
  #Dataset 3 - aggregate by Subject, status, tone  presence and tone probability
  dataSE3 <- summarySE(dataset, measurevar="JudgeErrorOutBL", groupvars=c("Name","Status","Block","TonePresence"), na.rm = TRUE)
  
  # Identify outliers in Dataset 3 - at subject/Block/Tone level
  
  dataSE3$slno <- as.numeric(rownames(dataSE3))
  dataSE3$JudgeErrorOutBLOut <- dataSE3$JudgeErrorOutBL
  #dataSE3$JudgeErrorOutBLOut[dataSE3$slno[dataSE3$Status == "Cntrl" & dataSE3$Block == "Tone0Trials"]] <- removeOut(subset(dataSE3, Block == "Tone0Trials" & Status == "Cntrl")$JudgeErrorOutBLOut)
  dataSE3$JudgeErrorOutBLOut[dataSE3$slno[dataSE3$Status == "Cntrl" & dataSE3$Block == "Tone50Trials" & dataSE3$TonePresence == "TRUE"]] <- removeOut(subset(dataSE3, Block == "Tone50Trials" & TonePresence == "TRUE" & Status == "Cntrl")$JudgeErrorOutBLOut)
  dataSE3$JudgeErrorOutBLOut[dataSE3$slno[dataSE3$Status == "Cntrl" & dataSE3$Block == "Tone75Trials" & dataSE3$TonePresence == "TRUE"]] <- removeOut(subset(dataSE3, Block == "Tone75Trials" & TonePresence == "TRUE" & Status == "Cntrl")$JudgeErrorOutBLOut)
  dataSE3$JudgeErrorOutBLOut[dataSE3$slno[dataSE3$Status == "Cntrl" & dataSE3$Block == "Tone50Trials" & dataSE3$TonePresence == "FALSE"]] <- removeOut(subset(dataSE3, Block == "Tone50Trials" & TonePresence == "FALSE" & Status == "Cntrl")$JudgeErrorOutBLOut)
  dataSE3$JudgeErrorOutBLOut[dataSE3$slno[dataSE3$Status == "Cntrl" & dataSE3$Block == "Tone75Trials" & dataSE3$TonePresence == "FALSE"]] <- removeOut(subset(dataSE3, Block == "Tone75Trials" & TonePresence == "FALSE" & Status == "Cntrl")$JudgeErrorOutBLOut)
  dataSE3$JudgeErrorOutBLOut[dataSE3$slno[dataSE3$Status == "SCZ" & dataSE3$Block == "Tone0Trials"]] <- removeOut(subset(dataSE3, Block == "Tone0Trials" & Status == "SCZ")$JudgeErrorOutBLOut)
  dataSE3$JudgeErrorOutBLOut[dataSE3$slno[dataSE3$Status == "SCZ" & dataSE3$Block == "Tone50Trials" & dataSE3$TonePresence == "TRUE"]] <- removeOut(subset(dataSE3, Block == "Tone50Trials" & TonePresence == "TRUE" & Status == "SCZ")$JudgeErrorOutBLOut)
  dataSE3$JudgeErrorOutBLOut[dataSE3$slno[dataSE3$Status == "SCZ" & dataSE3$Block == "Tone75Trials" & dataSE3$TonePresence == "TRUE"]] <- removeOut(subset(dataSE3, Block == "Tone75Trials" & TonePresence == "TRUE" & Status == "SCZ")$JudgeErrorOutBLOut)
  dataSE3$JudgeErrorOutBLOut[dataSE3$slno[dataSE3$Status == "SCZ" & dataSE3$Block == "Tone50Trials" & dataSE3$TonePresence == "FALSE"]] <- removeOut(subset(dataSE3, Block == "Tone50Trials" & TonePresence == "FALSE" & Status == "SCZ")$JudgeErrorOutBLOut)
  dataSE3$JudgeErrorOutBLOut[dataSE3$slno[dataSE3$Status == "SCZ" & dataSE3$Block == "Tone75Trials" & dataSE3$TonePresence == "FALSE"]] <- removeOut(subset(dataSE3, Block == "Tone75Trials" & TonePresence == "FALSE" & Status == "SCZ")$JudgeErrorOutBLOut)
  
  Out_mean=data.frame(Status=dataSE3$Status[is.na(dataSE3$JudgeErrorOutBLOut)],
                      Name=dataSE3$Name[is.na(dataSE3$JudgeErrorOutBLOut)],
                      Block=dataSE3$Block[is.na(dataSE3$JudgeErrorOutBLOut)],
                      Tone=dataSE3$TonePresence[is.na(dataSE3$JudgeErrorOutBLOut)],
                      mean=dataSE3$JudgeErrorOutBL[is.na(dataSE3$JudgeErrorOutBLOut)])
  
  
  dataSE3$sdOut <- dataSE3$sd
  dataSE3$sdOut[dataSE3$slno[dataSE3$Status == "Cntrl" & dataSE3$Block == "Tone0Trials"]] <- removeOut(subset(dataSE3, Block == "Tone0Trials" & Status == "Cntrl")$sdOut)
  dataSE3$sdOut[dataSE3$slno[dataSE3$Status == "Cntrl" & dataSE3$Block == "Tone50Trials" & dataSE3$TonePresence == "TRUE"]] <- removeOut(subset(dataSE3, Block == "Tone50Trials" & TonePresence == "TRUE" & Status == "Cntrl")$sdOut)
  dataSE3$sdOut[dataSE3$slno[dataSE3$Status == "Cntrl" & dataSE3$Block == "Tone75Trials" & dataSE3$TonePresence == "TRUE"]] <- removeOut(subset(dataSE3, Block == "Tone75Trials" & TonePresence == "TRUE" & Status == "Cntrl")$sdOut)
  dataSE3$sdOut[dataSE3$slno[dataSE3$Status == "Cntrl" & dataSE3$Block == "Tone50Trials" & dataSE3$TonePresence == "FALSE"]] <- removeOut(subset(dataSE3, Block == "Tone50Trials" & TonePresence == "FALSE" & Status == "Cntrl")$sdOut)
  dataSE3$sdOut[dataSE3$slno[dataSE3$Status == "Cntrl" & dataSE3$Block == "Tone75Trials" & dataSE3$TonePresence == "FALSE"]] <- removeOut(subset(dataSE3, Block == "Tone75Trials" & TonePresence == "FALSE" & Status == "Cntrl")$sdOut)
  dataSE3$sdOut[dataSE3$slno[dataSE3$Status == "SCZ" & dataSE3$Block == "Tone0Trials"]] <- removeOut(subset(dataSE3, Block == "Tone0Trials" & Status == "SCZ")$sdOut)
  dataSE3$sdOut[dataSE3$slno[dataSE3$Status == "SCZ" & dataSE3$Block == "Tone50Trials" & dataSE3$TonePresence == "TRUE"]] <- removeOut(subset(dataSE3, Block == "Tone50Trials" & TonePresence == "TRUE" & Status == "SCZ")$sdOut)
  dataSE3$sdOut[dataSE3$slno[dataSE3$Status == "SCZ" & dataSE3$Block == "Tone75Trials" & dataSE3$TonePresence == "TRUE"]] <- removeOut(subset(dataSE3, Block == "Tone75Trials" & TonePresence == "TRUE" & Status == "SCZ")$sdOut)
  dataSE3$sdOut[dataSE3$slno[dataSE3$Status == "SCZ" & dataSE3$Block == "Tone50Trials" & dataSE3$TonePresence == "FALSE"]] <- removeOut(subset(dataSE3, Block == "Tone50Trials" & TonePresence == "FALSE" & Status == "SCZ")$sdOut)
  dataSE3$sdOut[dataSE3$slno[dataSE3$Status == "SCZ" & dataSE3$Block == "Tone75Trials" & dataSE3$TonePresence == "FALSE"]] <- removeOut(subset(dataSE3, Block == "Tone75Trials" & TonePresence == "FALSE" & Status == "SCZ")$sdOut)
  
  Out_sd=data.frame(Status=dataSE3$Status[is.na(dataSE3$sdOut)],
                    Name=dataSE3$Name[is.na(dataSE3$sdOut)],
                    Block=dataSE3$Block[is.na(dataSE3$sdOut)],
                    Tone=dataSE3$TonePresence[is.na(dataSE3$sdOut)],
                    sd=dataSE3$sd[is.na(dataSE3$sdOut)])
  
  write.csv(file = paste0(flname, "_AggregateData.csv"), x = dataSE3, row.names = FALSE)
  
  #generate report  
  #rmarkdown::render('IBreport_MultiSubject.Rmd', output_format = 'html_document', quiet = TRUE, output_file = paste0(fname,'_IBreport_MultiSubject.html'))
  rmd_path = system.file("extdata/IBreport_MultiSubject.Rmd", package = "IntentionalBinding")
  out_path = paste0(getwd())
  rmarkdown::render(input = rmd_path, output_format = 'html_document', quiet = TRUE, output_dir = out_path, output_file = paste0(flname,'_IBreport_MultiSubject.html'))
} 




