
IBaggregate <- function(fname){
  
  dataset <- read.table(fname, sep = ",", header = TRUE)
  
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
  
  write.csv(file = paste0(fname, "_AggregateData.csv"), x = dataSE3, row.names = FALSE)
  
  #generate report  
  rmarkdown::render('IBreport_MultiSubject.Rmd', output_format = 'html_document', quiet = TRUE, output_file = paste0(fname,'_IBreport_MultiSubject.html'))
} 