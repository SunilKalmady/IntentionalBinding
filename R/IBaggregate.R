
IBaggregate <- function(fname){
  
  dataset <- read.table(fname, sep = ",", header = TRUE)
  
  #Dataset 1 - aggregate by Subject, status and tone presence
  dataSE1 <- summarySE(dataset, measurevar="JudgeErrorOutBL", groupvars=c("Name","Status","TonePresence"), na.rm = TRUE)
  
  #Dataset 2 - aggregate by Subject, status and tone probability
  dataSE2 <- summarySE(dataset, measurevar="JudgeErrorOutBL", groupvars=c("Name","Status","Block"), na.rm = TRUE)
  
  #Dataset 3 - aggregate by Subject, status, tone  presence and tone probability
  dataSE3 <- summarySE(dataset, measurevar="JudgeErrorOutBL", groupvars=c("Name","Status","Block","TonePresence"), na.rm = TRUE)
  
  rmarkdown::render(input = IBreport_MultiSubject.Rmd, output_format = 'html_document', quiet = TRUE, output_file = paste0(fname,'_IBreport_MultiSubject.html'))
} 