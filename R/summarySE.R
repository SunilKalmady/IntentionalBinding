#' @title summarySE
#' @description Gives count, mean, standard deviation, standard error of the mean, coefficient of variation and confidence interval.
#' @details This is a helper function to obtain descriptives and coefficient of variation. 
#' @param data A data frame
#' @param measurevar The name of a column that contains the variable to be summarized
#' @param groupvars A vector containing names of columns that contain grouping variables
#' @param na.rm A boolean that indicates whether to ignore NA's
#' @param conf.interval The percent range of the confidence interval (default is 95)
#' @return Dataframe with descriptive statistics
#' @author Sunil V Kalmady (citation: cookbook-r.com)
#' @references \url{www.transpsychlab.org}
#' @import plyr
#' @export

summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, conf.interval = 0.95, .drop = TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) 
      sum(!is.na(x)) else length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with N, mean, and sd
  datac <- ddply(data, groupvars, .drop = .drop, .fun = function(xx, col) {
    c(N = length2(xx[[col]], na.rm = na.rm), mean = mean(xx[[col]], na.rm = na.rm), sd = sd(xx[[col]], na.rm = na.rm))
  }, measurevar)
  
  datac$se <- datac$sd/sqrt(datac$N)  # Calculate standard error of the mean
  datac$cv <- datac$sd/datac$mean  # Calculate coefficient of variation
  # Rename the 'mean' column
  datac <- rename(datac, c(mean = measurevar))
  
  # Confidence interval multiplier for standard error Calculate t-statistic for confidence interval: e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + 0.5, datac$N - 1)
  datac$ci <- datac$se * ciMult
  return(datac)
}
