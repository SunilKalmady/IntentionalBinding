#' @title completeFun
#' @description Extract rows with complete data
#' @details This is a helper function to eliminate trials missing critical data such as key press.
#' @param dat A data frame
#' @param desiredCols A vector of column names
#' @return Data frame with complete rows 
#' @author Sunil V Kalmady (Citation: stackoverflow.com)
#' @references \url{www.transpsychlab.org}
#' @export

completeFun <- function(dat, desiredCols) {
  completeVec <- complete.cases(dat[, desiredCols])
  return(dat[completeVec, ])
}