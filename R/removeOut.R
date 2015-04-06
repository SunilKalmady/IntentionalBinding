#' @title removeOut
#' @description Remove outliers using tukey's method
#' @param x A vector
#' @param na.rm A boolean that indicates whether to ignore NA's
#' @return Vector with outliers removed
#' @references \url{www.transpsychlab.org}
#' @details This is a helper function to eliminate trials with outlying judgment values.
#' @author Sunil V Kalmady (Citation: stackoverflow.com)
#' @export

removeOut <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}