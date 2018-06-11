#' Function to restrict the legth of dataset in multiples of 24
#'
#' @param dataIn as inpute time series data with missing values
#' @param n.ahead as xxx
#' @import PSF
#' @import forecast
#' @return returns the time series with multiples of 24
#' @export

lpsf <- function(dataIn, n.ahead)
{
  x <- dataIn
  xn <- length(x)
  y <- xn%%12
  x1 <- x[y:xn]
  tryCatch({x2 <- psf(data = x1,  cycle = 12)
  x2 <- predict(object = x2, n.ahead = n.ahead)
  return(x2)},

  error = function(e) {
    x2 <- forecast(auto.arima(dataIn), n.ahead)
    x2 <- x2$mean
    x3 <- as.numeric(x2)
    return(x3)
  })
  # return(x2)
}














#lpsf <- function(dataIn, n.ahead)
#{
#  x <- dataIn
#  xn <- length(x)
#  y <- xn%%24
#  x1 <- x[y:xn]
#  tryCatch({x2 <- psf(data = x1, n.ahead = n.ahead)
#            x2 <- x2$predictions
#           return(x2)},
#
#  error = function(e) {
#    x2 <- forecast(auto.arima(dataIn), n.ahead)
#    x2 <- x2$mean
#    x3 <- as.numeric(x2)
#    return(x3)
#    })
# # return(x2)
#}
