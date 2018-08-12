
library(smooth)



mape <- function(y, error) {
  (abs(error) / mean(abs(diff(y, lag = frequency(y)
  ))))
}

percenterror <- function(y, error) {
  100 * error / y
}

rmse <- function(error) {
  sqrt(mean(error ^ 2, na.rm = TRUE))
}

mae <- function(error) {
  mean(abs(error))
}

maper <- function(percenterror) {
  mean(abs(percenterror), na.rm = TRUE)
}

smape <- function(error, plus) {
  mean(200 * abs(error) / (plus), na.rm = TRUE)
}

mse <- function(error) {
  mean(error ^ 2, na.rm = TRUE)
}

me <- function(error) {
  mean(error, na.rm = TRUE)
}

mpe <- function(percenterror) {
  mean(percenterror, na.rm = TRUE)
}

sapply(MatrixErrors1, rmse)


