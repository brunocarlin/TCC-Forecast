
Mean_Error <- function(y = NULL, error) {
  mean(error, na.rm = TRUE)
}

CV_Mean_Error <- function(y = NULL, error) {
  colMeans(error, na.rm = TRUE)
}


Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  return(Percentage_Error_Result)
  
}

Mean_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  mean(Percentage_Error_Result, na.rm = TRUE)
}

CV_Mean_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  colMeans(Percentage_Error_Result, na.rm = TRUE)
}


Squared_Error <- function(y = NULL, error) {
  error ^ 2
}

Mean_Squared_Error <- function(y = NULL, error) {
  mean(error ^ 2, na.rm = TRUE)
}

CV_Mean_Squared_Error <- function(y = NULL, error) {
  colMeans(error ^ 2, na.rm = TRUE)
}


Root_Mean_Squared_Error <- function(y = NULL, error) {
  mean(sqrt(error ^ 2), na.rm = TRUE)
}

CV_Root_Mean_Squared_Error <- function(y = NULL, error) {
  colMeans(sqrt(error ^ 2), na.rm = TRUE)
}


Absolute_Error <- function(y = NULL, error) {
  abs(error)
}

Mean_Absolute_Error <- function(y = NULL, error) {
  mean(abs(error), na.rm = TRUE)
}

CV_Mean_Absolute_Error <- function(y = NULL, error) {
  colMeans(abs(error), na.rm = TRUE)
}


Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  Absolute_Percentage_Error_Result <- abs(Percentage_Error_Result)
  return(Absolute_Percentage_Error_Result)
}

Mean_Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  Absolute_Percentage_Error_Result <- abs(Percentage_Error_Result)
  Mean_Absolute_Percentage_Error_Result <-
    mean(Absolute_Percentage_Error_Result, na.rm = TRUE)
  return(Mean_Absolute_Percentage_Error_Result)
}

CV_Mean_Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Percentage_Error_Result <- (100 * error) / y
  colnames(Percentage_Error_Result) <- col_Names_H
  Absolute_Percentage_Error_Result <- abs(Percentage_Error_Result)
  CV_Mean_Absolute_Percentage_Error_Result <-
    colMeans(Absolute_Percentage_Error_Result, na.rm = TRUE)
  return(CV_Mean_Absolute_Percentage_Error_Result)
  
}


Symmetric_Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Absolute_Error <- abs(error)
  Error_Plus_y <- error + y
  Symmetric_Absolute_Percentage_Error <- 200 * Absolute_Error / Error_Plus_y
  colnames(Symmetric_Absolute_Percentage_Error) <- col_Names_H
  return(Symmetric_Absolute_Percentage_Error)
}

Mean_Symmetric_Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Absolute_Error <- abs(error)
  Error_Plus_y <- error + y
  Symmetric_Absolute_Percentage_Error <-
    200 * Absolute_Error / Error_Plus_y
  colnames(Symmetric_Absolute_Percentage_Error) <- col_Names_H
  mean(Symmetric_Absolute_Percentage_Error, na.rm = TRUE)
}

CV_Mean_Symmetric_Absolute_Percentage_Error <- function(y, error) {
  col_Names_H <- colnames(error)
  Absolute_Error <- abs(error)
  Error_Plus_y <- error + y
  Symmetric_Absolute_Percentage_Error <- 200 * Absolute_Error / Error_Plus_y
  colnames(Symmetric_Absolute_Percentage_Error) <- col_Names_H
  colMeans(Symmetric_Absolute_Percentage_Error, na.rm = TRUE)
}


Absolute_Scaled_Error <- function(y, error) {
  (abs(error) / mean(abs(diff(y, lag = frequency(
    y
  )))))
}

Mean_Absolute_Scaled_Error <- function(y, error) {
  ASE <-  (abs(error) / mean(abs(diff(y, lag = frequency(
    y
  )))))
  mean(ASE, na.rm = TRUE)
}

CV_Mean_Absolute_Scaled_Error <- function(y, error) {
  ASE <-  (abs(error) / mean(abs(diff(y, lag = frequency(
    y
  )))))
  colMeans(ASE, na.rm = TRUE)
}


Mean_Accuracy <- function(y, error) {
  Mean_Accracy_Results <- rbind(
    ME = Mean_Error(y, error),
    RMSE = Root_Mean_Squared_Error(y, error),
    MAE = Mean_Absolute_Error(y, error),
    MPE = Mean_Percentage_Error(y, error),
    MAPE = Mean_Absolute_Percentage_Error(y, error),
    MASE = Mean_Absolute_Scaled_Error(y, error),
    sMAPE = Mean_Symmetric_Absolute_Percentage_Error(y, error)
  )
  colnames(Mean_Accracy_Results) <- "Placeholder_Name"
  return(Mean_Accracy_Results)
}

CV_Mean_Accuracy <- function(y, error) {
  Mean_Accracy_Results <- rbind(
    ME = CV_Mean_Error(y, error),
    RMSE = CV_Root_Mean_Squared_Error(y, error),
    MAE = CV_Mean_Absolute_Error(y, error),
    MPE = CV_Mean_Percentage_Error(y, error),
    MAPE = CV_Mean_Absolute_Percentage_Error(y, error),
    MASE = CV_Mean_Absolute_Scaled_Error(y, error),
    sMAPE = CV_Mean_Symmetric_Absolute_Percentage_Error(y, error)
  )
  colnames(Mean_Accracy_Results) <- colnames(error)
  return(Mean_Accracy_Results)
}



Better_CV <- function (y, forecastfunction, h = 1, window = NULL,start= NULL, ...) {
  y <- as.ts(y)
  n <- length(y)
  e <- ts(matrix(NA_real_, nrow = n, ncol = h))
  tsp(e) <- tsp(y)
  for (i in seq_len(n - 1)) {
    fc <- try(suppressWarnings(forecastfunction(subset(y, 
                                                       start = ifelse(i- start >= 0L, ifelse(is.null(window), 1L, ifelse(i - window >= 
                                                                                                                          0L, i - window + 1L, stop("small window"))),stop("Too Short")), 
                                                       end = i), h = h, ...)), silent = TRUE)
    if (!is.element("try-error", class(fc))) {
      e[i, ] <- y[i + (1:h)] - fc$mean
    }
  }
  if (h == 1) {
    return(e[, 1L])
  }
  else {
    colnames(e) <- paste("h=", 1:h, sep = "")
    return(e)
  }
}
