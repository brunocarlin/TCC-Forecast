library(future.apply)
library(furrr)

# libraries used ----------------------------------------------------------


library(forecast)
library(tidyverse)
library(Mcomp)
library(rlist)
library(tictoc)
library(reshape2)

# Use Forecast Functions --------------------------------------------------


AR <- function(x, h) {
  forecast(auto.arima(x,
                      #stepwise=FALSE,
                      #approximation=FALSE
  ), h = h)
}

TB <- function(x, h) {
  forecast(tbats(x), h = h)
}

ET <- function(x, h) {
  forecast(ets(x), h = h)
}

NN <- function(x, h) {
  forecast(nnetar(x), h = h)
}

SA <- function(x, h) {
  forecast(stlm(x, method = "arima"), h = h)
}

SE <- function(x, h) {
  forecast(stlm(x, method = "ets"), h = h)
}

TH <- function(x, h) {
  forecast(thetaf(x), h = h)
}

RW <- function(x, h) {
  rwf(x, drift = TRUE, h = h)
}

SN <- function(x, h) {
  forecast(snaive(x), h = h)
}

Forecast_Functions <- list(
  "Auto_Arima"      =     AR,
  "Tbats"           =     TB,
  "ETS"             =     ET,
  "Neural_Network"  =     NN,
  "Seasonal_AR"     =     SA,
  "Seasonal_ETS"    =     SE,
  "Thetha"          =     TH,
  "Randon_Walk"     =     RW,
  "Seasonal_Naive"  =     SN)

# Function Used -----------------------------------------------------------
Forecast_Saver <- function(y,List_Functions,h) {
  
  Number_Functions <- length(List_Functions)
  Names <- vector("character",Number_Functions)
  Forecass_Obeject <- vector("list",Number_Functions)
  
  for(i in seq_len(Number_Functions)) {
    Forecass_Obeject[[i]] <- List_Functions[[i]](y,h)
    Names[i] <- names(List_Functions[i])
    
    
  }
  names(Forecass_Obeject) <- Names
  return(Forecass_Obeject)
  
}
# Create List with Functions ----------------------------------------------
# Error Calculations Functions --------------------------------------------


Calculate_Errors <-function(y,List_Errors) {
  
  Number_Models <- length(List_Errors)
  Error_Metrics_CV <- vector("list",Number_Models)
  Error_Metrics_Mean <- vector("list",Number_Models)
  
  for(i in seq_len(Number_Models)) {
    Error_Metrics_CV[[i]] <- CV_Mean_Accuracy(y,List_Errors[[i]])
    Error_Metrics_Mean[[i]] <- Mean_Accuracy(y,List_Errors[[i]])
    
    
  }
  names(Error_Metrics_CV) <- names(List_Errors)
  names(Error_Metrics_Mean) <- names(List_Errors)
  
  return(list(CV_Error_Metrics = Error_Metrics_CV,Mean_Error_Metrics =Error_Metrics_Mean))
  
  
}

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
  sqrt(mean(error ^ 2, na.rm = TRUE))
}

CV_Root_Mean_Squared_Error <- function(y = NULL, error) {
  sqrt(colMeans(error ^ 2, na.rm = TRUE))
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


Mean_Random_Error <- function(y, error) {
  
  MRE <- apply(error, 1, function(x) x*0 + rnorm(ncol(error)) )
  
  
  mean(MRE, na.rm = TRUE)
  
}

CV_Mean_Random_Error <- function(y, error) {
  
  CMRE <- apply(error, 1, function(x) x*0 + rnorm(ncol(error)) )
  
  
  rowMeans(CMRE, na.rm = TRUE)
  
}


Mean_Accuracy <- function(y, error) {
  Mean_Accracy_Results <- rbind(
    AME = abs(Mean_Error(y, error)),
    RMSE = Root_Mean_Squared_Error(y, error),
    MAE = Mean_Absolute_Error(y, error),
    AMPE = abs(Mean_Percentage_Error(y, error)),
    MAPE = Mean_Absolute_Percentage_Error(y, error),
    MASE = Mean_Absolute_Scaled_Error(y, error),
    sMAPE = Mean_Symmetric_Absolute_Percentage_Error(y, error),
    AMRE = abs(Mean_Random_Error(y,error))
  )
  colnames(Mean_Accracy_Results) <- "Averaged_Time"
  return(Mean_Accracy_Results)
}

CV_Mean_Accuracy <- function(y, error) {
  CV_Mean_Accuracy_Results <- rbind(
    AME = abs(CV_Mean_Error(y, error)),
    RMSE = CV_Root_Mean_Squared_Error(y, error),
    MAE = CV_Mean_Absolute_Error(y, error),
    AMPE = abs(CV_Mean_Percentage_Error(y, error)),
    MAPE = CV_Mean_Absolute_Percentage_Error(y, error),
    MASE = CV_Mean_Absolute_Scaled_Error(y, error),
    sMAPE = CV_Mean_Symmetric_Absolute_Percentage_Error(y, error),
    AMRE = abs(CV_Mean_Random_Error(y,error))
  )
  colnames(CV_Mean_Accuracy_Results) <- colnames(error)
  return(CV_Mean_Accuracy_Results)
}

Both_Accuracies <- function(y, error) {
  CV_Mean_Accuracy_Results <- rbind(
    ME = Mean_Error(y, error),
    RMSE = Root_Mean_Squared_Error(y, error),
    MAE = Mean_Absolute_Error(y, error),
    MPE = Mean_Percentage_Error(y, error),
    MAPE = Mean_Absolute_Percentage_Error(y, error),
    MASE = Mean_Absolute_Scaled_Error(y, error),
    sMAPE = Mean_Symmetric_Absolute_Percentage_Error(y, error)
  )
  colnames(CV_Mean_Accuracy_Results) <- "Mean Hs"
  
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
  
  return(list(Mean_Accracy_Results,CV_Mean_Accuracy_Results))
}
# List Manipulation and Inversion --------------------------------------------------------
Invert_List_Accuracy <- function(Accuracy) {
  1/Accuracy 
  
}


Create_Weight_Matrix <- function(List_Accuracy) {
  
  RowNames <- names(List_Accuracy)
  
  ColNames <- colnames(List_Accuracy[[1]])
  
  ListNames <- rownames(List_Accuracy[[1]])
  
  Number_Models <- length(List_Accuracy)
  
  Number_Errors <- length(List_Accuracy[[1]][,1])
  
  Number_Predictions <- length( List_Accuracy[[1]][1,])
  
  
  Weight_Matrix <- vector("list",Number_Errors)
  Temporary_Matrix <-
    matrix(0, nrow = Number_Models, ncol = Number_Predictions)
  
  rownames(Temporary_Matrix) <- RowNames
  colnames(Temporary_Matrix) <- ColNames
  
  for(i in seq_len(Number_Errors)){
    
    
    for (j in seq_len(Number_Models)) {
      
      Temporary_Matrix[j,] <- List_Accuracy[[j]][i,1:Number_Predictions]
    }
    Temporary_Matrix <- sweep(Temporary_Matrix, 2, colSums(Temporary_Matrix), FUN = "/")
    
    Weight_Matrix[[i]] <- Temporary_Matrix
  }
  names(Weight_Matrix) <- ListNames
  return(Weight_Matrix)
}



# Rank Functions ----------------------------------------------------------

Rank <- function(Matrix_Weights, Position_Winner) {
  
  Number_Predictions <- length(Matrix_Weights[1, ])
  
  Number_Models <- length(Matrix_Weights[,1])
  
  
  for (i in seq_len(Number_Predictions )){
    
    Vector_Weights <- Matrix_Weights[,i]
    
    ord <- order(Vector_Weights, decreasing = T)
    
    Vector_Weights <- Vector_Weights[order(Vector_Weights, decreasing = TRUE)]
    
    Vector_Weights[(Position_Winner + 1 ):Number_Models] <- 0
    
    Vector_Weights <- Vector_Weights[order(ord)]
    
    Matrix_Weights[, i] <- Vector_Weights
    
  }
  return(Matrix_Weights)
}

Rank_Maker <- function(Matrix_Weights) {
  
  Number_Models <- length(Matrix_Weights[[1]][[1]][,1])
  
  
  Ranks <- vector("list", Number_Models)
  
  for (i in seq_len(Number_Models)) {
    Ranks[[i]] <- lapply(Matrix_Weights,lapply,Rank,i)
    
  }
  names(Ranks) <- paste(1:Number_Models,"_Selected_Models", sep = "")
  return(Ranks)
}

# Greater Than Value and Function on vector Selections -------------------------------------------------------------------

Greater_Value <- function(Matrix_Weights,Min_Value,If_Lower_Average = FALSE) {
  
  Number_Models <- length(Matrix_Weights[ ,1])
  Number_Predictions   <- length(Matrix_Weights[1,])
  
  for (i in seq_len(Number_Predictions)) {
    Vector_Weights <- Matrix_Weights[, i]
    
    for (j in  seq_len(Number_Models)) {
      if (Min_Value > Vector_Weights[[j]]) {
        Vector_Weights[[j]] <- 0
      }
      
      
      if (mean(Vector_Weights) != 0) {
        
      } else {
        if (If_Lower_Average == TRUE) {
          for (j in  seq_len(Number_Models)) {
            Vector_Weights[[j]] <- 1 / Number_Models
          }
        }
        
      }
      Matrix_Weights[, i] <- Vector_Weights
    }
  }
  return(Matrix_Weights)
}

Greater_Function_Vector <- function(Matrix_Weights,Function_Vector,If_Lower_Average = FALSE,...) {
  
  Number_Models <- length(Matrix_Weights[ ,1])
  Number_Predictions   <- length(Matrix_Weights[1,])
  
  for (i in seq_len(Number_Predictions)) {
    Vector_Weights <- Matrix_Weights[, i]
    Value <- Function_Vector(Vector_Weights)
    for (j in  seq_len(Number_Models)) {
      if (Value > Vector_Weights[[j]]) {
        Vector_Weights[[j]] <- 0
      }
      
      
      if (mean(Vector_Weights) != 0) {
        
      } else {
        if (If_Lower_Average == TRUE) {
          for (j in  seq_len(Number_Models)) {
            Vector_Weights[[j]] <- 1 / Number_Models
          }
        }
        
      }
      Matrix_Weights[, i] <- Vector_Weights
    }
  }
  return( Matrix_Weights)
}

# List Inversion but keeping errors instead of weights --------------------
Create_Error_Matrix <- function(List_Accuracy) {
  
  RowNames <- names(List_Accuracy)
  
  ColNames <- colnames(List_Accuracy[[1]])
  
  ListNames <- rownames(List_Accuracy[[1]])
  
  Number_Models <- length(List_Accuracy)
  
  Number_Errors <- length(List_Accuracy[[1]][,1])
  
  Number_Predictions <- length( List_Accuracy[[1]][1,])
  
  
  Weight_Matrix <- vector("list",Number_Errors)
  Temporary_Matrix <-
    matrix(0, nrow = Number_Models, ncol = Number_Predictions)
  
  rownames(Temporary_Matrix) <- RowNames
  colnames(Temporary_Matrix) <- ColNames
  
  for(i in seq_len(Number_Errors)){
    
    
    for (j in seq_len(Number_Models)) {
      
      Temporary_Matrix[j,] <- List_Accuracy[[j]][i,1:Number_Predictions]
    }
    
    Weight_Matrix[[i]] <- Temporary_Matrix
  }
  names(Weight_Matrix) <- ListNames
  return(Weight_Matrix)
}
# Islands -----------------------------------------------------------------------

Upper_Limit <- function(x) {
  
  Q1 <- quantile(x,probs = 0.25)
  Q3 <- quantile(x,probs = 0.75)
  IQR <- Q3 - Q1
  Results <- Q3 + 1.5 * IQR
  return(Results)
}

Lower_Limit <-  function(x) {
  
  Q1 <- quantile(x,probs = 0.25)
  Q3 <- quantile(x,probs = 0.75)
  IQR <- Q3 - Q1
  Results <- Q1 - 1.5*IQR
  return(Results)
}

Diff <- function(x) diff(c(head(x,1),x))


Islands <- function(Matrix_Errors, Function_For_Ordered_Vector) {
  
  Number_Predictions <- length(Matrix_Errors[1, ])
  
  Number_Models <- length(Matrix_Errors[,1])
  
  
  for (i in seq_len(Number_Predictions )){
    
    Vector_Errors <- Matrix_Errors[,i]
    
    Outlier_Detected <- 0
    
    ord <- order(Vector_Errors, decreasing = FALSE)
    
    Vector_Errors <- Vector_Errors[order(Vector_Errors, decreasing = FALSE)]
    
    for(j in seq_len(Number_Models)) {
      
      Considered_Vector <-  Diff(head(Vector_Errors,j))
      
      if( Outlier_Detected != 1) {    
        if (Function_For_Ordered_Vector(Considered_Vector) < tail(Considered_Vector,1)){
          
          Outlier_Detected <- 1
          Vector_Errors[[j]] <- Inf
          
        }
      } else{
        Vector_Errors[[j]] <- Inf
      }
      
    }
    
    Vector_Errors <- Vector_Errors[order(ord)]
    
    Matrix_Errors[, i] <- Vector_Errors
    
  }
  return(Matrix_Errors)
}


# Equalise or Keep Weights? -------------------------------------------------------------
Equalise <- function(Matrix_Weights) {
  
  Number_Predictions <- length(Matrix_Weights[1, ])
  Number_Models   <- length(Matrix_Weights[, 1])
  
  
  
  for (i in seq_len(Number_Predictions)) {
    Vector_Weights <- Matrix_Weights[,i]
    
    for (j in  seq_len(Number_Models)) {
      
      if (Vector_Weights[[j]] > 0) {
        Vector_Weights[[j]] <- 1L
      }
      
    }
    
    Matrix_Weights[, i] <- Vector_Weights
  }
  Matrix_Weights <- sweep(Matrix_Weights, 2, colSums(Matrix_Weights), FUN = "/")
  return(Matrix_Weights)
}


Weighted_Average <- function(Matrix_Weights) {
  
  Matrix_Weights <- sweep(Matrix_Weights, 2, colSums(Matrix_Weights), FUN = "/")
  return(Matrix_Weights)
}


# Calculate Out of Sample Errors ------------------------------------------

Multiplication_Forecast <- function(MatrixWeights,Forecast) {
  
  if( ncol(MatrixWeights) > 1) {
    Result <- diag(t(MatrixWeights) %*% Forecast,names = FALSE)
    
  } else {
    
    Result <-  t(MatrixWeights) %*% Forecast 
  }
  
  Result<- 1 %*% Result
  colnames(Result) <- colnames(Forecast)
  rownames(Result) <- "Teste"
  return(Result)
}


Calculate_OS_Errors <- function(f,train,test) {
  error <- -sweep(f, 2, test)  
  pcerror <- (200 * abs(error) / sweep(abs(f), 2, abs(test), FUN = "+"))
  
  scalederror <- (abs(error) / mean(abs(diff(train, lag = frequency(train)))))
  
  Errors <- rbind(Symmetric_Errors = pcerror,
                  Scaled_Errors = scalederror)
  rownames(Errors) <- c("Symmetric_Errors","Scaled_Errors")
  return(Errors)
}



# Better Cross Validations Function and Applied Vector ---------------------------------------
Better_CV <- function (y, forecastfunction, h = 1, window = NULL,Start= 1,Min_Lenght = 0, ...) {
  y <- as.ts(y)
  n <- length(y)
  e <- ts(matrix(NA_real_, nrow = n, ncol = h))
  tsp(e) <- tsp(y)
  for (i in seq_len(n - 1)) {
    fc <- try(suppressWarnings(forecastfunction(subset(y, 
                                                       start = ifelse(i- Start >= 0L & i- Min_Lenght >= 0L, ifelse(is.null(window), 1L, ifelse(i - window >= 
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

Cross_Calculate_Erros <- function(
  y,
  List_Functions,
  h,
  window = NULL,
  Start = 1,
  Min_Lenght = 0
){
  Number_Functions <- length(List_Functions)
  Names <- vector("character",Number_Functions)
  MatrixErrors <- vector("list",Number_Functions)
  for (i in seq_len(Number_Functions)) {
    Names[i] <- names(List_Functions[i])
    MatrixErrors[[i]] <- Better_CV(
      y = y,
      forecastfunction = List_Functions[[i]],
      h = h,
      window = window,
      Start = Start,
      Min_Lenght = Min_Lenght
      
    )
  }
  names(MatrixErrors) <- Names
  return(MatrixErrors)
}







# The M3 Results Function -------------------------------------------------
toc()
# Load Funtions from Testing File

SubsetM3 <- list.filter(M3, "MONTHLY" %in% period & n > 50)


# Do TCC --------------------------------------------------------------
plan(multisession)
tic("Whole Process")
Something <-  future_map(SubsetM3, safely(function(u) {
  
y <- u$x
xx<- u$xx
h <- 18



# Use Forecast Functions --------------------------------------------------

tic()
AR <- function(x, h) {
  forecast(auto.arima(x,
                      #stepwise=FALSE,
                      #approximation=FALSE
  ), h = h)
}

TB <- function(x, h) {
  forecast(tbats(x), h = h)
}

ET <- function(x, h) {
  forecast(ets(x), h = h)
}

NN <- function(x, h) {
  forecast(nnetar(x), h = h)
}

SA <- function(x, h) {
  forecast(stlm(x, method = "arima"), h = h)
}

SE <- function(x, h) {
  forecast(stlm(x, method = "ets"), h = h)
}

TH <- function(x, h) {
  forecast(thetaf(x), h = h)
}

RW <- function(x, h) {
  rwf(x, drift = TRUE, h = h)
}

SN <- function(x, h) {
  forecast(snaive(x), h = h)
}

Forecast_Functions <- list(
  "Auto_Arima"      =     AR,
  "Tbats"           =     TB,
  "ETS"             =     ET,
  "Neural_Network"  =     NN,
  "Seasonal_AR"     =     SA,
  "Seasonal_ETS"    =     SE,
  "Thetha"          =     TH,
  "Randon_Walk"     =     RW,
  "Seasonal_Naive"  =     SN)
toc()

List_Forecasts <- Forecast_Saver(y,Forecast_Functions,h)

Forecasts_Mean <- lapply(List_Forecasts, `[`, c('mean'))

Mean_Forecasts <- matrix(unlist(Forecasts_Mean), nrow = length(Forecasts_Mean), byrow = FALSE)

colnames(Mean_Forecasts) <- paste("h=", 1:length(Mean_Forecasts[1,]), sep = "")
rownames(Mean_Forecasts) <- names(Forecast_Functions)


Good_Start <- ifelse(length(y) -60 >= 0L, length(y) -60,1L)

tic()
List_of_Errors <- Cross_Calculate_Erros(
  y = y,
  List_Functions = Forecast_Functions,
  h = h,
  #window = window,
  Start = Good_Start,
  Min_Lenght = 24
)
toc()


Calculated_Errors <- Calculate_Errors(y,List_Errors = List_of_Errors)


Inverted_Errors <- lapply(Calculated_Errors,lapply,Invert_List_Accuracy)



Weight_Matrix <- lapply(Inverted_Errors,Create_Weight_Matrix)


List_Ranked <- Rank_Maker(Weight_Matrix)


List_Mean_Chosen <- lapply(Weight_Matrix,lapply, Greater_Function_Vector,mean)



Inverted_Matrix <- lapply(Calculated_Errors, Create_Error_Matrix)


Islands_Chosen <- lapply(Inverted_Matrix, lapply,Islands,Upper_Limit)


Islands_Chosen <- lapply(Islands_Chosen,lapply,Invert_List_Accuracy)



List_All_Selections <-
  list(
    Ranked_Position = List_Ranked,
    Vector_Functions = list(Mean_Chosen = List_Mean_Chosen, Island_Chosen = Islands_Chosen)
  )


List_Equalised <- lapply(List_All_Selections,lapply,lapply,lapply,Equalise)

List_Weighted <- lapply(List_All_Selections,lapply,lapply,lapply,Weighted_Average)

Final_List_Weights <- list(Same_Weights = List_Equalised, Weighted_Average = List_Weighted)


List_Weighted <- lapply(Final_List_Weights,lapply,lapply,lapply,lapply,Multiplication_Forecast,Mean_Forecasts)


Bonsai <- lapply(List_Weighted,lapply,lapply,lapply,lapply,Calculate_OS_Errors,y,xx)


return(Bonsai)

})
,.progress = T)

list.filter(Something, "result" %in% period & n > 50)

toc()





