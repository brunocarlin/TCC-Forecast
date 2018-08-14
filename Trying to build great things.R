# Load Libraries, Create Status ----------------------------------------------------------


library(forecast)
library(forecastHybrid)
library(tidyverse)
library(tsibble)
library(Mcomp)
library(rlist)
library(forecTheta)
library(tictoc)

Ok <- print("Feito")
Bad <- print("Doesn't Work")
Works <- print("Justs Works")

Ok # Get Some date, define h and y -------------------------------------------


SubsetM3 <- list.filter(M3, "MONTHLY" %in% period & n > 50)

SubsetM31 <- list.filter(M3, "MONTHLY" %in% period & n > 48 & "N1876" %in% sn)

y <- SubsetM3$N1876$x
h <- 2

Ok # Use Forecast Functions --------------------------------------------------



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

Works # Create List with Functions ----------------------------------------------

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

Forecast_Functions2 <- list(
  "Auto_Arima"      =     RW,
  "Tbats"           =     SN,
  "ETS"             =     RW,
  "Neural_Network"  =     SN,
  "Seasonal_AR"     =     RW,
  "Seasonal_ETS"    =     SN,
  "Thetha"          =     RW,
  "Randon_Walk"     =     SN,
  "Seasonal_Naive"  =     RW)

length(Forecast_Functions[[1]])                        
seq_len(length(Forecast_Functions))
Ok # Try to Save forecasts calls ------------------------------------------------

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

List_Forecasts <- Forecast_Saver(y,Forecast_Functions2,h)


Works # Cross Calculate for h and save errors ------------------------------------
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

Good_Start <- ifelse(length(y) -60 >= 0L, length(y) -60,1L)


List_Errors <- Cross_Calculate_Erros(
  y = y,
  List_Functions = Forecast_Functions2,
  h = h,
  #window = window,
  Start = Good_Start,
  Min_Lenght = 24
)

#forecast1 <- Auto1(y,h)
#forecast2 <- Auto2(y,h)


Ok # Create an object  with the Time Series, CV Accuracy, Forecast   --------

List_Forecasts_Errors <- list(Errors = List_Errors,Calls = List_Forecasts)


Works # Use some kind of cost to older errors -----------------------------------


Index_Maker <- function(y,Each,Cost_Function = as.numeric) {
  Rep_Sequence <- 1:((length(y)/Each)+1)
  Index_Intermediary1 <- rep(Rep_Sequence, each = Each, len = length(y))
  Index_Intermediary2 <- map_dbl(Index_Intermediary1,Cost_Function)
  sort(Index_Intermediary2, decreasing = TRUE)
}

#Index_Example <- Index_Maker(y,frequency(y))
# Reduced_Importance <- List_Forecasts_Errors[["Errors"]][["Auto_Arima"]]/Index_Example

Works# "ME,RMSE,MAE,MPE,MAPE,MASE" ----------------------------------------


Calculated_Errors <-
  function(y,List_Errors) 
  {
    
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

Calc <- Calculated_Errors(y,List_Errors = List_Errors)

Bad# CREATE WEIGHT MATRIX ----------------------------------------------------

Inverse_Function <- function(Accuracy) {
  1/Accuracy 
}

Inverted_Errors_CV <- map(Calc[["CV_Error_Metrics"]],Inverse_Function)

Inverted_Errors_Mean <- map(Calc[["Mean_Error_Metrics"]],Inverse_Function)


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

Weight_Matrix_CV <- Create_Weight_Matrix(Inverted_Errors_CV)

Weight_Matrix_Mean <- Create_Weight_Matrix(Inverted_Errors_Mean)

WeightMatrix <- sweep(InverseErrors, 1, rowSums(InverseErrors), FUN = "/")

Works# Example of Selection Functions Rank ------------------------------------------

Rank <- function(Matrix_Weights, Position) {
  
  Number_Models <- length(Matrix_Weights[1, ])
  
  for (i in 1 : length(Matrix_Weights[,1])){
    
    Vector_Weights <- Matrix_Weights[i,]
    
    ord <- order(Vector_Weights, decreasing = T)
    
    Vector_Weights <- Vector_Weights[order(Vector_Weights, decreasing = TRUE)]
    
    Vector_Weights[(Position + 1 ):Number_Models] <- 0
    
    Vector_Weights <- Vector_Weights[order(ord)]
    
    Matrix_Weights[i, ] <- Vector_Weights
    
  }
  return(Matrix_Weights)
}

#Ranked <- Rank(WeightMatrix, 1)

#Ranked_Weight <- sweep(Ranked, 1, rowSums(Ranked), FUN = "/")

Works# Example of Selection Functions Mean -------------------------------------
#WeightMatrix2 <- rbind(c(0.45,0.45,0.1),c(0.7,0.2,0.1),c(0.2,0.1,0.7))
#mean(WeightMatrix2[3,])
Greater_Value <- function(Matrix_Weights,Min_Value,If_Lower_Average = FALSE) {
  
  Number_Models <- seq_along(Matrix_Weights[1,])
  Number_Models_Actual <- length(Matrix_Weights[1,])
  Number_Errors_H   <- seq_along(Matrix_Weights[, 1])
  
  for (i in Number_Errors_H) {
    Vector_Weights <- Matrix_Weights[i, ]
    
    for (j in  Number_Models) {
      
      if (Min_Value > Vector_Weights[[j]]) {
        Vector_Weights[[j]] <- 0
      }
       
    
    if(mean(Vector_Weights) != 0) {
    Matrix_Weights[i, ] <- Vector_Weights
  } else {
    
    if (If_Lower_Average == TRUE) {
      for (j in  Number_Models) {
        Vector_Weights[[j]] <- 1/Number_Models_Actual
      }
    }
  
  }
   Matrix_Weights[i, ] <-  Vector_Weights
    }
  }
 return(Matrix_Weights)
}

#Greater1 <- Greater_Value(WeightMatrix2,0.9,TRUE)

Bad# Equaliser Function ------------------------------------------------------

Equaliser <- function(Matrix_Weights) {
  
  Number_Models <- seq_along(Matrix_Weights[1,])
  Number_Errors_H   <- seq_along(Matrix_Weights[, 1])
  
  for (i in Number_Errors_H) {
    Vector_Weights <- Matrix_Weights[i, ]
    
    for (j in  Number_Models) {
      
      if (Vector_Weights[[j]] > 0) {
        Vector_Weights[[j]] <- 1
      }
      
    }
    
    Matrix_Weights[i, ] <- Vector_Weights
  }
  return(Matrix_Weights)
}

#Equalised_Weight <- sweep(Equi, 1, rowSums(Equi), FUN = "/")








Works# Compute Errors out of sample --------------------------------------------

train <- y
TestResult1<- rowSums(WeightMatrix * Forecasts1)
TestResult2 <- TestResult1/0.95

f<- rbind(TestResult1,TestResult2)

error <- -sweep(f, 2, test)
pcerror <- (200 * abs(error) / sweep(abs(f), 2, abs(test), FUN = "+")) %>%
  as_tibble() %>%
  mutate(Method = rownames(f)) %>%
  gather(key = h, value = sAPE, -Method)
scalederror <- (abs(error) / mean(abs(diff(train, lag = frequency(train))))) %>%
  as_tibble() %>%
  mutate(Method = rownames(f)) %>%
  gather(key = h, value = ASE, -Method)
LiSTA<- list(pcerror = pcerror, scalederror = scalederror)




