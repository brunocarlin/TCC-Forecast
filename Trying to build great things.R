# Load Libraries, Create Status ----------------------------------------------------------

library(forecast)
library(forecastHybrid)
library(tidyverse)
library(tsibble)
library(Mcomp)
library(rlist)
library(forecTheta)
library(tictoc)
library(reshape2)

Ok <- print("Feito")
Bad <- print("Doesn't Work")
Works <- print("Justs Works")

Ok # Get Some date, define h and y -------------------------------------------


SubsetM3 <- list.filter(M3, "MONTHLY" %in% period & n > 50)

SubsetM31 <- list.filter(M3, "MONTHLY" %in% period & n > 48 & "N1876" %in% sn)

y <- SubsetM3$N1876$x
xx<- SubsetM3$N1876$xx
h <- 18

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

List_Forecasts <- Forecast_Saver(y,Forecast_Functions,h)

Forecasts_Mean <- lapply(List_Forecasts, `[`, c('mean'))

Mean_Forecasts <- matrix(unlist(Forecasts_Mean), nrow = length(Forecasts_Mean), byrow = FALSE)

colnames(Mean_Forecasts) <- paste("h=", 1:length(Mean_Forecasts[1,]), sep = "")
rownames(Mean_Forecasts) <- names(Forecast_Functions)

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
#forecast1 <- Auto1(y,h)
#forecast2 <- Auto2(y,h)


Works # Create an object  with the Time Series, CV Accuracy, Forecast   --------

# List_Forecasts_Errors <- list(Errors = List_Errors,Calls = List_Forecasts)


Works # Use some kind of cost to older errors -----------------------------------


 #Index_Maker <- function(y,Each,Cost_Function = as.numeric) {
 # Rep_Sequence <- 1:((length(y)/Each)+1)
 # Index_Intermediary1 <- rep(Rep_Sequence, each = Each, len = length(y))
 # Index_Intermediary2 <- map_dbl(Index_Intermediary1,Cost_Function)
 # sort(Index_Intermediary2, decreasing = TRUE)
#}

#Index_Example <- Index_Maker(y,frequency(y))
# Reduced_Importance <- List_Forecasts_Errors[["Errors"]][["Auto_Arima"]]/Index_Example

Works # "ME,RMSE,MAE,MPE,MAPE,MASE" ----------------------------------------


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

Calculated_Errors <- Calculate_Errors(y,List_Errors = List_of_Errors)

Works # CREATE WEIGHT MATRIX ----------------------------------------------------

Invert_List_Accuracy <- function(Accuracy) {
    1/Accuracy 

}

Inverted_Errors <- lapply(Calculated_Errors,lapply,Invert_List_Accuracy)

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

Weight_Matrix <- lapply(Inverted_Errors,Create_Weight_Matrix)


Ok # Example of Selection Functions Rank ------------------------------------------

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

List_Ranked <- Rank_Maker(Matrix_Weights)


Works # Example of Selection Functions Specif Value -------------------------------------

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


List_Mean_Chosen <- lapply(Weight_Matrix,lapply, Greater_Function_Vector,mean)
#List_Median_Chosen <- lapply(Weight_Matrix_Both,lapply, Greater_Function_Vector,median)

#Quantile_25 <- function(x) { quantile(x,probs = 0.25)}
#Quantile_75 <- function(x) { quantile(x,probs = 0.75)}

#List_Quantile_25 <- map(Weight_Matrix_Both,lapply, Greater_Function_Vector,Quantile_25)
#List_Quantile_75 <- map(Weight_Matrix_Both,lapply, Greater_Function_Vector,Quantile_75)



Ok # Extension by using Islands Idea --------------------------------------------
#Another look at forecast selection and combination:
#evidence from forecast pooling

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

Inverted_Matrix <- lapply(Calculated_Errors, Create_Error_Matrix)

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

Islands_Chosen <- lapply(Inverted_Matrix, lapply,Islands,Upper_Limit)

Islands_Chosen <- lapply(Islands_Chosen,lapply,Invert_List_Accuracy)

Bad # Maybe Combine Weights from different error methods ----------------------



Works # Combine All Lists -------------------------------------------------------

List_All_Selections <-
  list(
    Ranked_Position = List_Ranked,
    Vector_Functions = list(Mean_Chosen = List_Mean_Chosen, Island_Chosen = Islands_Chosen)
  )


Ok# Equalise or Calculate Weights Functions ------------------------------------------------------

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

List_Equalised <- lapply(List_All_Selections,lapply,lapply,lapply,Equalise)


Weighted_Average <- function(Matrix_Weights) {

  Matrix_Weights <- sweep(Matrix_Weights, 2, colSums(Matrix_Weights), FUN = "/")
  return(Matrix_Weights)
}

List_Weighted <- lapply(List_All_Selections,lapply,lapply,lapply,Weighted_Average)

Final_List_Weights <- list(Same_Weights = List_Equalised, Weighted_Average = List_Weighted)

Bad# Combine All Weight With the forecasts -----------------------------------
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

List_Weighted <- lapply(Final_List_Weights,lapply,lapply,lapply,lapply,Multiplication_Forecast,Mean_Forecasts)

Test <- melt(List_Weighted)
Ok# Compute Errors out of sample --------------------------------------------


train <- y

test <- xx

Calculate_OS_Errors <- function(f,train,test) {
  error <- -sweep(f, 2, test)  
  pcerror <- (200 * abs(error) / sweep(abs(f), 2, abs(test), FUN = "+"))
  
  scalederror <- (abs(error) / mean(abs(diff(train, lag = frequency(train)))))
  
  Errors <- rbind(Symmetric_Errors = pcerror,
                  Scaled_Errors = scalederror)

  return(Errors)
}

Bonsai <- lapply(List_Weighted,lapply,lapply,lapply,lapply,Calculate_OS_Errors,y,xx)

Leafs <- melt(Bonsai)



Bad # Loop and do something with results --------------------------------------





