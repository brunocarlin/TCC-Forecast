library(forecast)
library(forecastHybrid)
library(tidyverse)
library(tsibble)
library(Mcomp)
library(rlist)
library(forecTheta)
library(tictoc)


SubsetM3 <- list.filter(M3, "MONTHLY" %in% period & n > 50)

SubsetM31 <- list.filter(M3, "MONTHLY" %in% period & n > 48 & "N1876" %in% sn)

y <- SubsetM3$N1876$x
y <- tail(y,48)
test <- SubsetM3$N1452$xx
h = 18

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

# Create List with Functions ----------------------------------------------

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

length(Forecast_Functions[[1]])                        
seq_len(length(Forecast_Functions))
# Try to save model and CI ------------------------------------------------

Execute_Function <- function(
  y,
  List_Functions,
  h = 1,
  window = NULL,
  Start = 1,
  Min_Lenght = 0,
  ...
){
  
  Number_Functions <- length(List_Functions)
  MatrixErrors <- vector("list",Number_Functions)
  for (i in seq_len(Number_Functions)) {
    
  }
  MatrixErrors[[i]] <- Better_CV(
    y = y,
    Forecast_Function[[i]],
    h = h,
    window = NULL,
    Start = Start,
    Min_Lenght = 0,
    
)
  return(MatrixErrors)
}

OK<- Execute_Function(y,Forecast_Functions,5,1,1,1)

    
# Cross Calculate for h and save error ------------------------------------

tic("Cross Validating All Methods")


tic("AR")
MatrixErrorsAR <- Better_CV(y, AR, h = h,
                            #window = h,
                            Start = ifelse(length(y) -60 >= 0L, length(y) -60,1L),
                            Min_Lenght = 24
)
toc()

tic("TBATS")
MatrixErrorsTB <- Better_CV(y, TB, h = h,
                            #window = h,
                            Start = ifelse(length(y) -60 >= 0L, length(y) -60,1L),
                            Min_Lenght = 24
)
toc()

tic("ETS")
MatrixErrorsET  <- Better_CV(y, ET, h = h,
                              #window = h,
                              Start = ifelse(length(y) -60 >= 0L, length(y) -60,1L),
                              Min_Lenght = 24
)
toc()

tic("Neural Network")
MatrixErrorsNN <- Better_CV(y, NN, h = h,
                            #window = h,
                            Start = ifelse(length(y) -60 >= 0L, length(y) -60,1L),
                            Min_Lenght = 24
)
toc()

tic("Seasonal Ajusted AR")
MatrixErrorsSA <- Better_CV(y, SA, h = h,
                            #window = h,
                            Start = ifelse(length(y) -60 >= 0L, length(y) -60,1L),
                            Min_Lenght = 24
)
toc()

tic("Seasonal Ajusted ETS")
MatrixErrorsSE <- Better_CV(y, SE, h = h,
                            #window = h,
                            Start = ifelse(length(y) -60 >= 0L, length(y) -60,1L),
                            Min_Lenght = 24
)
toc()

tic("Theta")
MatrixErrorsTH <- Better_CV(y, TH, h = h,
                            #window = h,
                            Start = ifelse(length(y) -60 >= 0L, length(y) -60,1L),
                            Min_Lenght = 24
)
toc()

tic("Randow Walk Seaonal")
MatrixErrorsRW <- Better_CV(y, RW, h = h,
                            #window = h,
                            Start = ifelse(length(y) -60 >= 0L, length(y) -60,1L),
                            Min_Lenght = 24
)
toc()

tic("Seaonal Naive")
MatrixErrorsSN <- Better_CV(y, SN, h = h,
                            #window = h,
                            Start = ifelse(length(y) -60 >= 0L, length(y) -60,1L),
                            Min_Lenght = 24
)
toc()
toc()


#forecast1 <- Auto1(y,h)
#forecast2 <- Auto2(y,h)


# Create an object  with the Time Series, CV Accuracy, Forecast   --------

xx<- c(1,2,3,4)

map_dbl(xx,as.numeric)
Index_Example <- rep(1:10000, each = frequency(y), len = length(y))

Index_Example <- Index_Maker(y,12,map_dbl(c(1:1000),sqrt))

Index_Maker <- function(y,Each,Cost_Function = as.numeric) {
  Rep_Sequence <- 1:((length(y)/Each)+1)
  Index_Intermediary1 <- rep(Rep_Sequence, each = Each, len = length(y))
  Index_Intermediary2 <- map_dbl(Index_Intermediary1,Cost_Function)
  sort(Index_Intermediary2, decreasing = TRUE)
}

Index_Example <- Index_Maker(y,12)


Reduced_Importance <- MatrixErrorsSA/Index_Example

Reduce_Importance <- function(Index,errors) 
  errors

#Forecasts1<- cbind(
#  Auto11 = forecast1$mean,
#  Auto22 = forecast2$mean
#)


# "ME,RMSE,MAE,MPE,MAPE,MASE,ACF1" ----------------------------------------

MAPE1<- vapply(MatrixErrorsSN,mape, y = y,numeric(length(y)))
MAPE3<- mape(y,MatrixErrorsSN)
MAPE1 == MAPE3 
MAPE2<- mape(y,MatrixErrors2)

#lapply(MatrixErrors1, rmse)
# CREATE WEIGHT MATRIX ----------------------------------------------------


WInverseError1 <- 1/colMeans(MAPE1, na.rm = TRUE)
WInverseError2 <- 1/colMeans(MAPE2, na.rm = TRUE)

InverseErrors<- cbind(
  AutoMode1Weights =WInverseError1,
  AutoMode2Weights =WInverseError2
  
)

WeightMatrix <- sweep(InverseErrors, 1, rowSums(InverseErrors), FUN = "/")

# Example of Selection Functions Rank ------------------------------------------

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

# Example of Selection Functions Mean -------------------------------------
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

# Equaliser Function ------------------------------------------------------

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








# Compute Errors out of sample --------------------------------------------

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




