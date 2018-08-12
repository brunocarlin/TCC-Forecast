library(forecast)
library(forecastHybrid)
library(tidyverse)
library(tsibble)
library(Mcomp)
library(rlist)

cvts
SubsetM3 <- list.filter(M3, "MONTHLY" %in% period & n > 48)

SubsetM31 <- list.filter(M3, "MONTHLY" %in% period & n > 48 & "N1402" %in% sn)

y <- SubsetM3$N1452$x
test <- SubsetM3$N1452$xx
h = 18

# Use Forecast Functions --------------------------------------------------



A <- function(x, h) {
  forecast(auto.arima(x), h = h)
}

T <- function(x, h, ...) {
  forecast(tbats(x), h = h)
}

E <- function(x, h) {
  forecast(ets(x), h = h)
}

N <- function(x, h) {
  forecast(nnetar(x), h = h)
}

SA <- function(x, h) {
  forecast(stlm(y, modelfunction = ar), h = h)
}

SE <- function(x, h) {
  forecast(stlm(y, modelfunction = ets), h = h)
}

E <- function(x, h) {
  forecast(thetaf(x), h = h)
}


# Create List with Functions ----------------------------------------------


AutoFunctiona <- c(Auto1,Auto2,Auto3)




# Try to save model and CI ------------------------------------------------

ForecastModel1 <- auto.arima(y)
ForecastModel1$model
  # Cross Calculate for h and save error ------------------------------------
MatrixErrors1 <- tsCV(y,N, h =h, window = h)
MatrixErrors2 <- tsCV(y,Auto3, h =h, window = h)

forecast1 <- Auto1(y,h)
forecast2 <- Auto2(y,h)

Forecasts1<- cbind(
  Auto11 = forecast1$mean,
  Auto22 = forecast2$mean
)
# Create an object  with the Time Series, CV Accuracy, Forecast   --------





# "ME,RMSE,MAE,MPE,MAPE,MASE,ACF1" ----------------------------------------

MAPE1<- vapply(MatrixErrors1,mape, y = y,numeric(length(y)))
MAPE3<- mape(y,MatrixErrors1)
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




