library(tidyverse)
library(reshape2)
library(data.table)
Temp <- lapply(SomethingTeste2,"[","error")
Something4 <- Temp %>% simplify_all()

Temp <- lapply(Something4, simplify_all)
Test <- Filter(Negate(function(x) is.null(unlist(x))), Something3)

bad_lengths <- map_lgl(SomethingTeste2, ~is.null(.x$error) == F)

bad_techs <- SomethingTeste2 %>% 
  discard(bad_lengths)
str(Temp)

Temp <- lapply(bad_techs, `[[`, 1)

Results <- as.tibble(melt(Temp))


colnames(Results) <- c("OS_Error_Type","Forecast_Horizon","Resultss","In_Sample_Error","Error_Type","Family_Method","Selection_Method","Weight_Scheme","Original_List")  

Result <- Results %>% filter( OS_Error_Type == "Symmetric_Errors") %>%  group_by(
  OS_Error_Type,
  In_Sample_Error,
  Error_Type,
  Family_Method,
  Selection_Method,
  Weight_Scheme,
) %>% summarise(Tester = mean(Resultss)) %>%  arrange(Tester)


Data1 <- Results %>% filter(Resultss > 7 & OS_Error_Type == "Scaled_Errors" & In_Sample_Error != "AMRE" , Selection_Method == "Ranked_Position")
Data2 <- Results %>% filter(Resultss > 7 & OS_Error_Type == "Scaled_Errors" & In_Sample_Error != "AMRE", Selection_Method == "Vector_Functions")
Data3 <- Results %>% filter(Resultss > 7 & OS_Error_Type == "Scaled_Errors" & In_Sample_Error != "AMRE", Selection_Method == "Islands_Chosen")
Data4 <- Results %>% filter(Resultss > 7 & OS_Error_Type == "Scaled_Errors" & In_Sample_Error != "AMRE")

length(unique(Data$Original_List))

Bad <- unique(Data2$Original_List)
length(Bad)
library(rlist)
library(Mcomp)
SubsetM31 <- list.filter(M3, Bad[i] %in% sn)


ListaB <- list()
for (i in seq_len(length(Bad))) {
  ListaB <- c(ListaB,list.filter(M3, Bad[i] %in% sn))
}


SubsetM3333 <- list.filter(ListaB, "MONTHLY" %in% period & n > 100 & "N2573" %in% sn)

y <- SubsetM3333$N2573$x
xx <- SubsetM3333$N2573$xx
h <- 18

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

library(smooth)
apply(Mean_Forecasts,1,SMAPE,actual = xx)
