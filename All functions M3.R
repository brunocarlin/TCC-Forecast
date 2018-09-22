benchmarks <- function(y, h) {
  require(forecast)
  # Compute four benchmark methods
  fcasts <- rbind(
    N = snaive(y, h)$mean,
    R = rwf(y,h)$mean,
    E = forecast(ets(y), h)$mean,
    A = forecast(auto.arima(y), h)$mean,
    T = thetaf(y, h)$mean,
    M = forecast(nnetar(y),h)$mean,
    B = forecast(tbats(y),h)$mean,
    S = forecast(stlm(y, method = "ets"),h)$mean,
    W = forecast(stlm(y, method = "arima"),h)$mean)
  
  colnames(fcasts) <- seq(h)
  method_names <- rownames(fcasts)
  # Compute all possible combinations
  method_choice <- rep(list(0:1), length(method_names))
  names(method_choice) <- method_names
  combinations <- expand.grid(method_choice) %>% tail(-1) %>% as.matrix()
  # Construct names for all combinations
  for (i in seq(NROW(combinations))) {
    rownames(combinations)[i] <- paste0(method_names[which(combinations[i, ] > 0)], 
                                        collapse = "")
  }
  # Compute combination weights
  combinations <- sweep(combinations, 1, rowSums(combinations), FUN = "/")
  # Compute combinations of forecasts
  return(combinations %*% fcasts)
}


library(Mcomp)
library(tidyverse)
library(furrr)
library(rlist)
# Compute "symmetric" percentage errors and scaled errors
M3 <- list.filter(M3,h == 18)
plan(multisession)
errors <- future_map(M3, function(u) {
  train <- u$x
  test <- u$xx
  f <- benchmarks(train, u$h)
  error <- -sweep(f, 2, test)
  pcerror <- (200 * abs(error) / sweep(abs(f), 2, abs(test), FUN = "+")) %>%
    as_tibble() %>%
    mutate(Method = rownames(f)) %>%
    gather(key = h, value = sAPE, -Method)
  scalederror <- (abs(error) / mean(abs(diff(train, lag = frequency(train))))) %>%
    as_tibble() %>%
    mutate(Method = rownames(f)) %>%
    gather(key = h, value = ASE, -Method)
  return(list(pcerror = pcerror, scalederror = scalederror))
})
# Construct a tibble with all results

M3errors <- tibble(
  Series = names(M3),
  Period = map_chr(M3, "period"),
  se = map(errors, "scalederror"),
  pce = map(errors, "pcerror")) %>%
  unnest() %>%
  select(-h1, -Method1) %>%
  mutate(h = as.integer(h), 
         Period = factor(str_to_title(Period), 
                         levels = c("Monthly","Quarterly","Yearly","Other")))


accuracy <- M3errors %>%
  group_by(Period, Method, h) %>%
  summarize(MASE=mean(ASE), sMAPE=mean(sAPE)) %>%
  ungroup()

original_methods <- unique(accuracy[["Method"]])
original_methods <- original_methods[str_length(original_methods)==1L]
# Compute summary table of accuracy measures
accuracy_table <- accuracy %>%
  group_by(Method,Period) %>%
  summarise(
    sMAPE = mean(sMAPE, na.rm = TRUE),
    MASE = mean(MASE, na.rm = TRUE) ) %>%
  arrange(sMAPE) %>% ungroup()
best <- accuracy_table %>% filter(MASE==head(MASE,25))
accuracy_table <- accuracy_table %>%
  filter(Method %in% original_methods | Method %in% best$Method) %>%
  arrange(Period, MASE) %>%
  select(Method, sMAPE, MASE) %>% 
  arrange(sMAPE)
knitr::kable(accuracy_table)


