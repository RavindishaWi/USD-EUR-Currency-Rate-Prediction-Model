knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(tidymodels)
library(readxl)
library(tidyverse)
library(neuralnet)
library(lubridate)
library(zoo)

exchange_usd <- read_excel("ExchangeUSD.xlsx") %>%
  janitor::clean_names() %>%
  mutate(date_in_ymd = ymd(yyyy_mm_dd)) %>%
  select(-1) %>%
  select(date_in_ymd,everything())

exchange_usd_full = exchange_usd %>%
  mutate(prev_one_day_set_a <- lag(exchange_usd$usd_eur,1),
         prev_one_day_set_b <- lag(exchange_usd$usd_eur,1),
         prev_two_day_set_b <- lag(exchange_usd$usd_eur,2),
         prev_one_day_set_c <- lag(exchange_usd$usd_eur,1),
         prev_two_day_set_c <- lag(exchange_usd$usd_eur,2),
         prev_three_day_set_c <- lag(exchange_usd$usd_eur,3),
         prev_one_day_set_d <- lag(exchange_usd$usd_eur,1),
         prev_two_day_set_d <- lag(exchange_usd$usd_eur,2),
         five_day_rolling <- rollmean(usd_eur,5, fill = NA),
         ten_day_rolling <- rollmean(usd_eur,10, fill = NA)) %>%
  drop_na()

exchange_usd_full %>%
  pivot_longer(cols = 3,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "First Input Variables Set") +
  theme(legend.position = "none")

exchange_usd_full %>%
  pivot_longer(cols = c(4,5),names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Second Input Variables Set") +
  theme(legend.position = "none")

exchange_usd_full %>%
  pivot_longer(cols = 6:8,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Third Input Variables Set") +
  theme(legend.position = "none")

exchange_usd_full %>%
  pivot_longer(cols = 9:12,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Fourth Input Variables Set") +
  theme(legend.position = "none")

normalization <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

normalized_exchange_usd = exchange_usd_full %>%
  mutate(across(3:13, ~normalization(.x)))
summary(normalized_exchange_usd)

set.seed(123)
train_usd_exchange <- normalized_exchange_usd[1:400,]
test_usd_exchange <- normalized_exchange_usd[401:491,]

unnormalization <- function(x, min, max) {
  return( (max - min)*x + min ) }

min_train_exchange_usd <- min(exchange_usd_full[1:400,3])
max_train_exchange_usd <- max(exchange_usd_full[1:400,3])

min_train_exchange_usd
max_train_exchange_usd

min_test_exchange_usd <- min(exchange_usd_full[401:491,3])
max_test_exchange_usd <- max(exchange_usd_full[401:491,3])

min_test_exchange_usd
max_test_exchange_usd


relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}

relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}

relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}

set.seed(12345)

model_hidden_layers2 <- function(hidden,sec_hidden) {
  nn_model_true <- neuralnet(usd_eur ~ prev_one_day_set_a, data = train_usd_exchange, hidden=c(
    hidden,sec_hidden), linear.output=TRUE)
  training_results <- compute(nn_model_true, test_usd_exchange[,2:3])
  truthcol <- exchange_usd_full[401:491,3]$usd_eur
  predcol <- unnormalization(training_results$net.result, min_train_exchange_usd, max_train_exchange_usd)[,1]
  relevant_pred_stat(truthcol,predcol, "Two Hidden Layers") %>%
    mutate(hiddel_layers = paste0(hidden, " and ",sec_hidden),
           input_set = "A") %>%
    filter(.metric != "rsq")
}

results_hidden_layers2 <- bind_rows(
  lapply(1:10, function(n) {
    bind_rows(
      lapply(1:5, function(m) {
        model_hidden_layers2(n,m)
      })
    )
  })) %>%
  janitor::clean_names()
set_a_models_layers2 <- results_hidden_layers2 %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(set_a_models_layers2[1:10,])


# Combine dataframes
set_models_a <- rbind(set_models_a,set_a_models_layers2)
