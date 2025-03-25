## Read closed Binance account and transform to .divly.csv
# https://docs.google.com/spreadsheets/d/1jGvqnK8OxwcjwpobwkP1k4jj8Sk9OHTEO5WgKIBKbTU/

library(tidyverse)

exl_file <- "data/raw/<binance.xlsx>"

# collect the sheets
deposits <- readxl::read_xlsx(exl_file, sheet = "Deposit History") 

withdrawals <- 
  readxl::read_xlsx(exl_file, sheet = "Withdrawal History") |> 
  filter(Currency != "DATA")

trades <- 
  readxl::read_xlsx(exl_file, sheet = "Order History") |> 
  filter(`Trade Qty`>0.01)

card_action <- 
  readxl::read_xlsx(exl_file, sheet = "Binance Card transactions")  |> 
  dplyr::filter(Type=="capture") 

trades_div <-
  trades |> 
  mutate(
    received_currency = if_else(Side=="BUY", `Amount Unit`, `Price Unit`),
    sent_currency = if_else(Side=="BUY", `Price Unit`, `Amount Unit`),
    received_amount = if_else(Side=="BUY", `Trade Qty`, `Average Price`*`Trade Qty`),
    sent_amount = if_else(Side=="BUY", `Average Price`*`Trade Qty`, `Trade Qty`)
  )

# create compatible transactions table
binance_trades <- 
  tibble(
    date =  c(deposits$`Create Time`, withdrawals$`Apply Time`, trades_div$Time) |> lubridate::as_date(),
    time =  c(deposits$`Create Time`, withdrawals$`Apply Time`, trades_div$Time) |> lubridate::as_datetime() |> hms::as_hms(),
    transaction_type = c(rep("Deposit", nrow(deposits)), rep("Withdrawal", nrow(withdrawals)), rep("Trade", nrow(trades_div))),
    label = c(rep(NA, nrow(deposits)), rep(NA, nrow(withdrawals)), trades_div$`Market ID`),
    sent_amount = c(rep(NA, nrow(deposits)), withdrawals$Amount, trades_div$sent_amount),
    sent_currency = c(rep(NA, nrow(deposits)), withdrawals$Currency, trades_div$sent_currency),
    received_amount = c(deposits$Amount, rep(NA, nrow(withdrawals)), trades_div$received_amount),
    received_currency = c(deposits$Currency, rep(NA, nrow(withdrawals)), trades_div$received_currency),
    fee_amount = NA,
    fee_currency = NA,
    tx_hash = c(deposits$TXID, withdrawals$txId, rep(NA, nrow(trades_div))),
    custom_description = c(deposits$`Account Type`, withdrawals$`Account Type`, rep(NA, nrow(trades_div)))
  ) |> 
  # arrange transactions
  arrange(date, time) |> 
  rename(`time (UTC)` = time)

# save to file
csv_file <- "data/divly/binance.divly.csv"
binance_trades |> write_csv(csv_file, na="")