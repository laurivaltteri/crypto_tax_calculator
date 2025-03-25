## Read Crypto.com app transactions and transform to divly compatible format
# https://docs.google.com/spreadsheets/d/1jGvqnK8OxwcjwpobwkP1k4jj8Sk9OHTEO5WgKIBKbTU/

# divly compatible crypto.com
cryptocomfile <- "data/raw/<crypto_transactions_record.csv>"

crypto_ledger <- 
  readr::read_csv(cryptocomfile) |> 
  janitor::clean_names()

# trades
trades_ccom <- 
  crypto_ledger |> 
  filter(transaction_kind %in% c("crypto_viban_exchange", "crypto_exchange", "card_top_up", "viban_purchase")) |> 
  transmute(
    date =  timestamp_utc |> lubridate::as_date(),
    time =  timestamp_utc |> hms::as_hms(),
    transaction_type = "Trade",
    label = NA_character_,
    sent_amount = abs(amount),
    sent_currency = currency,
    received_amount = coalesce(to_amount, abs(native_amount)),
    received_currency = coalesce(to_currency, native_currency),
    fee_amount = NA_real_,
    fee_currency = NA_character_,
    tx_hash = transaction_hash,
    custom_description = transaction_description
  )

#others
withdrawals_ccom <- 
  crypto_ledger |> 
  filter(str_detect(transaction_kind,"withdrawal")) |> 
  transmute(
    date =  timestamp_utc |> lubridate::as_date(),
    time =  timestamp_utc |> hms::as_hms(),
    transaction_type = "Withdrawal",
    label = NA_character_,
    sent_amount = amount,
    sent_currency = currency,
    received_amount = NA_real_,
    received_currency = NA_character_,
    fee_amount = NA_real_,
    fee_currency = NA_character_,
    tx_hash = transaction_hash,
    custom_description = transaction_description
  )

deposits_ccom <-
  crypto_ledger |>
  filter(str_detect(transaction_kind, "cashback|reimbursement")) |> 
  transmute(
    date =  timestamp_utc |> lubridate::as_date(),
    time =  timestamp_utc |> hms::as_hms(),
    transaction_type = "Deposit",
    label = "Cashback",
    sent_amount = NA_real_,
    sent_currency = NA_character_,
    received_amount = amount,
    received_currency = currency,
    fee_amount = NA_real_,
    fee_currency = NA_character_,
    tx_hash = transaction_hash,
    custom_description = transaction_description
  ) |> 
  filter(received_amount>0) |> 
  bind_rows(
    crypto_ledger |>
      filter(str_detect(transaction_kind, "crypto_deposit")) |> 
      transmute(
        date =  timestamp_utc |> lubridate::as_date(),
        time =  timestamp_utc |> hms::as_hms(),
        transaction_type = "Deposit",
        label = NA_character_,
        sent_amount = NA_real_,
        sent_currency = NA_character_,
        received_amount = amount,
        received_currency = currency,
        fee_amount = NA_real_,
        fee_currency = NA_character_,
        tx_hash = transaction_hash,
        custom_description = transaction_description
      )
  ) |> 
  bind_rows(
    crypto_ledger |> 
      filter(transaction_kind=="mco_stake_reward") |> 
      transmute(
        date =  timestamp_utc |> lubridate::as_date(),
        time =  timestamp_utc |> hms::as_hms(),
        transaction_type = "Deposit",
        label = "Staking Reward",
        sent_amount = NA_real_,
        sent_currency = NA_character_,
        received_amount = amount,
        received_currency = currency,
        fee_amount = NA_real_,
        fee_currency = NA_character_,
        tx_hash = transaction_hash,
        custom_description = transaction_description
      )
  ) |> 
  bind_rows(
    crypto_ledger |> 
      filter(transaction_kind=="exchange_to_crypto_transfer") |> 
      filter(timestamp_utc > "2021-01-01") |> # only use the early swap bonus so exchange data doesn't need to be in the data at all
      transmute(
        date =  timestamp_utc |> lubridate::as_date(),
        time =  timestamp_utc |> hms::as_hms(),
        transaction_type = "Deposit",
        label = "Received Gift",
        sent_amount = NA_real_,
        sent_currency = NA_character_,
        received_amount = amount,
        received_currency = currency,
        fee_amount = NA_real_,
        fee_currency = NA_character_,
        tx_hash = transaction_hash,
        custom_description = "EARLY_SWAP_BONUS_DEPOSIT"
      )
  )

swap_ccom <-
  crypto_ledger |> 
  filter(transaction_description =="MCO/CRO Overall Swap") |> 
  summarize(
    date = lubridate::as_date(first(timestamp_utc)),
    time = hms::as_hms(first(as_datetime(timestamp_utc))),
    transaction_type = "Trade",
    label = NA_character_,
    received_currency = first(na.omit(currency[amount > 0])),
    received_amount = sum(amount[amount > 0], na.rm = TRUE),
    sent_currency = first(na.omit(currency[amount < 0])),
    sent_amount = abs(sum(amount[amount < 0], na.rm = TRUE)),
    fee_amount = NA_real_,
    fee_currency = NA_character_,
    tx_hash = NA_character_,
    custom_description = first(transaction_description)
  ) 

ccom_transactions <-
  bind_rows(
    trades_ccom,
    swap_ccom,
    deposits_ccom,
    withdrawals_ccom
  ) |> 
  arrange(date, time)


ccom_csv_file <- "data/divly/cryptocom_app.divly.csv"
ccom_transactions |> write_csv(ccom_csv_file, na="")