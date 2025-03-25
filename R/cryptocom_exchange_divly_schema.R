## Read crypto.com exchange and transform to divly compatible format
# https://docs.google.com/spreadsheets/d/1jGvqnK8OxwcjwpobwkP1k4jj8Sk9OHTEO5WgKIBKbTU/

depo <- "data/raw/<cryptoexchange-deposit-withdrawal.csv>"

exchange_dw <- 
  readr::read_csv(depo1, skip=4) |> 
  bind_rows(readr::read_csv(depo2, skip=4)) |> 
  janitor::clean_names() |> 
  arrange(time) |> 
  mutate(org_time =  time |> str_extract("^.*\\((.*)\\)$",1) |> lubridate::as_datetime()) |> 
  transmute(
    date =  org_time |> lubridate::as_date(),
    time =  org_time |> as_datetime() |> hms::as_hms(),
    transaction_type = type,
    label = status,
    sent_amount = if_else(type=="Deposit", NA, quantity),
    sent_currency = if_else(type=="Deposit", NA, coin),
    received_amount = if_else(type=="Deposit", quantity, NA),
    received_currency = if_else(type=="Deposit", coin, NA),
    fee_amount = NA,
    fee_currency = NA,
    tx_hash = txid,
    custom_description = information
  )

exc_csv_file <- "data/divly/cryptocom_exchange.divly.csv"
exchange_dw |> write_csv(exc_csv_file, na="")