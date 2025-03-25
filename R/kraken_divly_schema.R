## Read Kraken ledger and trades and transform to divly compatible format
# https://docs.google.com/spreadsheets/d/1jGvqnK8OxwcjwpobwkP1k4jj8Sk9OHTEO5WgKIBKbTU/

# kraken ledger + trades
krakenlfile <- "data/raw/<kraken_ledger.csv>"
kraken_ledger <- readr::read_csv(krakenlfile)
krakenlfile <- "data/raw/<kraken_trades.csv>"
kraken_trades <- readr::read_csv(krakentfile)

kraken_unified <-
  kraken_ledger |> 
  filter(!refid %in% err) |> 
  filter(type == "trade") |> 
  select(refid, time, type, asset, amount, fee, balance) |> 
  group_by(refid) |>
  summarize(
    date = lubridate::as_date(first(time)),
    time = hms::as_hms(first(as_datetime(time))),
    transaction_type = "Trade",
    label = NA_character_,
    received_currency = first(na.omit(asset[amount > 0])),
    received_amount = sum(amount[amount > 0], na.rm = TRUE),
    sent_currency = first(na.omit(asset[amount < 0])),
    sent_amount = abs(sum(amount[amount < 0], na.rm = TRUE)),
    fee_amount = sum(fee, na.rm = TRUE),
    fee_currency = first(na.omit(asset[fee > 0])),  # From right line
    tx_hash = first(refid),
    custom_description = NA_character_,
    .groups = "drop"
  ) |> 
  select(
    date, time, transaction_type, label, sent_amount, sent_currency,
    received_amount, received_currency, fee_amount, fee_currency, tx_hash, 
    custom_description
  ) |> 
  bind_rows(
    kraken_ledger |> 
      filter(type %in% c("deposit", "withdrawal")) |> 
      select(refid, time, type, subtype, asset, amount, fee, balance) |>
      transmute(
        date =  time |> lubridate::as_date(),
        time =  time |> as_datetime() |> hms::as_hms(),
        transaction_type = 
          case_when(
            type == "deposit" ~ "Deposit",
            type == "withdrawal" ~ "Withdrawal",
            .default = "Unknown"
          ),
        sent_amount =
          case_when(
            type == "deposit" ~ NA_real_,
            type == "withdrawal" ~ abs(amount),
            .default = NA_real_,
          ),
        sent_currency = 
          case_when(
            type == "deposit" ~ NA_character_,
            type == "withdrawal" ~ asset,
            .default = NA_character_,
          ),
        received_amount = 
          case_when(
            type == "deposit" ~ amount,
            type == "withdrawal" ~ NA_real_,
            .default = NA_real_
          ),
        received_currency = 
          case_when(
            type == "deposit" ~ asset,
            type == "withdrawal" ~ NA_character_,
            .default = NA_character_
          ),
        fee_amount = if_else(fee>0, fee, NA_real_),
        fee_currency = if_else(fee>0, asset, NA_character_),
        tx_hash = refid,
        custom_description = NA_character_
      )
  ) |> 
  bind_rows(
    kraken_ledger |> 
      filter(type == "staking") |> 
      select(refid, time, type, asset, amount, fee, balance) |>
      transmute(
        date =  time |> lubridate::as_date(),
        time =  time |> as_datetime() |> hms::as_hms(),
        transaction_type = "Deposit",
        label = "Staking Reward",
        sent_amount = NA_real_,
        sent_currency = NA_character_,
        received_amount = amount,
        received_currency = asset,
        fee_amount = NA_real_,
        fee_currency = NA_character_,
        tx_hash = refid,
        custom_description = NA_character_
      )
  ) |> 
  arrange(date, time)

krkn_csv_file <- "data/divly/kraken.divly.csv"
kraken_unified |> write_csv(krkn_csv_file, na="")