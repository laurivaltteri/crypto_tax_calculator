## Generic trades and transform to divly compatible format
# https://docs.google.com/spreadsheets/d/1jGvqnK8OxwcjwpobwkP1k4jj8Sk9OHTEO5WgKIBKbTU/

trades_from_x <-
  tribble(
    ~date, ~time, ~transaction_type, ~label, ~sent_amount, ~sent_currency, ~received_amount, ~received_currency, ~fee_amount, ~fee_currency, ~tx_hash, ~custom_description,
    "2017-02-16", "09:30:00", "Withdrawal", NA, 70500, "XRP", NA, NA, NA, "NA", "F601C1F11C513C5915485011AABAA73AC2EF1E5588EBEDE622209FE979EA8914", NA, 
    "2017-02-10", "10:11:00", "Withdrawal", NA, 59.82, "ETH", NA, NA, 0.00042, "ETH", "0x59449fdc428d3a858cce423d8a1107f1f44b358b0bbf486670e3229360bbdeb9", NA,
    "2017-02-09", "09:32:00", "Trade", NA, 40, "ETH", 70511.893706, "XRP", 0.01, "XRP", "2DA7C44CC639762232B063173D9F86173F3F385BF2F7A9AC9A2F3D45F4950DAE", NA,
    "2022-02-07", "00:10:33", "Deposit", NA, NA, NA, 99.83518, "ETH", NA, NA, "0xc04d2a127d012bee7f250b263da0cb0c6110fe8f9ef97aee73d133be6dd9c797", NA
  )

generic_csv_file <- "data/divly/<some>.divly.csv"
trades_from_x |> write_csv(generic_csv_file, na="")
