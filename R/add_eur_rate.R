#' Add EUR Value to Crypto-Crypto Transactions
#'
#' This function adds the EUR conversion rate to crypto-to-crypto transactions based on a combination 
#' of manual rates, a preset table for NANO, Yahoo Finance data, and Kraken API data. 
#' Non-trade transactions and non-crypto-crypto rows are carried through without modification.
#'
#' @param transactions A data frame containing cryptocurrency transaction data. Must include the following columns:
#'   - `date` – Date of the transaction.
#'   - `time` – Time of the transaction.
#'   - `sent_currency` – The currency that was sent.
#'   - `received_currency` – The currency that was received.
#'   - `transaction_type` – Type of transaction (e.g., "Trade").
#'   - `exchange` – Exchange where the transaction occurred.
#'
#' @return A data frame with an additional column `eur_rate` containing the EUR conversion rate.
#'
#' @details
#' - If the transaction is a crypto-to-crypto trade and the exchange is not Kraken:
#'   - If the sent currency is "VEN", a manual rate of 1.756 is applied.
#'   - If the sent currency is "ERD", a manual rate of 0.0157 is applied.
#'   - If the sent currency is "NANO", the rate is fetched from a preset table.
#'   - For other currencies, the rate is fetched from Yahoo Finance API.
#' - If the exchange is Kraken:
#'   - If the transaction date is after May 1, 2023, daily data is retrieved from Kraken.
#'   - If the transaction date is before May 1, 2023, weekly data is retrieved from Kraken.
#' - Transactions that are not crypto-crypto trades are preserved with `eur_rate = NA`.
#' 
#' @seealso Live coinwatch is good location to get values for manually added coins \url{https://www.livecoinwatch.com/}
#'
#' @examples
#' \dontrun{
#' transactions <- data.frame(
#'   date = as.Date("2023-01-01"),
#'   time = "12:00:00",
#'   sent_currency = "BTC",
#'   received_currency = "ETH",
#'   transaction_type = "Trade",
#'   exchange = "kraken"
#' )
#' 
#' add_eur_rate(transactions)
#' }
#'
#' @export
add_eur_rate <- function(transactions) {
  
  # Preset table for NANO prices (not available on Yahoo)
  nano_prices <- tribble(
    ~date, ~eur_rate,
    "2018-06-14", 2.331,
    "2019-06-17", 1.388
  ) |> mutate(date = as_date(date))
  
  result_list <- list()
  
  for (i in seq_len(nrow(transactions))) {
    row <- transactions[i, ]
    sent_currency <- row$sent_currency
    received_currency <- row$received_currency
    
    if (row$transaction_type == "Trade" && received_currency != "EUR" && sent_currency != "EUR") {
      cat("Crypto-to-crypto trade on row: ", i, "\n")
      
      if (!row$exchange %in% c("kraken", "gatehub")) {
        
        if (sent_currency == "VEN") {
          cat("Manual VEN rate set to 1.756\n")
          row$eur_rate <- 1.756
        } else if (sent_currency == "ERD") {
          cat("Manual ERD rate set to 0.0157\n")
          row$eur_rate <- 0.0157
        } else if (sent_currency == "NANO") {
          cat("Getting NANO rate from preset table\n")
          row$eur_rate <- inner_join(tibble(date = row$date), nano_prices, by = "date")$eur_rate
        } else {
          cat("Fetching rate from Yahoo Finance\n")
          row$eur_rate <- get_crypto_yahoo(sent_currency, row$date)$value
        }
        
      } else {
        timestr <- ymd_hms(paste(row$date, row$time))
        cat("Fetching rate from Kraken API\n")
        
        if (timestr > ymd("2023-05-01")) {
          price_data <- get_ohlc(paste0(sent_currency, "EUR"), 0, interval = 1440)
        } else {
          timeint <- as.numeric(timestr) - (86400 * 7)
          price_data <- get_ohlc(paste0(sent_currency, "EUR"), timeint)
        }
        
        if (!is.null(price_data)) {
          closest_price <- inner_join(
            tibble(time = timestr),
            price_data,
            by = join_by(closest(time < stamp))
          )$value
          row$eur_rate <- closest_price
        } else {
          row$eur_rate <- NA
        }
      }
    } else {
      row$eur_rate <- NA
    }
    
    result_list[[i]] <- row
  }
  
  txs_with_eur_rate <- bind_rows(result_list)
  return(txs_with_eur_rate)
}
