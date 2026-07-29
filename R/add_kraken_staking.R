#' Add EUR Value to Skating Rewards
#'
#' This function adds the EUR conversion rate to transactions in Kraken that
#' provides staking rewards. It identifies rewars based on "Staking Earn" label. 
#' Other rows are carried through without modification.
#'
#' @param transactions A data frame containing cryptocurrency transaction data. Must include the following columns:
#'   - `date` – Date of the transaction.
#'   - `time` – Time of the transaction.
#'   - `received_currency` – The currency that was received.
#'   - `label` – Type of transaction (specifically, "Staking Earn").
#'
#' @return A data frame with an additional column `eur_rate` containing the EUR conversion rate.
#'
#' @details
#' - Transactions that are not crypto-crypto trades are preserved with `eur_rate = NA`.
#' 
#' @seealso Live coinwatch is good location to get values for manually added coins \url{https://www.livecoinwatch.com/}
#'
#' @examples
#' \dontrun{
#' transactions <- data.frame(
#'   date = as.Date("2023-01-01"),
#'   time = "12:00:00",
#'   received_currency = "ETH",
#'   received_amount = 0.0005,
#'   label = "Staking Reward",
#'   exchange = "kraken",
#'   eur_rate = NA_real_
#' )
#' 
#' add_kraken_staking(transactions)
#' }
#'
#' @export
add_kraken_staking <- function(transactions) {
  result_list <- vector("list", nrow(transactions))
  ohlc_cache <- list()  # per currency: daily table, wide table, daily table's actual start date

  for (i in seq_len(nrow(transactions))) {
    row <- transactions[i, ]
    label <- row$label[[1]]
    received_currency <- row$received_currency[[1]]
    received_currency <- sub("\\..*", "", received_currency) # Remove any suffix as the Kraken API may rename currencies (e.g., "DOT" to "DOT.S")
    eur_rate <- row$eur_rate[[1]]

    if (!is.na(label) && label == "Staking Reward" && is.na(eur_rate)) {
      timestr <- lubridate::ymd_hms(paste(row$date[[1]], row$time[[1]]))

      if (is.null(ohlc_cache[[received_currency]])) {
        ohlc_cache[[received_currency]] <- list(daily = NULL, wide = NULL, daily_min = NULL)
      }
      cur <- ohlc_cache[[received_currency]]

      # fetch daily table once per currency; note its actual coverage start
      if (is.null(cur$daily)) {
        cat("Fetching daily rate table for", received_currency, "\n")
        cur$daily <- get_ohlc(paste0(received_currency, "EUR"), 0, interval = 1440)
        if (!is.null(cur$daily) && nrow(cur$daily) > 0) {
          cur$daily_min <- min(cur$daily$stamp)
        }
        Sys.sleep(0.3)
      }

      use_daily <- !is.null(cur$daily_min) && timestr >= cur$daily_min

      if (use_daily) {
        price_data <- cur$daily
      } else {
        # fetch the wide, coarse table once per currency — covers full history
        if (is.null(cur$wide)) {
          cat("Fetching wide rate table for", received_currency, "\n")
          cur$wide <- get_ohlc(paste0(received_currency, "EUR"), 0, interval = 21600)
          Sys.sleep(0.3)
        }
        price_data <- cur$wide
      }

      ohlc_cache[[received_currency]] <- cur  # persist updated cache entry

      if (!is.null(price_data) && nrow(price_data) > 0) {
        closest_price <- dplyr::inner_join(
          tibble::tibble(time = timestr),
          price_data,
          by = dplyr::join_by(closest(time < stamp))
        )$value_low
        row$eur_rate <- if (length(closest_price) > 0) closest_price[[1]] else NA_real_
      } else {
        row$eur_rate <- NA_real_
      }
    }

    result_list[[i]] <- row
  }

  dplyr::bind_rows(result_list)
}