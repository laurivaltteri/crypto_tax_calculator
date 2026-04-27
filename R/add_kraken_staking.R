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

  for (i in seq_len(nrow(transactions))) {
    row <- transactions[i, ]
    label <- row$label[[1]]
    received_currency <- row$received_currency[[1]]
		eur_rate <- row$eur_rate[[1]]

    if (!is.na(label) && label == "Staking Reward" && is.na(eur_rate)) {
			cat("Staking reward on row: ", i, "\n")
      timestr <- lubridate::ymd_hms(paste(row$date[[1]], row$time[[1]]))
			cat("Fetching rate from Kraken API\n")
      price_data <- if (timestr > lubridate::ymd("2024-01-01")) {
        get_ohlc(paste0(received_currency, "EUR"), 0, interval = 1440)
      } else {
        timeint <- as.numeric(timestr) - (86400 * 15)
        get_ohlc(paste0(received_currency, "EUR"), timeint, interval = 21600)
      }

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