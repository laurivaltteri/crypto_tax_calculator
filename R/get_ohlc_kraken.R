#' Get OHLC (Open, High, Low, Close) Data from Kraken API
#'
#' This function retrieves OHLC data for a specified asset pair from the Kraken public API.
#'
#' @param asset_pair A string representing the trading pair (e.g., "BTCUSD", "ETHUSD").
#' @param since A numeric value representing the starting timestamp (in seconds since the epoch) 
#'   from which to retrieve the data.
#' @param interval An optional numeric value representing the interval in minutes for OHLC data. 
#'   Defaults to 10080 (1 week).
#'
#' @details
#' - The function sends a GET request to the Kraken public API endpoint for OHLC data.
#' - If data is successfully retrieved, it returns a tibble with the following columns:
#'   - `stamp` – POSIXct timestamp of the OHLC data point.
#'   - `value_low` – Lowest price in the interval.
#'   - `value_high` – Highest price in the interval.
#'   - `value` – Average of the high and low prices in the interval.
#' - If the API request fails or returns no data, the function returns `NULL`.
#'
#' @return A tibble with OHLC data or `NULL` if the request fails or returns no data.
#'
#' @examples
#' \dontrun{
#' # Retrieve weekly BTC/USD data starting from a specific timestamp
#' get_ohlc("BTCUSD", 1609459200, interval = 10080)
#' }
#'
#' @seealso 
#' - Kraken API documentation: \url{https://docs.kraken.com/api/docs/rest-api/get-ohlc-data/}
#' - Relevant blog post: \url{https://medium.com/@kyleleedixon/using-krakens-api-with-r-2376064a3244}
#'
#' @export
get_ohlc <- function(asset_pair, since, interval = 10080) {
  timeint <- since
  url <- "https://api.kraken.com/0/public/OHLC?"
  parameters <- base::paste0("pair=", asset_pair, "&since=", timeint, "&interval=", interval)
  
  response <- httr::GET(base::paste0(url, parameters))
  
  if (httr::status_code(response) == 200) {
    out <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    
    if (length(out[["result"]][[1]]) > 0) {
      data <- out[["result"]][[1]] 
      colnames(data) <- c("time", "open", "high", "low", "close", "vwap", "volume", "count")
      
      price <- 
        data |> 
        as_tibble() |> 
        transmute(
          stamp = as.POSIXct(as.numeric(time)),
          value_low = as.numeric(low),
          value_high = as.numeric(high),
          value = (value_high + value_low) / 2
        )
      
      return(price)
    } else {
      cat("Call returned no data.\n")
      return(NULL)
    }
  } else {
    cat("Failed to fetch data. HTTP Status: ", httr::status_code(response), "\n")
    return(NULL)
  }
}