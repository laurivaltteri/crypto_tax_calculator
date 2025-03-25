#' Get Cryptocurrency Price Data from Yahoo Finance
#'
#' This function retrieves the open, high, low, close, and volume data for a specified cryptocurrency 
#' in EUR from the Yahoo Finance API on a specific date.
#'
#' @param symbol A string representing the cryptocurrency symbol (e.g., "BTC", "ETH").
#' @param date A string representing the date for which to retrieve the data, in "YYYY-MM-DD" format.
#'
#' @details
#' - The function constructs a GET request to the Yahoo Finance API for the specified cryptocurrency symbol.
#' - The data is retrieved in EUR.
#' - If data is successfully retrieved, it returns a data frame with the following columns:
#'   - `date` – Date of the price data.
#'   - `open` – Opening price.
#'   - `value_high` – Highest price during the day.
#'   - `value_low` – Lowest price during the day.
#'   - `close` – Closing price.
#'   - `volume` – Trading volume.
#'   - `value` – Average of the high and low prices during the day.
#' - If the API request fails or returns no data, the function returns `NULL`.
#'
#' @return A data frame with cryptocurrency price data or `NULL` if the request fails or returns no data.
#'
#' @examples
#' \dontrun{
#' # Retrieve BTC price data in EUR on January 1, 2022
#' get_crypto_yahoo("BTC", "2022-01-01")
#' }
#'
#' @seealso 
#' - Yahoo Finance API documentation: \url{https://www.yahoofinanceapi.com/}
#'
#' @export
get_crypto_yahoo <- function(symbol, date) {
  # Convert date to timestamp
  start_date <- as.numeric(as.POSIXct(date, tz = "UTC"))
  end_date <- start_date + 86400  # +1 day in seconds to cover full day range
  
  # Construct URL for Yahoo Finance API with EUR pair
  url <- paste0(
    "https://query1.finance.yahoo.com/v8/finance/chart/",
    symbol, "-EUR?period1=", start_date,
    "&period2=", end_date,
    "&interval=1d"
  )
  
  # Make the GET request
  response <- httr::GET(url)
  
  if (httr::status_code(response) == 200) {
    # Parse the response JSON
    data <- jsonlite::fromJSON(httr::content(response, as = "text"))
    
    if (length(data$chart$result) > 0) {
      quotes <- data$chart$result$indicators$quote[[1]]
      
      # Extract open, high, low, and close prices
      price_data <- data.frame(
        date = as.Date(date),
        open = quotes$open[[1]][1],
        value_high = quotes$high[[1]][1],
        value_low = quotes$low[[1]][1],
        close = quotes$close[[1]][1],
        volume = quotes$volume[[1]][1]
      ) |> mutate(value = (value_high + value_low) / 2)
      
      return(price_data)
    } else {
      cat("No data available for this date.\n")
      return(NULL)
    }
  } else {
    cat("Failed to fetch data. HTTP Status: ", httr::status_code(response), "\n")
    return(NULL)
  }
}