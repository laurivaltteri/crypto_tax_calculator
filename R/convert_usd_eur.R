#' Retrieve USD to EUR Exchange Rate for a Given Date
#'
#' This function retrieves the daily USD to EUR exchange rate (closing value) for a specific date
#' using Yahoo Finance's unofficial API.
#'
#' @param date A character string representing the date in "YYYY-MM-DD" format (e.g., "2022-01-01").
#'
#' @return A numeric value representing the USD to EUR exchange rate (closing price) for the given date.
#' Returns `NULL` if the data is not available or if the request fails.
#'
#' @details
#' The function queries the Yahoo Finance chart API for the `USDEUR=X` currency pair,
#' retrieving data at a daily interval for the specified date. Internally, it converts the
#' provided date into a UNIX timestamp and fetches the close value for the day.
#'
#' Note that the Yahoo Finance API is unofficial and may change without notice.
#'
#' @examples
#' \dontrun{
#' convert_usd_eur("2022-01-01")
#' }
#'
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @export

convert_usd_eur <- function(date) {
  # Convert date to timestamp
  start_date <- as.numeric(as.POSIXct(date, tz = "UTC"))
  end_date <- start_date + 86400  # +1 day in seconds to cover full day range
  # Construct URL for Yahoo Finance API with EUR pair
  url <- paste0(
    "https://query1.finance.yahoo.com/v8/finance/chart/",
    "USDEUR=X?period1=", start_date,
    "&period2=", end_date,
    "&interval=1d"
  )
  
  # Make the GET request
  response <- httr::GET(url)
  
  if (httr::status_code(response) == 200) {
    # Parse the response JSON
    data <- jsonlite::fromJSON(httr::content(response, as = "text"))
    
    if (length(data$chart$result) > 0) {
      price_data <- data$chart$result$indicators$quote[[1]]$close[[1]][1]
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