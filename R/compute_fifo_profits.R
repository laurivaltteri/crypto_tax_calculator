#' Compute Profits Using FIFO Method for Cryptocurrency Trades
#'
#' This function calculates realized and deemed profits from cryptocurrency trades using the FIFO (First-In-First-Out) method.
#'
#' @param transactions A data frame containing cryptocurrency transactions, with EUR rates already added.
#'   Expected columns:
#'   - `transaction_type`: should include "Trade" for trades.
#'   - `sent_currency`, `received_currency`: asset symbols.
#'   - `sent_amount`, `received_amount`: numeric values of amounts transacted.
#'   - `eur_rate`: EUR conversion rate for non-EUR assets.
#'
#' @return A data frame containing the original transaction data plus the following additional columns:
#'   - `total_cost`: Actual targeted cost
#'   - `deemed_cost`: Deemed cost (20% of EUR proceeds)
#'   - `profit_real`: Realized profit based on FIFO acquisition cost.
#'   - `profit_deemed`: Profit based on deemed acquisition cost (20% of EUR proceeds).
#'   - `profit_final`: Minimum of real and deemed profit (conservative tax treatment).
#'   - `sold_currency_stack`: Remaining amount in FIFO stack of sold currency.
#'   - `received_currency_stack`: Remaining amount in FIFO stack of received currency.
#'
#' @details
#' - Applies FIFO logic to match sales with prior purchases.
#' - For crypto-to-crypto trades, EUR value is estimated based on the `eur_rate`.
#' - Warnings are raised if assets are sold beyond the available FIFO stack.
#' - Handles purchases (adding to FIFO stack) and sales (removing and computing profit).
#'
#' @examples
#' \dontrun{
#' result <- compute_fifo_profits(transactions_with_eur_rate)
#' }
#'
#' @export
compute_fifo_profits <- function(transactions) {
  fifo_stacks <- list()  # FIFO asset stacks
  results <- list()      # Result rows
  
  for (i in seq_len(nrow(transactions))) {
    row <- transactions[i, ]
    
    if (row$transaction_type != "Trade") {
      next
    }
    
    asset <- row$sent_currency
    received_asset <- row$received_currency
    sent_EUR <- 0
    
    # Estimate EUR value of trade
    if (asset != "EUR" & received_asset != "EUR") {
      sent_EUR <- row$eur_rate * row$sent_amount
    } else if (asset == "EUR") {
      sent_EUR <- row$sent_amount
    } else {
      sent_EUR <- NA
    }
    
    # Handle purchases (add to FIFO stack)
    if (received_asset != "EUR") {
      if (!received_asset %in% names(fifo_stacks)) {
        fifo_stacks[[received_asset]] <- tibble(amount = numeric(), price = numeric())
      }
      fifo_stacks[[received_asset]] <- 
        bind_rows(
          fifo_stacks[[received_asset]],
          tibble(
            amount = row$received_amount,
            price = sent_EUR / row$received_amount
            )
          )
    }
    
    # Handle sales (remove from FIFO stack and compute profit)
    if (asset != "EUR") {
      total_cost <- 0
      remaining_uncovered <- row$sent_amount
      
      if (asset %in% names(fifo_stacks) && nrow(fifo_stacks[[asset]]) > 0) {
        sell_amount <- row$sent_amount
        while (sell_amount > 0 & nrow(fifo_stacks[[asset]]) > 0) {
          first_entry <- fifo_stacks[[asset]][1, ]
          if (first_entry$amount <= sell_amount) {
            total_cost <- total_cost + first_entry$amount * first_entry$price
            sell_amount <- sell_amount - first_entry$amount
            fifo_stacks[[asset]] <- fifo_stacks[[asset]][-1, ]
          } else {
            total_cost <- total_cost + sell_amount * first_entry$price
            fifo_stacks[[asset]]$amount[1] <- fifo_stacks[[asset]]$amount[1] - sell_amount
            sell_amount <- 0
          }
        }
        remaining_uncovered <- sell_amount
      }
      
      if (remaining_uncovered > 0) {
        warning(glue::glue("Insufficient FIFO stack for {asset} on row {i}. Missing {remaining_uncovered} units."))
      }
      
      # Compute profits
      revenue <- ifelse(received_asset == "EUR", row$received_amount, sent_EUR)
      deemed_acquisition_cost <- revenue * 0.2
      profit_real <- revenue - total_cost
      profit_deemed <- revenue - deemed_acquisition_cost
      
      row$total_cost <- total_cost
      row$deemed_cost <- deemed_acquisition_cost
      row$profit_real <- profit_real
      row$profit_deemed <- profit_deemed
      row$profit_final <- min(profit_real, profit_deemed)
    } else {
      row$total_cost <- NA
      row$deemed_cost <- NA
      row$profit_real <- NA
      row$profit_deemed <- NA
      row$profit_final <- NA
    }
    
    # Track FIFO stack status
    row$sold_currency_stack <- ifelse(asset %in% names(fifo_stacks),
                                      sum(fifo_stacks[[asset]]$amount), NA)
    row$received_currency_stack <- ifelse(received_asset %in% names(fifo_stacks),
                                          sum(fifo_stacks[[received_asset]]$amount), NA)
    
    results[[i]] <- row
  }
  
  return(bind_rows(results))
}
