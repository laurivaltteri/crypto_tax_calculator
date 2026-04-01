#' Compute EUR Earnings from Staking Rewards
#'
#' This function computes staking earnings in EUR from rows labeled
#' `"Staking Reward"`.
#'
#' @param transactions A data frame containing transaction rows.
#'   Expected columns:
#'   - `label`
#'   - `received_amount`
#'   - `fee_amount`
#'   - `eur_rate`
#'
#' @return A data frame containing only `"Staking Reward"` rows and one extra column:
#'   - `staking_earnings_eur`: `(received_amount - fee_amount) * eur_rate`
#'
#' @details
#' - Missing `fee_amount` values are treated as `0`.
#' - Rows with missing `eur_rate` will result in `NA` earnings.
#'
#' @examples
#' \dontrun{
#' staking <- compute_staking_earnings(all_exchange_rewards)
#' }
#'
#' @export
compute_staking_earnings <- function(transactions) {
  transactions |>
    dplyr::filter(label %in% "Staking Reward") |>
    dplyr::mutate(
      staking_earnings_eur = (received_amount - dplyr::coalesce(fee_amount, 0)) * eur_rate
    )
}
