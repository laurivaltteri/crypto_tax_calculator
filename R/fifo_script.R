## Main FIFO profit calculation script
#

# read functions
source("R/add_eur_rate.R")
source("R/compute_fifo_profits.R")
source("R/get_crypto_yahoo.R")
source("R/get_ohlc_kraken.R")

# tidyvee
library(tidyverse)

# read data
krkn_data <- readr::read_csv("../data/divly/kraken.divly.csv")
ethr_data <- readr::read_csv("../data/divly/ethereum.divly.csv")
# ccom_data <- readr::read_csv("data/divly/cryptocom.divly.csv")
# binc_data <- readr::read_csv("data/divly/binance.divly.csv") |> rename(time = `time (UTC)`)
# ghub_data <- readr::read_csv("data/divly/gatehub.divly.csv")
# batl_data <- readr::read_csv("data/divly/idex_bat.divly.csv")

# combine data
all_exchanges <- 
  bind_rows(
    list(
      kraken = krkn_data,
      ethereum = ethr_data
      # cryptocom = ccom_data,
      # binance = binc_data,
      # gatehub = ghub_data,
      # idex = batl_data
    ),
    .id = "exchange"
  ) |> 
  arrange(date, time)

################## --- ###

################## --- ###

# add EUR link to crypto-crypto trades
all_with_eurrate <- add_eur_rate(all_exchanges)

# compute the fifo profits
results <- compute_fifo_profits(all_with_eurrate)

################## --- ###

################## --- ###

################## --- ###
## Create FIFO table for Vero

# tax_fifo <- 
#   results |> 
#   transmute(
#     aika = ymd_hms(paste(date, time)),
#     realisoitu_valuutta = sent_currency,
#     realisoitu_maara = sent_amount,
#     hankittu_valuutta = received_currency,
#     hankittu_maara = received_amount,
#     tapahtuman_tyyppi = transaction_type,
#     lahde = exchange,
#     pari = paste0(sent_currency, "|", received_currency),
#     euro_arvo = 
#       case_when(
#         received_currency== "EUR" ~ received_amount,
#         sent_currency == "EUR" ~ sent_amount,
#         .default = eur_rate*sent_amount),
#     realisoidun_kurssi = # sent_price
#       case_when(
#         sent_currency=="EUR" ~ 1,
#         received_currency=="EUR" ~ received_amount/sent_amount,
#         .default = eur_rate
#       ),
#     hankitun_kurssi = # recieved_price
#       case_when(
#         received_currency == "EUR" ~ 1,  
#         sent_currency == "EUR" ~ sent_amount/received_amount,
#         .default = eur_rate*sent_amount/received_amount),
#     todellinen_hankintameno = total_cost,
#     hankintameno_olettama = deemed_cost,
#     voitto_tappio = profit_final,
#     realisoitua_jaljella = sold_currency_stack,
#     hankittua_jaljella = received_currency_stack
#   )

## Create FIFO table for Vero

tax_fifo <- 
  results |> 
  mutate(
    profit_nofee = 
      case_when(
        fee_currency=="EUR" ~ profit_final-fee_amount,
        !is.na(eur_rate)&!is.na(fee_amount) ~ profit_final-fee_amount*eur_rate,
        received_currency=="EUR"&fee_currency!="EUR"&!is.na(fee_amount) ~ profit_final-fee_amount*(received_amount/sent_amount),
        .default = profit_final
      )
  ) |>
  transmute(
    aika = ymd_hms(paste(date, time)),
    realisoitu_valuutta = sent_currency,
    realisoitu_maara = sent_amount,
    hankittu_valuutta = received_currency,
    hankittu_maara = received_amount,
    tapahtuman_tyyppi = transaction_type,
    lahde = exchange,
    pari = paste0(sent_currency, "|", received_currency),
    euro_arvo = 
      case_when(
        received_currency== "EUR" ~ received_amount,
        sent_currency == "EUR" ~ sent_amount,
        .default = eur_rate*sent_amount),
    realisoidun_kurssi = # sent_price
      case_when(
        sent_currency=="EUR" ~ 1,
        received_currency=="EUR" ~ received_amount/sent_amount,
        .default = eur_rate
      ),
    hankitun_kurssi = # recieved_price
      case_when(
        received_currency == "EUR" ~ 1,  
        sent_currency == "EUR" ~ sent_amount/received_amount,
        .default = eur_rate*sent_amount/received_amount),
    todellinen_hankintameno = total_cost,
    hankintameno_olettama = deemed_cost,
    voitto_tappio = profit_nofee,
    realisoitua_jaljella = sold_currency_stack,
    hankittua_jaljella = received_currency_stack
  )

tax_fifo |> write_csv("../output/lactc_tax_fifo.csv", na="")

# tax_fifo |> 
#   filter(lubridate::year(aika)==2025) |> 
#   write_csv("../output/tax_fifo_2025.csv", na="")

tax_fifo |> 
  mutate(year = lubridate::year(aika)) |> 
  group_by(year) |> 
  summarise(
    voitto = sum(voitto_tappio, na.rm = TRUE)
  )

yhteenveto <-
  tax_fifo |> 
  mutate(vuosi = lubridate::year(aika)) |> 
  filter(vuosi == 2025) |> 
  drop_na(voitto_tappio) |> 
  mutate(
    voitollinen = if_else(voitto_tappio > 0, "voitollisiin", "tappiollisiin")
  ) |> 
  group_by(voitollinen, .drop = NA) |> 
  summarise(
    luovutushinnat = sum(euro_arvo),
    hankintamenot = sum(todellinen_hankintameno),
    `voitot/tappiot` = sum(voitto_tappio)
  ) |> 
  pivot_wider(
    names_from = voitollinen, 
    values_from = c(luovutushinnat, hankintamenot, `voitot/tappiot`),
    names_glue = "{voitollinen} kohdistuneet {.value}") |> 
  mutate(
    `luovutushinnat yht.` = `voitollisiin kohdistuneet luovutushinnat`, #+ `tappiollisiin kohdistuneet luovutushinnat`,
    `voitto/tappio yht.` = `voitollisiin kohdistuneet voitot/tappiot`, #+ `tappiollisiin kohdistuneet voitot/tappiot`,
  ) |> 
  select(
    "luovutushinnat yht.",
    "voitollisiin kohdistuneet luovutushinnat",
    "voitollisiin kohdistuneet hankintamenot",
    luovutusvoitot = "voitollisiin kohdistuneet voitot/tappiot",
    "tappiollisiin kohdistuneet luovutushinnat",
    "tappiollisiin kohdistuneet hankintamenot" ,
    luovutustappiot = "tappiollisiin kohdistuneet voitot/tappiot",
    "voitto/tappio yht."
  )

as_tibble(cbind(vuosi_2025 = names(yhteenveto), t(yhteenveto))) |>
  transmute(
    vuosi_2025=vuosi_2025,
    yhteenveto=V2
  )