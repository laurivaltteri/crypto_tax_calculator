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
krkn_data <- readr::read_csv("data/divly/kraken.divly.csv")
ccom_data <- readr::read_csv("data/divly/cryptocom.divly.csv")
binc_data <- readr::read_csv("data/divly/binance.divly.csv") |> rename(time = `time (UTC)`)
ghub_data <- readr::read_csv("data/divly/gatehub.divly.csv")
batl_data <- readr::read_csv("data/divly/idex_bat.divly.csv")

# combine data
all_exchanges <- 
  bind_rows(
    list(
      kraken = krkn_data,
      cryptocom = ccom_data,
      binance = binc_data,
      gatehub = ghub_data,
      idex = batl_data
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

tax_fifo <- 
  results |> 
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
    voitto_tappio = profit_final,
    realisoitua_jaljella = sold_currency_stack,
    hankittua_jaljella = received_currency_stack
  )
