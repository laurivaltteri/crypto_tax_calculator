# 🧾 crypto_tax_calculator

A lightweight and extensible R-based tool for calculating taxable profits from cryptocurrency transactions, using FIFO (First-In-First-Out) methodology. 

This repo also includes scripts to **normalize export files from multiple exchanges** into a unified format compatible with [Divly](https://divly.com/) style `.csv` files, example can be found [here](https://docs.google.com/spreadsheets/d/1jGvqnK8OxwcjwpobwkP1k4jj8Sk9OHTEO5WgKIBKbTU/).

---

## 📦 Features

- 🔄 **Data Normalization**  
  Scripts to clean and convert exported data from various exchanges (e.g. Kraken, Binance) to a common format.

- 💶 **EUR Valuation**  
  Adds EUR values to crypto-to-crypto trades using:
  - Yahoo Finance API for historical fiat conversions
  - Kraken OHLC API for price data from the right source
  - Manual rates can be added for unsupported assets (e.g., VEN, ERD)

- 📈 **FIFO-Based Profit Computation**  
  Realized and deemed profits calculated per transaction using FIFO matching of acquisition cost.

- 📂 **Modular Architecture**  
  Functions are decoupled and well-documented for easy inspection and testing.

---

## 🛠️ Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/laurivaltteri/crypto_tax_calculator.git
   ```

2. Open in RStudio or your preferred R environment.
3. Init `renv`

## 🚀 Usage
Example Workflow
1. Convert exchange exports to Divly-style CSVs using scripts in `R/*_schema.R`.
  - For this you will need to upload your exchange exports to `/raw` folder
  - Then modify the appropoate `R/<exhange>_schema.R` to read and convert your ledger
2. Load and process your data:
```r
# Assuming 'combined.csv' is your merged, normalized transaction file
df <- read_csv("data/combined.csv")

df_with_rates <- add_eur_rate(df)
df_with_profits <- compute_fifo_profits(df_with_rates)
```
3. Inspect or export your final data:
```r
View(df_with_profits)
write_csv(df_with_profits, "results/crypto_profits.csv", row.names = FALSE)
```
## ⚠️ Notes & Limitations
- Kraken OHLC API Limit: The get_ohlc() function retrieves only up to 720 data points. When fetching daily data for long time spans, the script switches to weekly data (see add_eur_rate() for fallback logic).
- Manual Price Data: Some assets (e.g., NANO, VEN, ERD) are not supported via APIs and require manually maintained price tables (see nano_prices in add_eur_rate()).
- FIFO Stack Overflows: The system raises warnings if an asset is sold without sufficient acquisition history (i.e., empty FIFO stack).

## 📁 Project Structure
```graphql
.
├── R/
│   ├── add_eur_rate.R             # Adds EUR valuation to each trade
│   ├── compute_fifo_profits.R     # FIFO-based profit calculation
│   ├── compute_staking_earnings.R # Staking earnings calculation for Kraken
│   ├── add_kraken_staking.R       # Adds EUR values to Kraken staking rewards
│   ├── get_ohlc_kraken.R          # Kraken OHLC data retrieval
│   ├── get_crypto_yahoo.R         # Yahoo price fetcher
│   ├── *_divly_schema.R           # Varous examples to read in data
│   └── fifo_script.R              # Example glue script
├── data/
│   └── divly/
│   │   └── *.divly.csv        # Normalized transaction data
│   └── raw/
│       └── <from_source>.csv  # Exported data
├── results/
│   └── crypto_profits.csv     # Output
└── README.md
```

## 🧪 Example
```r
source("R/fifo_script.R")
# produces table similar to Vero FIFO table
```

## 📄 License
MIT License. Feel free to use and adapt.


