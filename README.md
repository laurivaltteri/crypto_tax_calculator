# crypto_tax_calculator
Compute the taxable profits from divly style .csv:s.  
And scripts to transform exported exchange form various exchanges to unified divly style tables.


NOTES:
WHEN CALCULATING FIFO IN THE FUTURE: As only 720 entries is retrieved with get_ohlc_kraken() function remember to adjust the start of daily values retrieval accordingly (see add_eur_rate() function for details).
