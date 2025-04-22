import pandas as pd
import yfinance as yf
import time

# === Get NASDAQ tickers ===
url = 'https://www.nasdaqtrader.com/dynamic/SymDir/nasdaqlisted.txt'
df_raw = pd.read_csv(url, sep='|')
tickers = df_raw[df_raw['Test Issue'] == 'N']['Symbol'].tolist()

# === Initialize option data storage ===
all_options = []

# === Loop through all tickers with progress output ===
for idx, symbol in enumerate(tickers):
    # Progress Update
    print(f"Processing {idx + 1}/{len(tickers)}: {symbol}")  
    try:
        ticker = yf.Ticker(symbol)
        for exp in ticker.options:
            chain = ticker.option_chain(exp)
            calls = chain.calls
            calls['symbol'] = symbol
            calls['expirationDate'] = exp
            all_options.append(calls)
        # Avoid Getting Block & Limit Request to Public Server
        time.sleep(1)  
    except Exception as e:
        print(f"Error with {symbol}: {e}")
        continue

# === Save final combined CSV ===
if all_options:
    df_options = pd.concat(all_options, ignore_index=True)
    df_options.to_csv("C:/Users/ngtru/OneDrive/Documents/College work/STA404/bulk_options_nasdaq.csv", index=False)
    print("Option data saved as bulk_options_nasdaq.csv")
else:
    print("No option data collected.")
