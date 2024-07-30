---
title: "ACTL1101 Assignment Part A"
author: "William Yoo"
date: "2024 T2"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1.  **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2.  **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3.  **Customize Trading Period:** Choose your entry and exit dates.

4.  **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5.  **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI.

6.  **Discussion:** Summarise your finding.

## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates.

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```

##Plotting the Data Plot the closing prices over time to visualize the price movement.

```{r plot}
plot(amd_df$date, amd_df$close,'l')
```

## Step 2: Trading Algorithm

Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

-   Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
-   Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
    -   If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
    -   Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
    -   You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
    -   If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.

```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  
  # Initial buy
  if (previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- - current_price * share_size
    accumulated_shares <- share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
  } 
  # Buy more shares if price drops
  else if (current_price < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- - current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
  } 
  # Case where you neither buy nor sell
  else {
    if (i > 1) {
      amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
    }
  }
  
  # Preparing to move to next day
  previous_price <- current_price
  
  # Sell all shares on the final day
  if (i == nrow(amd_df)) {
    last_row <- nrow(amd_df)
    amd_df$trade_type[last_row] <- 'sell'
    amd_df$costs_proceeds[last_row] <- accumulated_shares * amd_df$close[last_row]
  }
}

```

## Step 3: Customize Trading Period

-   Define a trading period you wanted in the past five years

```{r period}
# Define start and end dates
start_date <- as.Date('2023-01-01')
end_date <- as.Date('2024-01-01')

# Filter dates to include the trading period
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

```

## Step 4: Run Your Algorithm and Analyze Results

After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

-   Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
-   Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
-   ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# Initialise step 4 variables
total_PL <- 0
total_invested_capital <- 0
ROI <- 0

# Calculate total P/L and invested capital
for (i in 1:nrow(amd_df)) {
  # Updating total P/L value
  if (is.na(amd_df$costs_proceeds[i]) == FALSE) {
    total_PL <- total_PL + amd_df$costs_proceeds[i]
  }
  # Updating total invested capital value
  if (is.na(amd_df$trade_type[i]) == FALSE && amd_df$trade_type[i] == 'buy') {
    total_invested_capital <- total_invested_capital - amd_df$costs_proceeds[i]
  }
}

# Calculate ROI
ROI <- (total_PL / total_invested_capital) * 100

```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanism (Choose 1)

-   Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
-   Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.

```{r option}
# Option 1 - Profit-taking strategy

amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- NA
amd_df$total_cost <- NA
amd_df$avg_purchase_price <- NA

previous_price <- 0
share_size <- 100

for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  
  # Initial buy
  if (previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- - current_price * share_size
    amd_df$accumulated_shares[i] <- share_size
    amd_df$total_cost[i] <- - amd_df$costs_proceeds[i]
    amd_df$avg_purchase_price[i] <- amd_df$total_cost[i] / amd_df$accumulated_shares[i]
  } 
  # Buy more shares if price drops
  else if (current_price < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- - current_price * share_size
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + share_size
    amd_df$total_cost[i] <- amd_df$total_cost[i-1] - amd_df$costs_proceeds[i]
    amd_df$avg_purchase_price[i] <- amd_df$total_cost[i] / amd_df$accumulated_shares[i]
  } 
  # Sell half of the holdings if price increases by 20%
  else if (i > 1 && current_price >= amd_df$avg_purchase_price[i-1] * 1.2) {
    sold_shares <- amd_df$accumulated_shares[i-1] / 2
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- current_price * sold_shares
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] - sold_shares
    amd_df$total_cost[i] <- amd_df$total_cost[i-1] / 2
    # Average purchase price remains the same after selling half of the holdings, 
    # as both the total cost and accumulated shares are halved
    amd_df$avg_purchase_price[i] <- amd_df$avg_purchase_price[i-1]
  }
  # Case where you neither buy nor sell  
  else {
    if (i > 1) {
      amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
      amd_df$avg_purchase_price[i] <- amd_df$avg_purchase_price[i-1]
      amd_df$total_cost[i] <- amd_df$total_cost[i-1]
    }
  }
  
  # Preparing to move to next day
  previous_price <- current_price
  
  # Sell all shares on the final day
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i] * current_price
    amd_df$accumulated_shares[i] <- 0
  }
}

```

## Step 6: Summarize Your Findings

-   Did your P/L and ROI improve over your chosen period?
-   Relate your results to a relevant market event and explain why these outcomes may have occurred.

```{r}
# Initialise new variables
new_total_PL <- 0
new_total_invested_capital <- 0
new_ROI <- 0

# Calculate new total P/L and new invested capital
for (i in 1:nrow(amd_df)) {
  # Updating new total P/L value
  if (is.na(amd_df$costs_proceeds[i]) == FALSE) {
    new_total_PL <- new_total_PL + amd_df$costs_proceeds[i]
  }
  # Updating new total invested capital value
  if (is.na(amd_df$trade_type[i]) == FALSE && amd_df$trade_type[i] == 'buy') {
    new_total_invested_capital <- new_total_invested_capital - amd_df$costs_proceeds[i]
  }
}

# Calculate new ROI
new_ROI <- (new_total_PL / new_total_invested_capital) * 100

# Calculate difference in total P/L
difference_total_PL <- new_total_PL - total_PL

```

### Discussion

Between January 1st, 2023 and January 1st, 2024, we compared two algorithmic trading strategies. The first strategy involved a simple buy-on-dip approach, while the second strategy used a profit-taking mechanism where half of our holdings were sold upon a 20% increase in stock price from the average purchase price. Contrary to expectations, the profit-taking strategy (step 5) largely underperformed compared to the buy-on-dip strategy (step 2).

Over the course of the trading period, the profit-taking strategy produced a substantially lower ROI of 26.39% and a total profit of $310763.74, compared to the first strategy's 46.44% ROI and total profit of $546938.05, resulting in a profit margin of $236174.31. This underperformance from the second strategy is likely due to selling shares too early before allowing them to increase further. Additionally, the first strategy's higher ROI is likely due to being able to accumulate more shares at lower prices, resulting in greater profit over time.

There were several key market events in 2023 that caused AMD's stock price to significantly increase (starting 2023 at $64.02 and ending at $147.41). The release of AI-specific processors such as the MI300 Series on January 4th, 2023 caused a large growth in AMD's data centre segment and served as a big advancement within the AI industry. The large increase in share price over the next month is likely due to this event, and served as an initial push upwards after a continual decrease over the previous year. This growth was also driven by AMD's collaboration with major tech companies such as Microsoft and Meta, expanding its market reach, subsequently increasing its customer base and revenue, thereby contributing to the overall uptrend in its share price.

Overall, it can be seen that these market events have positively impacted AMD's share price, resulting in a relatively high ROI (as per the first strategy), as such establishing AMD's strong market position within the increasingly competitive semiconductor industry.
