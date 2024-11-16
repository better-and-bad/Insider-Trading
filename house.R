
######## BUSINESS INSIDER CONFLICTED CONGRESS ##########
bi <- readxl::read_excel("/Users/jackconnors/Downloads/conflicted_congress.xlsx",
                         sheet = "SCHEDULE B Transactions")

bi <- bi %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))

# Check the data structure
glimpse(bi)
library(dplyr)
library(stringr)


bi <- bi %>%
  filter(`Asset type` == "[ST]") %>%
  mutate(
    Company = str_extract(Asset, "(?<=⇒ ).*?(?= \\()"),
    Ticker = str_extract(Asset, "(?<=\\().*?(?=\\))")
  )

bi <- bi %>% 
  mutate(avg_price = (`Amount min` + `Amount max`) / 2) 

bi %>% 
  group_by(`Full name`) %>% 
  reframe(trade_value = sum(avg_price)) %>% 
  filter(trade_value > 50000) %>% 
  reframe(money = n_distinct(`Full name`))
n_distinct(`Full name`)

bi %>% 
  group_by(`Full name`) %>% 
  reframe(trade_value = sum(avg_price)) %>% 
  arrange(desc(trade_value))

tickers <- bi %>% 
  select(Ticker) %>% 
  distinct() %>% 
  pull(Ticker)

library(tidyquant)
library(quantmod)
library(purrr)
library(tidyverse)


### S&P
sp <- "^SPX"

sp_price <- getSymbols(sp, src='yahoo', 
                       from = "2020-01-01", to = "2020-12-31", 
                       auto.assign = TRUE, warnings = FALSE) %>%
  map(~Ad(get(.))) %>% reduce(merge) %>% `colnames<-`(sp)


sp_price <- as.data.frame(sp_price)

# Convert row names to a column
sp_price <- sp_price %>%
  rownames_to_column(var = "Date")

sp_return <- sp_price %>% 
  reframe(first = first(`^SPX`),
          last = last(`^SPX`)) %>% select(first, last) 

sp_return %>% 
  reframe(return = ((last - first) / first) *100) 

sp_return <- 14.6

# Function to safely get symbols
safe_getSymbols <- safely(getSymbols, otherwise = NULL)

# Fetch prices and filter out tickers that cause errors
valid_symbols <- tickers %>%
  map(~ safe_getSymbols(.x, src = 'yahoo', from = "2020-01-01", to = "2020-12-31", auto.assign = TRUE, warnings = FALSE)) %>%
  keep(~ !is.null(.x$result)) %>%
  map_chr(~ .x$result[[1]])

# Extract adjusted prices and ensure column names
prices <- valid_symbols %>%
  map(~ Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(valid_symbols)

library(tibble)
prices_df <- as.data.frame(prices) %>% 
  rownames_to_column(var = "Date")

# Convert Date column to Date type
prices_df$Date <- as.Date(prices_df$Date)

prices_df <- as.data.frame(prices) %>% 
  rownames_to_column(var = "Date") 
pivot_longer(-Date, names_to = "Ticker", values_to = "Price")

prices_df$Date <- as.Date(prices_df$Date)

colnames(prices_df) <- c("Date", "Ticker", "Price")

# Merge the data frames by Date and Ticker
merged_df <- merge(prices_df, bi, by = c("Date", "Ticker"))
########
party <- readxl::read_excel("/Users/jackconnors/Downloads/conflicted_congress.xlsx",
                            sheet = "party")

party_trim <- party %>% 
  separate(Name, into = c("Last", "First"), sep=",", remove=FALSE) %>% 
  mutate(First = str_trim(First),
         Last = str_trim(Last)) %>% 
  mutate(
    `Full name` = paste(First, Last)) %>%
  select(`Full name`, Party)


party_trim <- party_trim %>%
  mutate(`Full name` = str_to_upper(str_trim(`Full name`)))

# Merge with party_trim to bring in the Party column
merged_with_party <- merged_df %>%
  left_join(party_trim, by = "Full name")

merged_with_party %>% 
  group_by(`Full name`) %>% 
  reframe(purchases = sum(avg_price)) %>% 
  arrange(desc(purchases))


# Get the first and last prices for each Ticker and Date
### get the start and end prices of each stock per name
first_last <- merged_with_party %>% 
  group_by(`Full name`, Ticker) %>% 
  reframe(First = first(Price),
          Last = last(Price))

### multiply the fraction by the avg_price
merged_df_with_prices <- merged_with_party %>%
  left_join(first_last, by = c("Full name", "Ticker"))

## calculate returns
returns_df <- merged_df_with_prices %>%
  mutate(
    Invested_Amount = avg_price,
    Total_Return = (Last / First) * avg_price) %>%
  group_by(`Full name`) %>%
  reframe(
    Total_Invested = sum(Invested_Amount, na.rm = TRUE),
    Total_Return = sum(Total_Return, na.rm = TRUE),
    Total_Return_Percentage = (Total_Return / Total_Invested) * 100
  )

congress_returns <- returns_df %>% 
  mutate(adjusted_returns = Total_Return_Percentage - sp_return) %>% 
  select(`Full name`, adjusted_returns) %>% 
  arrange(desc(adjusted_returns))



merged_with_party <- congress_returns %>%
  left_join(party_trim, by = "Full name")

##### CONGRESSIONAL POTENTIAL RETURNS GRAPH ######
library(ggplot2)
merged_with_party %>%
  mutate(Party = case_when(
    `Full name` == "BRADLEY S. SCHNEIDER" ~ "D",
    `Full name` == "DON BEYER" ~ "D",
    `Full name` == "ADAM KINZINGER" ~ "R",
    `Full name` == "ALAN LOWENTHAL" ~ "D",
    `Full name` == "BOB GIBBS" ~ "R",
    `Full name` == "DAVID MCKINLEY" ~ "R",
    `Full name` == "JAMES R. LANGEVIN" ~ "R",
    TRUE ~ Party
  )) %>% 
  head(n=23) %>% 
  ggplot() +
  geom_col(aes(reorder(`Full name`, adjusted_returns), adjusted_returns, fill=Party)) +
  coord_flip() +
  scale_fill_manual(values = c("D" = "blue", "R" = "red")) +
  labs(x = " ", y = "Adjusted Returns %", fill = "Party",
       title = "Potential House Portfolio Returns", 
       subtitle="S&P Adjusted - 2020", caption="Source: Insider's Conflicted Congress") +
  theme(legend.position = "none")

