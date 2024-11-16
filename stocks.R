

### LABOR ECONOMICS
library(tidyverse)
library(fredr)
library(tsibble)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyquant)
library(lubridate)


### SET PLOT THEME ###
theme_cx <- function(){
  theme_light() +
    theme(
      axis.text = element_text(size=14),
      axis.title.x = element_text(size=14, margin = margin(t=12)),
      axis.text.x = element_text(hjust = 1, size=14),  # Rotate x-axis labels by 45 degrees
      axis.title.y = element_text(size=14, margin = margin(r=12)),
      plot.title = element_text(size=18, face="bold", margin = margin(b=10),
                                hjust=0.5),
      plot.subtitle = element_text(size=14, face="bold", margin = margin(b=10),
                                   hjust=0.5),
      legend.text = element_text(size = 16),  # Increase the size of legend text
      
    )
}

### set the theme
theme_set(theme_cx())

#### DATA Upload ####
insider_trading <- readr::read_csv("/Users/jackconnors/Downloads/house_trades.csv")

### clean data
insider_trading <- insider_trading %>% 
  mutate(date= mdy(transaction_date)) %>% 
  rename(symbol = ticker) %>% 
  select(-transaction_date)

### unqiue dates and ticker
unique_tickers_dates <- insider_trading %>%
  select(symbol, date) %>%
  filter(symbol != "--") %>% 
  distinct() %>% 
  na.omit()

### all unique stock ppl bought
tix <- unique(unique_tickers_dates$symbol)

tickers <- tq_get(tix, get="stock.prices")

dim(tickers)

insider_trading <- insider_trading %>% 
  merge(tickers, by=c('date', 'symbol'))

library(stringr)
insider_trading <- insider_trading %>% 
  separate(amount, into=c("lower","higher"), sep="-", remove = FALSE) %>%
  mutate(
    lower = as.numeric(gsub("[\\$,]", "", lower)),
    higher = as.numeric(gsub("[\\$,]", "", higher)),
    avg_price = (lower + higher)/2)

politicians <- insider_trading %>% 
  select(date, owner, adjusted, symbol, avg_price, representative, party, state, type)

# Filter purchases and calculate shares
purchases <- politicians %>%
  filter(type == "purchase") %>%
  mutate(shares = avg_price / adjusted)

sales <- politicians %>%
  filter(grepl("sale", type)) %>%
  mutate(sale_factor = ifelse(type == "sale_partial", 0.20, 1))

### Calculate returns for each sale and match with corresponding purchase(s)
returns <- sales %>%
  left_join(purchases, by = c("symbol", "representative", "owner"), suffix = c("_sale", "_purchase")) %>%
  filter(date_sale > date_purchase) %>%
  group_by(symbol, representative, owner, date_sale) %>%
  summarize(
    sale_value = sum(shares * adjusted_sale * sale_factor, na.rm = TRUE),
    investment_cost = sum(shares * adjusted_purchase * sale_factor, na.rm = TRUE),
    return = sale_value - investment_cost,
    party = first(party_sale)) %>%
  ungroup()

### cumulative returns
cumulative_returns <- returns %>%
  group_by(representative, date_sale, party) %>%
  summarize(
    daily_return = sum(return, na.rm = TRUE),
    .groups = 'drop') %>%
  arrange(representative, date_sale) %>%
  group_by(representative) %>%
  mutate(cumulative_return = cumsum(daily_return)) %>%
  ungroup() %>%
  rename(date = date_sale, party = party)

### Ensure all senators are included in the plot, even those without sales
all_representative <- unique(fucks$representative)

### fill for dates
cumulative_returns <- cumulative_returns %>%
  complete(representative = all_representative, date = seq(min(politicians$date), max(politicians$date), by = "day")) %>%
  fill(cumulative_return, party, .direction = "downup")

### quick look at returns per rep
cumulative_returns %>% 
  group_by(representative) %>% 
  summarize(total_return = sum(cumulative_return)) %>% 
  arrange(desc(total_return)) %>%
  head(n=18) %>% 
  ggplot() +
  geom_col(aes(reorder(representative, total_return), total_return)) +
  scale_y_log10() +
  coord_flip()

### Plot the cumulative returns over time
ggplot(cumulative_returns, aes(x = date, y = cumulative_return, color = party)) +
  geom_line() +
  labs(title = "Cumulative Portfolio Returns Over Time by House Reps", x = "Date", y = "Cumulative Return") +
  theme_minimal() +
  facet_wrap(~representative, scales = "free_y") +
  theme(legend.position = "right")

returns %>% filter(representative == "Nancy Pelosi")
