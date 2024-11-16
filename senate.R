

######### SENATE TRADES ###########
senate <- readxl::read_excel("/Users/jackconnors/Downloads/senate_conflicted.xlsx",
                             sheet = "Part 4b Transactions")

senate <- senate %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))

# Check the data structure
glimpse(senate)
library(dplyr)
library(stringr)


### create full name
senate_gains <- senate %>% 
  mutate(full_name = paste(`First name`, `Last name`)) %>% 
  filter(`Transaction type` == "SALE (FULL)") %>% 
  mutate(max = as.numeric(`Amount max`),
         min = as.numeric(`Amount min`),
         avg_sale = (min + max) / 2) %>% 
  group_by(full_name) %>% 
  reframe(total_gains = sum(avg_sale)) %>% 
  arrange(desc(total_gains))

### Upload senate party
senate_party <- readxl::read_excel("/Users/jackconnors/Downloads/senate_conflicted.xlsx",
                                   sheet = "Sheet2")

senate_party <- senate_party %>%
  separate(Senator, into = c('full_name', 'state'), sep = " \\(", extra = "merge", fill = "right") %>%
  mutate(full_name = str_to_upper(str_trim(full_name)))

# Update the Party column using case_when
senate_party <- senate_party %>%
  mutate(Party = case_when(
    full_name == "BENJAMIN SASSE" ~ "R",
    full_name == "ANGUS KING" ~ "I",
    full_name == "ROBERT PORTMAN" ~ "R",
    TRUE ~ Party
  ))

# Print senate_party to check the updates
print(senate_party %>% filter(is.na(Party) == FALSE))

# Perform the left join
senate_gains <- senate_gains %>%
  left_join(senate_party, by = "full_name")

senate_gains %>% 
  na.omit() %>% 
  reframe(avg = mean(total_gains))
senate_gains %>%
  mutate(Party = case_when(
    full_name == "BENJAMIN SASSE" ~ "R",
    full_name == "ANGUS KING" ~ "I",
    full_name == "ROBERT PORTMAN" ~ "R",
    full_name == "CHRISTOPHER COONS" ~ "D",
    full_name == "JAMES RISCH" ~ "R",
    full_name == "ROGER MARSHALL" ~ "R",
    full_name == "JEFFREY MERKLEY" ~ "D",
    full_name == "TIMOTHY KAINE" ~ "D",
    full_name == "CHARLES GRASSLEY" ~ "R",
    full_name == "MICHAEL CRAPO" ~ "R",
    full_name == "DANIEL SULLIVAN" ~ "R",
    TRUE ~ Party)) %>% 
  head(n=23) %>% 
  ggplot(aes(reorder(full_name, total_gains), total_gains / 1e6, fill = Party)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, name = "Total Gains (in Millions)", breaks = seq(0, 16, by = 2)) +
  scale_fill_manual(values = c("D" = "blue", "R" = "red", "I" = "black")) +
  labs(x = "", y = "Total Gains (in Millions)", fill = "Party",
       title="Senate Income from Stock Sales", subtitle = 'Millions - 2020',
       caption = "Source: Insider's Conflicted Congress ") +
  theme(legend.position = "none")


# Print the result to verify
print(senate_gains %>% select(full_name, Party) %>% head())
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
merged_df %>% 
  group_by(`Full name`) %>% 
  reframe(purchases = sum(avg_price)) %>% 
  arrange(desc(purchases))

library(dplyr)

# Get the first and last prices for each Ticker and Date
### get the start and end prices of each stock per name
first_last <- merged_df %>% 
  group_by(`Full name`, Ticker) %>% 
  reframe(First = first(Price),
          Last = last(Price))

### multiply the fraction by the avg_price
merged_df_with_prices <- merged_df %>%
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

returns_df %>% 
  arrange(Total_Return_Percentage)
