# Here we are going to make some code that will use the selector
# web scraper and will look up a companies CIK by typing it 
# into the sec search link. From there the code will do another 
# loop to click into the specified file and open the document. 
# Then the text will be unnested

# web scrape and then save to csv to save time

library(tidyverse)
library(caret)  # For predictive modeling
library(keyATM)
library(quanteda)
library(tidyverse)
library(pdftools)
library(tidytext)
library(rvest)
library(ldatuning)
library(topicmodels)
library(tm)
library(XML)
library(dplyr)
library(ggplot2)
library(httr)
library(RSelenium)
library(netstat)
library(wdman)
library(curl)
library(rJava)

### Need to figure out html elements link with scraper

# Define the URL of the SEC page
sec_site <- "https://www.sec.gov/edgar/search/#/category=form-cat1&filter_forms=10-K"


driver <- rsDriver(browser = "firefox",
                   chromever = NULL,
                   verbose = F,
                   port = free_port())



#this line might not be needed
remDr <- driver[["client"]]
# remDr$open()

#get the list of podcast urls
remDr$navigate('https://www.sec.gov/edgar/search/#/category=form-cat1&filter_forms=10-K')






element <- remDr$findElement(using = 'css selector',
                             ".preview-file") # find selector for button
next_button <- remDr$findElement(using = 'css selector',
                                 ".btn-warning")

close <- remDr$findElement(using = 'css selector',
                           '#close-modal')


doc_scrape <- remDr$findElement(using = 'css selector',
                                'span')





test <- list()

for(i in 1:50) {
  # Navigate to the SEC search page
  remDr$navigate('https://www.sec.gov/edgar/search/#/category=form-cat1&filter_forms=10-K')
  Sys.sleep(0)  # Give time for the page to load
  
  # Find and click the i-th preview file link
  links <- remDr$findElements(using = 'css selector', ".preview-file")
  links[[i]]$clickElement()
  Sys.sleep(0)
  
  # Extract page source and find the document URL
  test[[i]] <- read_html(remDr$getPageSource()[[1]]) %>%
    html_elements("#open-file") %>%
    html_attrs() %>%
    tibble() %>%
    unnest(cols = c(.)) %>%  # Specify 'cols' to avoid warning
    filter(str_detect(., "https")) %>%
    pull(.) %>%
    unname()
  
  # Close the preview to return to the main page
  close_button <- remDr$findElement(using = 'css selector', ".close")
  close_button$clickElement()
  Sys.sleep(0)
}

# Print the test list to check extracted URLs
print(test)

# Function to predict stock price movement based on sentiment score
predict_stock_movement <- function(sentiment_df, stock_prices) {
  # Prepare data for modeling
  model_data <- stock_prices %>%
    left_join(sentiment_df, by = c("symbol" = "ticker")) %>%
    filter(!is.na(sentiment_score)) %>%
    select(date, close, sentiment_score)
  
  # Train a simple linear regression model
  model <- train(
    close ~ sentiment_score,
    data = model_data,
    method = "lm"
  )
  
  # Predict future stock prices
  predictions <- predict(model, newdata = model_data)
  
  # Add predictions to the data
  model_data <- model_data %>%
    mutate(predicted_close = predictions)
  
  return(model_data)
}

# Call the function and print predictions
predicted_data <- predict_stock_movement(sentiment_df, stock_prices)
print(predicted_data)

# Close the browser and server when done
remDr$close()
driver$server$stop()




urls <- unlist(test)

library(httr)
df<- list()
for(j in 1:length(urls)){
  x <- GET(urls[j], add_headers('user-agent' = 'student scraping ([[your email]])'))
  
  df[[j]] <- x %>%
    read_html() %>%
    html_elements("span") %>%
    html_text()
}


processed_df <- tibble(df) %>%
  mutate(name = urls) %>%
  unnest(df) %>%
  mutate(extracted_name = str_extract(name, "[^/]+(?=\\.htm$)")) %>%
  select(-name) 



analyze_df <- processed_df %>%
  unnest_tokens(word, df) %>%
  anti_join(stop_words, by = "word") 

sentiment_df <- analyze_df %>%
  inner_join(get_sentiments('bing'), by = 'word') %>%
  count(extracted_name, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative)

view(sentiment_df)





ggplot(sentiment_df, aes(x = extracted_name, y = sentiment_score, fill = sentiment_score > 0)) +
  geom_bar(stat = 'identity', color = 'black') +
  scale_fill_manual(values = c("TRUE" = "skyblue", "FALSE" = "salmon"), 
                    name = 'sentiment',
                    labels = c('NEGATIVE', 'POSITIVE')) +
  labs(
    title = "Sentiment Scores by Ticker",
    x = 'Ticker',
    y = 'Sentiment Score'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 16)
  )
  



# Top 10 most positive words
top_positive_words <- sentiment_df %>%
  filter(sentiment == "positive") %>%  # Filter positive words
  count(word, sort = TRUE) %>%         # Count occurrences
  slice_max(n, n = 10)                 # Get the top 10

# Top 10 most negative words
top_negative_words <- sentiment_df %>%
  filter(sentiment == "negative") %>%  # Filter negative words
  count(word, sort = TRUE) %>%         # Count occurrences
  slice_max(n, n = 10)                 # Get the top 10

# View results
top_positive_words
top_negative_words








# Join with sentiment lexicon to get individual word sentiments
word_sentiment_df <- analyze_df %>%
  inner_join(get_sentiments("bing"), by = "word")

# Top 10 most positive words
top_positive_words <- word_sentiment_df %>%
  filter(sentiment == "positive") %>%  # Filter positive words
  count(word, sort = TRUE) %>%         # Count occurrences
  slice_max(n, n = 10)                 # Get the top 10

# Top 10 most negative words
top_negative_words <- word_sentiment_df %>%
  filter(sentiment == "negative") %>%  # Filter negative words
  count(word, sort = TRUE) %>%         # Count occurrences
  slice_max(n, n = 10)                 # Get the top 10

# View results
top_positive_words
top_negative_words










# Count word occurrences by sentiment and extracted name
word_counts <- word_sentiment_df %>%
  group_by(extracted_name, sentiment, word) %>%
  count(sort = TRUE) %>%
  ungroup()

# Get top 10 positive and negative words for each extracted_name
top_words <- word_counts %>%
  group_by(extracted_name, sentiment) %>%
  slice_max(n, n = 5) %>%
  ungroup()

# Plot the top words by sentiment and name
ggplot(top_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~ extracted_name, scales = "free") +
  coord_flip() +
  scale_fill_manual(values = c("positive" = "skyblue", "negative" = "salmon")) +
  labs(
    title = "Top 10 Positive and Negative Words by Name",
    x = "Words",
    y = "Count",
    fill = "Sentiment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16)
  )










library(tidyquant)
library(ggplot2)
library(dplyr)

# Define tickers and sentiment scores
tickers <- c("SWKS", "PLXS", "CLFD", "SONO", "SPB")
sentiment_scores <- tibble(
  ticker = tickers,
  sentiment_score = c(3, 7, -2, 5, 0)  # Replace with your actual sentiment scores
)

# Fetch historical stock prices for the last 12 months
stock_prices <- tq_get(
  tickers,
  from = Sys.Date() - 365,
  to = Sys.Date(),
  get = "stock.prices"
)

# Get the closing price for each ticker at the end of the 12-month period
end_prices <- stock_prices %>%
  group_by(symbol) %>%
  slice_max(order_by = date, n = 1) %>%
  select(symbol, date, close)

# Combine sentiment scores with stock data
final_data <- sentiment_scores %>%
  rename(symbol = ticker) %>%  # Match column name with `end_prices`
  left_join(end_prices, by = "symbol")

# Plot stock prices and overlay sentiment scores
ggplot(stock_prices, aes(x = date, y = close, color = symbol, group = symbol)) +
  geom_line(linewidth = 1) +  # Use linewidth instead of size
  geom_point(data = final_data, aes(x = date, y = close, color = symbol), size = 3) +
  geom_text(data = final_data, aes(x = date, y = close, label = sentiment_score), 
            vjust = -1, hjust = 1.2, size = 4) +
  labs(
    title = "12-Month Stock Prices with Sentiment Scores",
    x = "Date",
    y = "Closing Price",
    color = "Ticker"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




















library(tidyquant)
library(ggplot2)
library(dplyr)

# Define tickers and sentiment scores
tickers <- c("SWKS", "PLXS", "CLFD", "SONO", "SPB")
sentiment_scores <- tibble(
  ticker = tickers,
  sentiment_score = c(3, 7, -2, 5, 0)  # Replace with your actual sentiment scores
)

# Fetch historical stock prices for the last 12 months
stock_prices <- tq_get(
  tickers,
  from = Sys.Date() - 365,
  to = Sys.Date(),
  get = "stock.prices"
)

# Get the closing price for each ticker at the end of the 12-month period
end_prices <- stock_prices %>%
  group_by(symbol) %>%
  slice_max(order_by = date, n = 1) %>%
  select(symbol, date, close)

# Combine sentiment scores with stock data
final_data <- sentiment_scores %>%
  rename(symbol = ticker) %>%  # Match column name with `end_prices`
  left_join(end_prices, by = "symbol")

# Create faceted plot
ggplot(stock_prices, aes(x = date, y = close)) +
  geom_line(aes(color = symbol), size = 1) +  # Line plot for stock prices
  geom_bar(
    data = final_data,
    aes(x = date + 30, y = sentiment_score, fill = symbol),  # Offset bar position
    stat = "identity",
    width = 20,
    alpha = 0.8
  ) +
  facet_wrap(~ symbol, scales = "free") +
  labs(
    title = "12-Month Stock Prices with Sentiment Scores",
    x = "Date",
    y = "Closing Price / Sentiment Score",
    color = "Stock Price",
    fill = "Sentiment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )











library(tidyquant)
library(ggplot2)
library(dplyr)

# Define tickers and sentiment scores (replace with actual data as needed)
tickers <- c("SWKS", "PLXS", "CLFD", "SONO", "SPB")
sentiment_scores <- tibble(
  ticker = tickers,
  sentiment_score = c(3, 7, -2, 5, 0)  # Replace with actual sentiment data
)

# Fetch historical stock prices for the last 12 months
stock_prices <- tq_get(
  tickers,
  from = Sys.Date() - 365,
  to = Sys.Date(),
  get = "stock.prices"
)

# Generate synthetic sentiment data over time (replace this with actual data if available)
set.seed(42)
sentiment_time_series <- stock_prices %>%
  group_by(symbol) %>%
  mutate(
    sentiment = cumsum(runif(n(), -1, 1)),  # Cumulative random sentiment changes
    sentiment_date = date
  ) %>%
  ungroup()

# Combine stock prices and sentiment data
combined_data <- stock_prices %>%
  left_join(sentiment_time_series %>% select(symbol, sentiment_date, sentiment), 
            by = c("symbol" = "symbol", "date" = "sentiment_date"))

# Create the plot
ggplot(combined_data, aes(x = date)) +
  geom_line(aes(y = close, color = "Stock Price"), size = 1) +
  geom_line(aes(y = sentiment, color = "Sentiment Score"), linetype = "dashed", size = 1) +
  facet_wrap(~ symbol, scales = "free_y") +
  labs(
    title = "Stock Prices and Sentiment Scores Over Time",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )










library(tidyquant)
library(ggplot2)
library(dplyr)

# Define tickers and sentiment scores
sentiment_df <- tibble(
  extracted_name = c("swks-20240927", "plxs-20240928", "clfd-20240930", "sono-20240928", "spb-20240930"),
  sentiment_score = c(66, 197, 191, 45, -34)  # Replace with your actual sentiment scores from `sentiment_df`
)

# Map tickers to extracted names
ticker_mapping <- tibble(
  extracted_name = c("swks-20240927", "plxs-20240928", "clfd-20240930", "sono-20240928", "spb-20240930"),
  ticker = c("SWKS", "PLXS", "CLFD", "SONO", "SPB")
)

# Join sentiment scores with ticker mapping
sentiment_df <- sentiment_df %>%
  left_join(ticker_mapping, by = "extracted_name")

# Fetch stock prices for the last 12 months
tickers <- sentiment_df$ticker
stock_prices <- tq_get(
  tickers,
  from = Sys.Date() - 365,
  to = Sys.Date(),
  get = "stock.prices"
)

# Combine stock prices with sentiment scores
stock_prices <- stock_prices %>%
  left_join(sentiment_df, by = c("symbol" = "ticker"))

# Create the plot with actual sentiment scores and predicted stock prices
predicted_data <- predict_stock_movement(sentiment_df, stock_prices)
ggplot(stock_prices, aes(x = date)) +
  geom_line(aes(y = close, color = "Stock Price"), size = 1) +  # Plot stock prices
  geom_hline(data = sentiment_df, aes(yintercept = sentiment_score, color = "Sentiment Score"), linetype = "dashed", size = 1) +  # Overlay sentiment scores
  geom_line(data = predicted_data, aes(y = predicted_close, color = "Predicted Price"), linetype = "dotted", size = 1) +  # Add predicted prices
  facet_wrap(~ symbol, scales = "free_y") +  # Separate panels for each ticker
  labs(
    title = "Stock Prices and Sentiment Scores Over Time",
    x = "Date",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



















## Topic Distribution by Company
# Extracts the topic distribution for each document (gamma values).
Joins topic distributions with company names.
# Visualizes the topic proportions for each company in a grouped bar chart.

```{r}

# Extract topic distribution for each document (gamma values)
topic_distribution <- tidy(lda_model, matrix = "gamma")

# Join with company names
topic_distribution <- topic_distribution %>%
  rename(company = document)  # Rename `document` column for clarity

# Plot topic distribution by company
topic_distribution %>%
  ggplot(aes(x = factor(topic), y = gamma, fill = company)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Topic Distribution by Company",
    x = "Topic",
    y = "Proportion (Gamma)",
    fill = "Company"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom"
  )

```






























# Get the top 10 extracted_name entries with the highest sentiment_score
top_extracted_names <- sentiment_df %>%
  arrange(desc(sentiment_score)) %>%
  slice_head(n = 6) %>%
  pull(extracted_name)

# Filter words for these top extracted_name entries
word_sentiment_top <- analyze_df %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(extracted_name %in% top_extracted_names) %>%
  count(extracted_name, word, sentiment, sort = TRUE) %>%
  group_by(extracted_name) %>%
  slice_max(n, n = 5) %>%  # Get top 10 words for each extracted_name
  ungroup()

# Create a faceted bar plot
ggplot(word_sentiment_top, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  facet_wrap(~ extracted_name, scales = "free_y", ncol = 2) +
  labs(
    title = "Top 5 Words for the 6 Tickers with Highest Sentiment Scores",
    x = "Words",
    y = "Frequency",
    fill = "Sentiment"
  ) +
  scale_fill_manual(values = c("positive" = "skyblue", "negative" = "salmon")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 7),
    legend.position = "top"
  )




























### Scrape the data to get the links.
pm_links <- read_html(remDr$getPageSource()[[1]]) %>%
  html_elements("a") %>%
  html_attrs() %>% 
  tibble() %>% 
  rename(text = ".") %>% 
  unnest(text) %>% 
  filter(str_detect(text,"transcript")) %>% 
  filter(str_detect(text,"https:")) 

# Next get the url link for the opened page
then use selector to scrape and save text data 
# then exit opened url and back to original for the loop. 

















sec_file <- sec_site %>%
  read_html() %>%
  html_elements("tbody") %>%
  html_attrs() %>% # change to text from attrs to pull text instead of links
  tibble() %>%
  rename(text = ".") %>%
  unnest(cols = text) %>%
  filter(!str_detect(text, "\n"))




# We will use this example from class to get what we need
#### Pulling urls using the webscraper for S&P 500 companies

wiki_link <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'

urls <- wiki_link %>%
  read_html() %>%
  html_elements("td:nth-child(2) a") %>%
  html_attrs() %>% # attrs to read links
  tibble() %>%
  rename(web = ".") %>%
  unnest(cols = web) %>%
  filter(str_detect(web, "wiki")) %>%  # Correctly filtering based on "web" and filtering for wiki results to get the right amount
  mutate(url = paste0("https://en.wikipedia.org/", web)) %>%  # Append to create the URL
  pull(url) %>%
  unique()


spnames <- wiki_link %>%
  read_html() %>%
  html_elements("td:nth-child(2) a") %>%
  html_text() %>%
  tibble() %>%
  filter(. != "Insulet Corporation") %>% # filter out the one without a link
  distinct() %>%
  pull(.) # pull to keep name as vector
spnames

# reading in the data from the first url of the urls variable
urls[1] %>%
  read_html() %>%
  html_elements("p+ ul li , p") %>%
  html_text () %>%
  tibble() %>%
  mutate(document = spnames[1]) # new column to match name of company

sp_list <- list() # blank list to fill

# lets expand this to a loop
for(i in 1:length(urls)){
  sp_list[[i]] <- urls[i] %>%
    read_html() %>%
    html_elements("p+ ul li , p") %>%
    html_text() %>%
    tibble() %>%
    rename(text = ".") %>%
    mutate(document = spnames[i])
}

sp_tibble <- sp_list %>% # binding the filled list into a tibble
  bind_rows()
sp_tibble










# web scrape and then save to csv to save time


#### Topic modeling
## This code will help to dtermine how many topics to use in your topic model

#install ldatuning
# Choosing number of topics
sp_dtm <- read_rds("sp_dtm.rds") ## 
result <- ldatuning::FindTopicsNumber(
  sp_dtm,
  topics = seq(from = 4, to = 10, by = 1),
  metrics = c("CaoJuan2009",  "Griffiths2004"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
ldatuning::FindTopicsNumber_plot(result)





### Here we will seed or tell the code some topics beforehand
# Key word assisted topic model, ie pushing it to some notion
data(keyATM_data_bills)

bills_keywords <- keyATM_data_bills$keywords # This will find the key words in each bill
#the keywords are in a list format

bills_dfm <- keyATM_data_bills$doc_dfm  
bills_dfm

# quanteda dfm object
keyATM_docs <- keyATM_read(bills_dfm) # turn to key atm object

# keyATM Base
out <- keyATM(docs = keyATM_docs,
              model = "base",
              no_keyword_topics = 5, #number of additional
              #topics to model
              keywords = bills_keywords)

top_words(out)
###

plot_topicprop(out, show_topic = 1:5)

top_docs(out)

plot_modelfit(out)

# example of making a list
list("complex" = c("programming","R","text","mining"),
     "stats" = c("anova","regression","p","value"))

#Try to do keyATM with the NASA data on Canvas
#with the following keywords
#1.Experiment: experiment, data, measurement
#2.Soil: soil, carbon, site
#3.Craft: propulsion, heat, system





