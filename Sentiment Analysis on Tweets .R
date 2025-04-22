install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
library(dplyr)
library(tidytext)
library(ggplot2)

tweets=data.frame(status_id = 1:5,text = c(
  "I love learning R programming!",
  "This is so frustrating ",
  "What a great day to analyze data!",
  "I hate bugs in my code.",
  "R makes data visualization so easy."
)
)

tweet_words <- tweets %>%unnest_tokens(word, text)
data("stop_words")
clean_words <- tweet_words %>%anti_join(stop_words)
sentiment <- clean_words %>% inner_join(get_sentiments("bing"))


print(sentiment)
sentiment %>%count(sentiment) %>% ggplot(aes(x = sentiment, y = n, fill = sentiment)) + geom_col() +labs(title = "Sentiment Analysis of Sample Tweets",x = "Sentiment", y = "Word Count") +theme_minimal()