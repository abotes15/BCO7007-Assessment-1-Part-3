knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(rtweet)
library(readtext)
library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)

# Part 2: Analysis & Visualisation

## Data import

table1<- read_csv("digitalmarketing_short_07_05_22.csv")
table2<- read_csv("digitalmarketing_short_14_05_22.csv")
table3<- read_csv("digitalmarketing_short_15_05_22.csv")

one <- table1
two <- table2
three <- table3

digitalmarketing_combine <- bind_rows(one, two, three, id = NULL) 

#look at first 10 obs
digitalmarketing_combine%>%
  head(10)

#look at structure
digitalmarketing_combine%>%
  str()

digitalmarketing_combine_short<-digitalmarketing_combine %>%
  select(user_id , screen_name , created_at , text , favorite_count, retweet_count, urls_expanded_url, hashtags, mentions_screen_name, location)

digitalmarketing_combine_short%>%write_csv("final_digitalmarketing_combine_short_20_05_22.csv")
digitalmarketing_combine<-read_csv("final_digitalmarketing_combine_short_20_05_22.csv")

## Explore frequency of tweets

ts_plot(digitalmarketing_combine, "hours") +
  labs(x = NULL, y = NULL,
       title = "Frequency of tweets with digitalmarketing",
       subtitle = paste0(format(min(digitalmarketing_combine$created_at), "%d %B %Y"), " to ", format(max(digitalmarketing_combine$created_at),"%d %B %Y")),
       caption = "digitalmarketing tweets Data collected from Twitter") +
  theme_minimal()

## Top tweeting location
### Most Twitter users turn off their location in their privacy settings but those that don’t add valuable location information to their tweets. We can count unique values of the “place_full_name” variable to obtain the most frequent tweet location.


digitalmarketing_combine_short %>% 
  filter(!is.na(location)) %>% 
  count(location, sort = TRUE) %>%
  top_n(10)

## Most frequently shared link

### The `urls_expanded_url` variable provides the full URL of shared links. Here we exclude tweets without a shared link, count, sort the frequency of links in descending order and print the top 5.

digitalmarketing_combine_short %>% 
  filter(!is.na(urls_expanded_url)) %>% 
  count(urls_expanded_url, sort = TRUE) %>%
  top_n(5)

## Most retweeted tweet

### `retweet_count` variable shows retweeting. We sort all the tweets in descending order by the size of the “retweet_count”, slice off the top row and print the date, handle, text and retweet count.

digitalmarketing_combine %>% 
  arrange(-retweet_count) %>%
  slice(20) %>% 
  select(created_at, screen_name, text, retweet_count)

## Embed tweets in the RMarkdown

### `gadenbuie/tweetrmd: Embed Tweets in R Markdown` - Easily embed Tweets anywhere R Markdown turns plain text into HTML.

### https://github.com/gadenbuie/tweetrmd

#devtools::install_github("gadenbuie/tweetrmd")
#devtools::install_github('rstudio/webshot2')


library(tweetrmd)
library(webshot2)
tweet_screenshot(tweet_url("Tanjilasmm", "1523884824758751232"))

## Most liked tweet

### To find the most liked tweet we can sort our tweets by the “favorite_count” variable in descending order and print the rows with the top 5 highest counts.

digitalmarketing_combine %>% 
  arrange(-favorite_count) %>%
  top_n(15, favorite_count) %>% 
  select(created_at, screen_name, text, favorite_count)

## Top tweeters

### To identify the most active tweeters we can use the “screen_name” variable to tot up the number of tweets by Twitter handle. We can then add back the @ symbol using the paste0() function.

digitalmarketing_combine %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(5) %>%
  mutate(screen_name = paste0("@", screen_name))

## Top emoji

### To identify the most frequently used emoji we can use the `ji_extract_all()` function from the `emo` package. 

### This function extracts all the emojis from the text of each tweet. 

###We can then use the unnest() function from the tidyr package to split out the emojis, count, sort in descending order and identify the top 10.


#devtools::install_github("hadley/emo")
library(emo)
digitalmarketing_combine %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(10)


## Top hashtags

### To pull out the hashtags from the text of each tweet we first need to convert the text into a one word per row format using the `unnest_tokens()` function from the `tidytext` package. 
###We then select only those terms that have a hashtag, count them, sort in descending order and pick the top 10.

library(tidytext)
digitalmarketing_combine %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"),
         hashtag != "#traditionalmarketing") %>%
  count(hashtag, sort = TRUE) %>%
  top_n(10)


## Top mentions

### ere we tokenise the text of each tweet and use `str_detect()` from the `tidyverse` package to filter out words that start with an @ .

digitalmarketing_combine %>% 
  unnest_tokens(mentions, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(mentions, "^@")) %>%  
  count(mentions, sort = TRUE) %>%
  top_n(10)


# Part 3: Topic Modeling & visualisation

## Preprocess text from dataset to tidy text & convert to DocumenttermMatric

### Clean up dataset text

tidy_digitalmarketing <- digitalmarketing_combine %>%
  mutate(text) %>%
  unnest_tokens(word , text) %>%
  anti_join(get_stopwords()) %>%
  filter(!is.na(word) & !grepl("[^A-Za-z]" , word) & word != "https" & word !="na")

tidy_digitalmarketing %>%
  count(word , sort = TRUE)

####Convert to DocumentTermMatrix


digitalmarketing_corpus <- Corpus(VectorSource(digitalmarketing_combine))
digitalmarketing_dtm <- DocumentTermMatrix(digitalmarketing_corpus)
digitalmarketing_dtm

### Use 'LDA()' function to create LDA model 

#set a seed so that the output of the model is predictable

digitalmarketing_lda <-  LDA(digitalmarketing_dtm , k=5 , control = list(seed = 1234))
digitalmarketing_lda

dm_topics <- tidy(digitalmarketing_lda , matrix = "beta")
```
### Visualise the top 10 words from each topic to brainstorm possible topics that they may cover

# install.packages("reshape2")

library(ggplot2)
library(dplyr)

dm_top_terms <- dm_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

dm_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

beta_wide <- dm_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic1> .001 | topic2> .001) %>%
  mutate(log_ratio = log2(topic2/topic1))

dm_topics <- tidy(digitalmarketing_lda , matrix = "gamma")

dm_top_terms <- dm_topics %>%
  group_by(topic) %>%
  slice_max(gamma, n = 10) %>%
  ungroup() %>%
  arrange(topic, -gamma)

dm_top_terms %>%
  mutate(term = reorder_within(document, gamma, topic)) %>%
  ggplot(aes(gamma, document, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

gamma_wide <- dm_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = gamma) %>%
  filter(topic1> .001 | topic2> .001) %>%
  mutate(log_ratio = log2(topic2/topic1))