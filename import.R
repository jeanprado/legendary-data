library(tidyverse)
library(lubridate)
library(rtweet)

query <- '#legendarymax -filter:retweets -filter:replies'

raw <- search_tweets(q = query, n = 40000, retryonratelimit = T)

raw %>% write_as_csv(paste0('data/legendary-', format(Sys.time(), "%F %Hh%m"), '.csv'))

# most mentioned houses
ignore_case <- function(x) regex(x, ignore_case=T)

tweets <- raw %>% select(user_id:text, favorite_count:hashtags, mentions_screen_name:lang,
                               place_name:country, status_url, followers_count, account_created_at:verified) %>% 
  mutate(house=str_extract_all(text, ignore_case('house of [\\w$]{2,}'))) %>% unnest(house, keep_empty=T)

tweets %>% mutate_at('house', tolower) %>% count(house, sort=T)

# most used words
library(tidytext)
library(stopwords)

words <- tweets %>% select(status_url, text) %>% unnest_tokens(word, text) %>% 
  count(word, sort=T) %>% filter(!word %in% c(stop_words$word, stopwords('pt'),
                                              'https', 't.co', 'hbomax', 'legendary', 'episódio',
                                              'legendarymax', 'legendaryhbo', 'episode',
                                              'é', 'pra', 'gt', 'ep', 'legendaryhbomax'))

words[1:15,] %>% ggplot(aes(n, fct_reorder(word, n))) + geom_col(fill="#8575E6") + theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = ggplot2::element_line(color="#eeeeee"),
        text=element_text(family="Helvetica", size=13),
        plot.title=element_text(family="Helvetica", size=15, face="bold")) +
  labs(x="", y="", title="15 most used words on #LegendaryMAX",
       caption = paste0("source: twitter. last updated: ", format(Sys.Date(), "%B %d %Y")))