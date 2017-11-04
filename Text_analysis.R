library(tidyverse)
library(tm)
library(syuzhet)
library(wordcloud2)
library(ggplot2)
library(stringr)
library(tidytext)


# load .txt file

### replace 'YOUR_TXT_FILE' with your file path
text_df <- tibble(text = readLines('cheela.txt')) %>% 
  #  slice(4:n()) %>% 
  mutate(time = as.Date(str_match(text, "(?:(?!,(?-).).)*"), "%d/%m/%y"),
         name = str_extract(as.character(str_match(text,"-.*:")), "[A-Za-z']+"), 
         body = as.character(str_match(text, "[^:]*$"))) %>% 
  select(-text)

text_df

# tokenize data and remove NA's
text_df <- text_df %>% 
  unnest_tokens(word,body) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  na.omit()


text_df

# create vector of custom stop words and remove
custom_stop <- bind_rows(data_frame(word = c("hai","acha","de","kuch","rha",
                                             "ye", "na" , "kya" , "pe" , "se",
                                             "to" , "bhi" , "le" , "ko" , "apni",
                                             "sb" , "koi" , "nhe" ,"btao","batao","aa",
                                             "pooo" , "kr","hn" ,"ke" , "bhe",
                                             "aise", "aur" ,"aajkl","ni"),
                                    lexicon = c("custom")),stop_words)

text_df <- text_df %>% anti_join(custom_stop)

text_df

# top 10 words by person
text_df %>% 
  count(name,word, sort = TRUE) %>% 
  arrange(name, -n) %>% 
  group_by(name) %>%
  mutate(rank = seq(1:length(name))) %>% 
  ungroup() %>% 
  filter(rank <= 5) %>% 
  mutate(word = reorder(word, n)) %>% 
  arrange(name, -n) %>% 
  ggplot(aes(word, n, fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  labs(y = NULL, x = NULL, title = "Top 10 Words by Person") +
  coord_flip() +
  theme(axis.text = element_text(size = 8),
        strip.text.x = element_text(size = 8))
ggsave("top15.png")



######################
# SENTIMENT ANALYSIS #
######################

# calculate overall sentiment scores using afinn methodology
# (a specific model is built for wherer key words are given 
#  different localised values and an overall modd of the sentence or conversations
#  is decieved based on these scores. )

text_df %>% 
  group_by(name) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  summarise(total = mean(score)) %>% 
  mutate(name = reorder(name, total)) %>% 
  arrange(desc(total)) %>% 
  ggplot(aes(name, total, fill = name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme(axis.text = element_text(size = 14))
ggsave("overall.png")

# create tibble for visualizations

sent <- text_df %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(time, name) %>% 
  mutate(sentiment = mean(score))

p <- sent %>% 
  ggplot(aes(as.Date(time), sentiment)) +
  geom_point(aes(colour = as.factor(name)), alpha = .3) +
  geom_smooth(aes(colour = as.factor(name)), se = FALSE) +
  facet_wrap(~name, ncol = 2) +
  labs(x = "", y = "") +
  theme(axis.text = element_text(size = 14),
        legend.position = "none",
        strip.text.x = element_text(size = 16)) 

ggplotly(p)

p2 <- sent %>%
  ggplot(aes(as.Date(time), sentiment)) +
  geom_point(aes(colour = as.factor(name)), alpha = .15) +
  geom_smooth(aes(colour = as.factor(name)), se = FALSE) +
  labs(x = "Month", y = "Mean Daily Sentiment Score")  +
  theme(axis.text = element_text(size = 16))
ggplotly(p2)
ggsave("p2.png")


##############
# wordclouds #
##############
text_df %>% 
  count(word) %>% 
  wordcloud2(shape ="square")
