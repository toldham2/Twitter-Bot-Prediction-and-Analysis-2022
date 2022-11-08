#### Libraries ####

library(academictwitteR)
library(tidyverse)
library(lubridate)

#### Get Data ####

options(scipen=999)

bot_data <- read.csv("/Users/tyleroldham/Documents/Data Viz Projects/Post-Elon Twitter Analysis/twitter_human_bots_dataset.csv")

bot_info <- get_user_profile(bot_data$id, bearer_token = get_bearer())

bot_data$id <- as.character(bot_data$id)

bot_info <- bot_info %>% 
  left_join(bot_data, by = "id")

bot_info$created_at <- as_datetime(bot_info$created_at)

bot_info <- bot_info %>%
  subset(
    select = c(
      id,
      username,
      name,
      description,
      location,
      url,
      protected,
      verified,
      created_at,
      public_metrics,
      account_type
    )
  )

#write.csv(bot_info, "/Users/tyleroldham/Documents/Data Viz Projects/Post-Elon Twitter Analysis/model_research_data.csv")

bot_info <- read.csv("/Users/tyleroldham/Documents/Data Viz Projects/Post-Elon Twitter Analysis/model_research_data.csv")

#### Calculations ####

bot_info <- bot_info %>%
  mutate(
    calc.account_age_days =
      trunc((created_at %--% now()) / days(1)),
    calc.tweets_per_day =
      public_metrics.tweet_count / calc.account_age_days,
    calc.flwr_to_flwg_ratio =
      public_metrics.followers_count / public_metrics.following_count,
    calc.username_num_count =
      str_count(username, pattern = "0|1|2|3|4|5|6|7|8|9")
  )

bot_info$calc.tweets_per_day[is.na(bot_info$calc.tweets_per_day)] = 0

bot_info$calc.flwr_to_flwg_ratio[is.infinite(bot_info$calc.flwr_to_flwg_ratio)] = 0

bot_info$calc.flwr_to_flwg_ratio[is.nan(bot_info$calc.flwr_to_flwg_ratio)] = 0

#### Thresholding ####

# 0 = bot, 1 = human
bot_info <- bot_info %>%
  mutate(score.account_age_days =
           case_when(calc.account_age_days >= 365 ~ 1,
                     calc.account_age_days < 365 ~ 0),
         score.tweets_per_day =
           case_when(calc.tweets_per_day == 0 ~ 0,
                     calc.tweets_per_day < 200  ~ 1,
                     calc.tweets_per_day >= 200 ~ 0),
         score.flwr_to_flwg_ratio =
           case_when(calc.flwr_to_flwg_ratio > 0.05 ~ 1,
                     calc.flwr_to_flwg_ratio <= 0.05 ~ 0,
                     is.nan(calc.flwr_to_flwg_ratio) ~ 0,
                     is.infinite(calc.flwr_to_flwg_ratio) ~ 0),
         score.username_num_count =
           case_when(calc.username_num_count < 5 ~ 1,
                     calc.username_num_count >= 5 ~ 0),
         score.bio =
           case_when(description != "" ~ 1,
                     description == "" ~ 0)
  )

bot_info <- bot_info %>% 
  mutate(final.bot_score =
           score.account_age_days+
           score.tweets_per_day+
           score.flwr_to_flwg_ratio+
           score.username_num_count+
           score.bio)

#### Matching ####

bot_info <- bot_info %>% 
  mutate(account_type =
           case_when(account_type == "bot" ~ 0,
                     account_type == "human" ~ 1))
bot_info <- bot_info %>% 
  mutate(match.account_age_days =
           score.account_age_days * account_type,
         match.tweets_per_day =
           score.tweets_per_day * account_type,
         match.flwr_to_flwg_ratio =
           score.flwr_to_flwg_ratio * account_type,
         match.username_num_count =
           score.username_num_count * account_type,
         match.bio =
           score.bio * account_type)

#### Optimizing ####

#mean(bot_info$match.account_age_days)
#mean(bot_info$match.tweets_per_day)
mean(bot_info$match.flwr_to_flwg_ratio)
#mean(bot_info$match.username_num_count)
#mean(bot_info$match.bio)

#### Modeling ####

var.test(calc.tweets_per_day ~ account_type, data=bot_info)
t.test(calc.tweets_per_day ~ account_type, data=bot_info, var.equal=FALSE)

var.test(calc.account_age_days ~ account_type, data=bot_info)
t.test(calc.account_age_days ~ account_type, data=bot_info, var.equal=TRUE)

var.test(calc.flwr_to_flwg_ratio ~ account_type, data=bot_info)
t.test(calc.flwr_to_flwg_ratio ~ account_type, data=bot_info, var.equal=FALSE)

model <- lm(account_type ~ calc.tweets_per_day + calc.account_age_days + calc.flwr_to_flwg_ratio + calc.username_num_count + score.bio, data=bot_info)

summary(model)

#### Testing ####

#Means
botsonly <- bot_info %>%
  filter(account_type == 0)

humanonly <- bot_info %>% 
  filter(account_type == 1)

mean(botsonly$calc.tweets_per_day)
mean(humanonly$calc.tweets_per_day)

#Bigrams, ngrams
#Nothing seemingly valuable here
library(tidytext)
library(tidyverse)

bot_ngrams <- bot_info %>% 
  filter(account_type == 0) %>% 
  unnest_tokens(input=description,
                output=word,
                token = "ngrams") %>% 
  count(word, sort = TRUE)

human_ngrams <- bot_info %>% 
  filter(account_type == 1) %>% 
  unnest_tokens(input=description,
                output=word,
                token = "ngrams") %>% 
  count(word, sort = TRUE)

bot_words <- bot_info %>% 
  filter(account_type == 0) %>% 
  unnest_tokens(input=description,
                output=word,
                token = "words") %>% 
  count(word, sort = TRUE)

human_words <- bot_info %>% 
  filter(account_type == 1) %>% 
  unnest_tokens(input=description,
                output=word,
                token = "words") %>% 
  count(word, sort = TRUE)
