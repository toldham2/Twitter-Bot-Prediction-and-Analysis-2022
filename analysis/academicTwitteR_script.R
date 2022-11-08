#### Libraries ####

library(academictwitteR)
library(lubridate)
library(tidyverse)
library(ggthemes)

#### Process ####

bearer_token <- get_bearer()

username <- "TyKai53"

user_ID <- get_user_id(username)

# benchmark of user age

follower_list <- get_user_followers(user_ID, bearer_token)

follower_list$created_at <- as_datetime(follower_list$created_at)

clean_list <- follower_list %>%
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
      public_metrics
    )
  )

clean_list <- clean_list %>%
  mutate(
    calc.account_age_days =
      trunc((created_at %--% now()) / days(1)),
    calc.tweets_per_day =
      public_metrics$tweet_count / calc.account_age_days,
    calc.flwr_to_flwg_ratio =
      public_metrics$followers_count / public_metrics$following_count,
    calc.username_num_count =
      str_count(username, pattern = "0|1|2|3|4|5|6|7|8|9")
  )

# 0 = bot, 1 = human
clean_list <- clean_list %>%
  mutate(score.account_age_days =
           case_when(calc.account_age_days >= 365 ~ 1,
                     calc.account_age_days < 365 ~ 0),
         score.tweets_per_day =
           case_when(calc.tweets_per_day == 0 ~ 0,
                     calc.tweets_per_day < 50  ~ 1,
                     calc.tweets_per_day >= 50 ~ 0),
         score.flwr_to_flwg_ratio =
           case_when(calc.flwr_to_flwg_ratio > 0.05 ~ 1,
                     calc.flwr_to_flwg_ratio <= 0.05 ~ 0,
                     calc.flwr_to_flwg_ratio == NaN ~ 0,
                     calc.flwr_to_flwg_ratio == Inf ~ 0),
         score.username_num_count =
           case_when(calc.username_num_count < 5 ~ 1,
                     calc.username_num_count >= 5 ~ 0),
         score.bio =
           case_when(description != "" ~ 1,
                     description == "" ~ 0)
  )

clean_list <- clean_list %>% 
  mutate(final.bot_score =
           score.account_age_days+
           score.tweets_per_day+
           score.flwr_to_flwg_ratio+
           score.username_num_count+
           score.bio)

clean_list <- tibble::rowid_to_column(clean_list, "row_ID")

#### Plot ####

twitter_blue <- "#1DA1F2"

my_theme <-
  theme(
    text = element_text(family = "Helvetica", size = 8),
    axis.text.y = element_text(angle = 0, hjust = 1)
  )

clean_list %>%
  ggplot(aes(x = calc.account_age_days, y = ..density..)) +
  geom_density(fill = twitter_blue,
               color = twitter_blue,
               alpha = 0.8) +
  scale_y_continuous(name = "Density",
                     labels = scales::percent_format(.01)) +
  scale_x_continuous(name = "Account Age (Days)") +
  ggtitle(sprintf("%s | Account Age of Followers Distribution", username)) + theme_stata() + my_theme

maxrow <- max(clean_list$row_ID)
minrow <- min(clean_list$row_ID)

clean_list %>%
  ggplot(aes(x = row_ID, y = calc.account_age_days)) +
  geom_smooth(color = twitter_blue) +
  scale_x_reverse(name=" ", 
                  expand = c(0,0),
                  breaks=c(maxrow, minrow),
                  labels=c("Longest Follower", "Most Recent Follower")) +
  scale_y_continuous(name = "Account Age (Days)") +
  ggtitle(sprintf("%s | Account Age of Followers Over Time", username)) + theme_stata() + my_theme + theme(axis.text.x = element_text(hjust=c(0, 1)))

clean_list %>%
  ggplot(aes(x = row_ID, y = final.bot_score)) +
  geom_smooth(color = twitter_blue) +
  scale_x_reverse(name=" ", 
                  expand = c(0,0),
                  breaks=c(maxrow, minrow),
                  labels=c("Longest Follower", "Most Recent Follower")) +
  scale_y_continuous(name = "Bot Score") + coord_cartesian(ylim = c(0, 5)) +
  ggtitle(sprintf("%s | ", username)) + theme_stata() + my_theme + theme(axis.text.x = element_text(hjust=c(0, 1)))


