library(tidyverse)

archive <- read_csv('raw-data/twitter-archive-enhanced.csv')

glimpse(archive)

#Convert 'tweet_id' to str
archive$tweet_id <- as.character(archive$tweet_id)

#Convert 'rating_numerator' and 'rating_denominator' to integers
archive$rating_numerator <- as.integer(archive$rating_numerator)
archive$rating_denominator <- as.integer(archive$rating_denominator)

#Check again
glimpse(archive)

#Drop retweeted rows
archive <-archive %>%
  filter(is.na(archive$retweeted_status_id))

#Make sure only NA value remaining
archive$retweeted_status_id %>% unique()

#Drop rows without pictures
archive <- archive %>%
  filter(grepl(".*,?https://twitter.com/dog_rates/status/[0-9]{18}/photo/1,?.*", archive$expanded_urls, ignore.case=TRUE))

#Make sure all rows contain image url, this should read TRUE
all(grepl(".*,?https://twitter.com/dog_rates/status/[0-9]{18}/photo/1,?.*", archive$expanded_urls, ignore.case=TRUE))

#Check if any timestamp is after 2017-08-01
#Should read FALSE
any(archive$timestamp > '2017-08-02')

#Check those odd denominotor
archive %>%
  filter(rating_denominator != 10) %>%
  select(tweet_id, text, rating_numerator, rating_denominator) %>%
  View()

#Drop denominators which are not multiples of 10
archive <- archive %>%
  filter((archive$rating_denominator %% 10) == 0)

#Test, it shoud output TRUE
all((archive$rating_denominator %% 10) == 0)

#Create a rating_result column
archive <- archive %>%
  mutate(rating_result = rating_numerator / rating_denominator) 

#Check 'rating_result' value_counts
archive$rating_result %>%
  table()

archive %>%
  filter(rating_result >= 2.6) %>%
  View() #looks like those are parsed wrong or absurd rating

#Drop those rows
archive <- archive %>%
  filter(rating_result < 2.6)

stage <- archive %>%
  select(doggo, floofer, pupper, puppo)

gsub('None', NA, stage$doggo)
