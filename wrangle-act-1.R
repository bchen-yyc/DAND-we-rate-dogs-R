library(tidyverse)

archive <- read_csv('raw-data/twitter-archive-enhanced.csv', col_types='ccccccccccdiccccc')

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

#Define a function to extract dog stage for each row
#Return NA if there are more than one stage
dog_stage <- function(stage_list) {
  stage_list_sorted <- str_sort(stage_list, locale="C")
  if (stage_list_sorted[3] != 'None') {
    return(NA)
  } else {
    return(stage_list_sorted[4])
  }
}

#Use apply to apply the `dog_stage` function over each row, create new column `stage` and drop NA in the column
archive <- archive %>%
  mutate(stage = apply(stage, 1, dog_stage)) %>%
  filter(!is.na(stage))

#Test
dim(filter(archive, doggo == 'doggo'))[1] == dim(filter(archive, stage == 'doggo'))[1]
dim(filter(archive, floofer == 'floofer'))[1] == dim(filter(archive, stage == 'floofer'))[1]

#Drop unnessery columns
archive <- archive %>%
  select(tweet_id, timestamp, rating_result, stage)

write_csv(archive, 'data/archive_clean.csv')
