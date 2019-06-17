library(tidyverse)
library(twitteR)
library(rjson)
library(tibble)

filepath <- 'raw-data/tweet_json.txt'

conn = file(filepath, "r")

lines <- readLines(conn)

i <- 1
for (line in lines) {
  #Convert line to a list
  json_list <- fromJSON(line)
  #Replace 'null' to 'NA' because the original file was done by Python
  for(j in 1:length(json_list)) {
    json_list[j] <- gsub('null', NA, json_list[j], ignore.case=TRUE)
  }
  #Convert each json record to tibble and append to tweet_json tibble
  if (i == 1) {
    tweet_json <- as_data_frame(json_list)
  } else {
    tweet_json <- union_all(tweet_json, as_data_frame(json_list))
  }
  i <- i+1
}

close(conn)

#Delete the three lines below!!!
tweet_json %>% 
  filter(id != id_str) %>% 
  View()

#Select necessery columns
tweet_json <- tweet_json %>% 
  select('id_str', 'favorite_count', 'retweet_count')

#Import archive_clean tibble
archive_clean <- read_csv('data/archive_clean.csv', col_types='ccdc')
glimpse(archive_clean)

tweet_merge <- left_join(archive_clean, tweet_json, by=c('tweet_id' = 'id_str'))
glimpse(tweet_merge)

#Test NA rows in `favorite_count` column 
tweet_merge$favorite_count %>% 
  is.na() %>% 
  sum()

#Drop those rows
tweet_merge <- tweet_merge %>% 
  filter(!is.na(favorite_count))

############################
#Process `img_predit` file
############################

#Download file from url
url <- "https://d17h27t6h515a5.cloudfront.net/topher/2017/August/599fd2ad_image-predictions/image-predictions.tsv"
download.file(url, 'raw-data/img_predit.tsv')

#Read it to tibble
img_predict <- read_tsv('raw-data/img_predit.tsv', col_types='ccicdlcdlcdl')
glimpse(img_predict)

#Replace `p1` elements with NA if p1_dog is FALSE
img_predict[!img_predict$p1_dog, 'p1'] <- NA

#Convert all `p1` elements to lower cases
img_predict$p1 <- tolower(img_predict$p1)

#Merge tibble img_predit to tweet_merge
tweet_merge <- left_join(tweet_merge, img_predict[,c('tweet_id', 'p1')], by='tweet_id') 

#Rename column `p1` to `breed`
colnames(tweet_merge)[7] <- 'breed'

write_csv(tweet_merge, 'data/tweet_merge.csv')

