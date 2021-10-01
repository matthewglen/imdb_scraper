# Libraries ####
library(tidyverse)
library(rvest)
library(bbplot)
library(stringr)

# Vigil tt11846996
# Peaky Blinders tt2442560
# Line of Duty tt2303687
# Breaking Bad tt0903747

# URL https://www.imdb.com/title/.../episodes
series_code <- 'tt2442560'
url <- paste("https://www.imdb.com/title/",series_code,"/episodes",sep="")
# Read the HTML from the website
webpage <- read_html(url)

# Series title
# Scrape
title_data_html <- html_nodes(webpage,'div.parent h3 a')
# Convert to text
title_data <- html_text(title_data_html)
title_data

# Number of series
# Scrape the most recent series (default on page load)
num_data_html <- html_nodes(webpage,'div.clear strong')
# Convert to text. Remove "season". Convert to integer.
num_data <- strtoi(substr(tail(html_text(num_data_html), n=1), 8, 8))
num_data

df <- data.frame(
  season = character(),
  episode = character(),
  rating = character(),
  rater = character(),
  aired_date = character(),
  synopsis = character()
)

for (i in 1:num_data) {
  # URL for each season
  url <- paste("https://www.imdb.com/title/",series_code,"/episodes?season=",i,sep="")
  webpage <- read_html(url)
  
  # Series number
  # Scrape
  series_data_html <- html_nodes(webpage,'div.clear h3')
  # Convert to text
  series_data <- html_text(series_data_html)
  series_data
  
  # Episode names
  # Scrape
  episode_data_html <- html_nodes(webpage,'div.info strong')
  # Convert to text
  episode_data <- html_text(episode_data_html)
  episode_data
  
  # Episode rating
  # Scrape
  rating_data_html <- html_nodes(webpage,'div.ipl-rating-star.small span.ipl-rating-star__rating')
  # Convert to text
  rating_data <- html_text(rating_data_html)
  rating_data
  
  # Episode rating
  # Scrape
  raters_data_html <- html_nodes(webpage,'div.ipl-rating-star.small span.ipl-rating-star__total-votes')
  # Convert to text. Brackets remain, will leave them for now, but might come back
  raters_data <- html_text(raters_data_html)
  raters_data
  
  # Release date
  # Scrape
  date_data_html <- html_nodes(webpage,'div.airdate')
  # Convert to text. Do some string tidying.
  date_data <- str_trim(str_replace_all(html_text(date_data_html), "[\n]" , ""))
  date_data
  
  # Synopsis
  # Scrape
  synopsis_data_html <- html_nodes(webpage,'div.item_description')
  # Convert to text. Do some string tidying.
  synopsis_data <- str_trim(str_replace_all(html_text(synopsis_data_html), "[\n]" , ""))
  synopsis_data
  
  this_season <- data.frame(
    season = series_data,
    episode = episode_data,
    rating = rating_data,
    rater = raters_data,
    aired_date = date_data,
    synopsis = synopsis_data
  )
  
  df <- rbind(df,this_season)
}

# Remove unnecessary data
rm(date_data_html)
rm(episode_data_html)
rm(num_data_html)
rm(raters_data_html)
rm(rating_data_html)
rm(series_data_html)
rm(synopsis_data_html)
rm(title_data_html)
rm(webpage)
rm(this_season)
rm(series_data)
rm(episode_data)
rm(rating_data)
rm(raters_data)
rm(date_data)
rm(synopsis_data)
rm(num_data)
rm(series_code)
rm(url)
rm(i)

# Keep episodes in the right order
df$episode <- factor(df$episode, levels=unique(df$episode))

ggplot(data = df,
       aes(x = episode, y = rating, group = season, colour = season)) +
  geom_point() +
  geom_line() +
  labs(
    title = paste(title_data)
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  bbc_style()








