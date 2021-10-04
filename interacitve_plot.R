# Libraries ####
library(tidyverse)
library(rvest)
library(bbplot)
library(stringr)

# Vigil tt11846996
# Peaky Blinders tt2442560
# Line of Duty tt2303687
# Breaking Bad tt0903747

search_term <- readline(prompt="Enter show: ")
search_term <- "Breaking Bad"
# Add +
search_term <- gsub(" ", "+", search_term)
# Search URL: https://www.imdb.com/find?q=...+...
search_url <- paste("https://www.imdb.com/find?q=",search_term,sep = "")
# Read the HTML from the website
search_webpage <- read_html(search_url)

# Series code
# Scrape
series_code_html <- html_nodes(search_webpage,'tr.findResult.odd a')
# Convert to text
series_code_data <- substr(series_code_html[1], 17, 25)

# URL https://www.imdb.com/title/.../episodes
url <- paste("https://www.imdb.com/title/",series_code_data,"/episodes",sep="")
# Read the HTML from the website
webpage <- read_html(url)

# Remove unnecessary
rm(search_term)
rm(search_url)
rm(search_webpage)
rm(series_code_html)

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
num_data <- strtoi(str_sub(tail(html_text(num_data_html), n=1), start= -2))
num_data

df <- data.frame(
  season = character(),
  episode = character(),
  rating = character(),
  raters = character(),
  aired_date = character(),
  synopsis = character()
)

for (i in 1:num_data) {
  # URL for each season
  url <- paste("https://www.imdb.com/title/",series_code_data,"/episodes?season=",i,sep="")
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
  raters_data <- gsub('^.|.$', '', html_text(raters_data_html))
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
rm(series_code_data)
rm(url)
rm(i)

# Keep episodes in the right order
df$episode <- factor(df$episode, levels=unique(df$episode))

# Keep ratings in the right order (convert to numeric)
df$rating <- as.numeric(df$rating)

plot <- ggplot(data = df,
       aes(x = episode, y = rating, group = season, colour = season,
           text = paste(
             paste("Episode:", episode, sep = " "),
             paste("Rating:", rating, sep = " "),
             paste("Rated by: ", rater, sep = " "),
             sep = "\n"
           ))) +
  geom_point() +
  geom_line() +
  labs(
    title = paste(title_data),
    caption = "Source: IMDB. Ratings and episode names are as shown on IMDB."
  ) +
  scale_y_continuous(limits = c(0,10)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  bbc_style() +
  theme(
    plot.caption = element_text()
  )

# geom_text(data = rbind(df[which.min(df$rating), ], df[which.max(df$rating),]), 
#           aes(episode,round(rating), label=rating))

ggplotly(plot, tooltip = "text") 

# %>% layout(
#     annotations = list(x = 1.12, y = -0.22,
#                        text = "Source: IMDB. Ratings and episode names are as shown on IMDB.", 
#                        showarrow = F, xref='paper', yref='paper',
#                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
#                        font=list(size=15, color="grey")))









