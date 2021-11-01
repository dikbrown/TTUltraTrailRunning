# Get the Data
library(lubridate)
library(dplyr)
library(tibble)

rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
# race_year_id	double	    Race ID
# rank	        double	    Racer rank
# runner	      character	  Runner name
# time	        double	    Runner time, hour:minute:seconds
# age	          double	    Runner age
# gender	      character	  Runner gender (Man = M, Woman = W)
# nationality	  character	  Nationality of runner
rankings$gender <- factor(rankings$gender)

race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')
race$distance[race$distance == 0] <- NA
race$participants[race$participants == 0] <- NA

# race_year_id	  double	    Race ID for join
# event	          character	  Event name
# race	          character	  Race Name
# city	          character	  City name
# country	        character	  Country Name
# date	          double	    Date
# start_time	    double	    Start Time
# participation	  character	  Participation type
#     Solo, solor, team, relay
# distance	      double	    Distance traveled in Km
# elevation_gain	double	    Elevation gains in meters
# elevation_loss	double	    Eleveation loss in meters
# aid_stations	  double	    Aid station count
# participants	  double	    Total N of participants


race$total_delta <- race$elevation_gain - race$elevation_loss
race$weekday <- wday(race$date, label = TRUE)
race$race_year_id <- as.integer(race$race_year_id)


short <- race[race$distance < 10,] # all 74 records report zero distance
# based on manual analysis of 'short' df, most records have distance in event or race field
#     Only correct those records for which distance = 0 and there is a distance in the race field
race$distance[intersect(grep('100', race$race), which(race$distance == 0))] <- 161 
race$distance[intersect(grep('160', race$race), which(race$distance == 0))] <- 161
# Manually add two more
race$distance[race$race_year_id == 52109] <- 161  #distance in event name
race$distance[race$race_year_id == 42905] <- 172  #only 172k race - distance in race field
#There are still five races with no indication of actual distance

#now add these back into main df
empty <- race$race_year_id
race$distance

with(race, plot(aid_stations ~ distance))

# Look at number of races per runner
race_count <- summary(factor(tolower(rankings$runner)))
top_racers <- race_count[1:9] %>%  
  data.frame() %>%
  rownames_to_column()
  
names(top_racers) <- c('runner', 'count')
