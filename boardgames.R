library(tidyverse)

board_games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")
# 1)
# Which game from 1970-1979 had the highest average rating, with at least 1000 user ratings?
board_games %>%
	filter(year_published >= 1970 & year_published <= 1979 & users_rated >= 1000 )

board_games %>%
	filter (year_published %in% 1970:1979 & users_rated >= 1000 ) %>%
	arrange(desc(average_rating ))

# 2)
# Among games published in 2000 or later, which game(s) require the most number of players?
board_games %>%
	filter(year_published >- 2000)
	top_n(1, min_players)

# 3)
# Whats the average rating for games with a minimum playtime of over 2 hours? 

board_games %>%
	filter(min_playtime >= 120) %>%
	summarise(avg_rate - mean(average_rating))

over_two_hours <- board_games %>%
	filter(min_playtime >= 120)

mean(over_two_hours$average_rating)

# 4)
# Which year did games have the highest average minimum playtime?
# Hint: Use group_by() and summarize(0

board_games %>%
	group_by(year_published) %>%
	summarize(avg_min_playtime = mean(min_playtime)) %>%
	arrange(-avg_min_playtime)
	
