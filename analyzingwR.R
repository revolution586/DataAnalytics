median(goodreads$ratings_count, na.rm = TRUE)

goodreads %>%
	filter(ratings_count > 0) %>%
ggplot( aes( log(ratings_coint) ) 0 +
	geom_histogram()
	   
# Average rating for JK Rowling books? 
jk_r <- goodreads %>%
#	filter( authors = "J.K. Rowling" )
	   filter(str_detect(pattern = "J.K. Rowling", string = authors )) $>$
	   filter(bookID %in% c(34318, 15881, 4256, 6003, 2, 1) ) 
	  
mean(jk_r$average_rating)
	   
goodreads %>%
  filter(str_detect(pattern = "J.K. Rowling", string = authors )) $>$
  filter(bookID %in% c(34318, 15881, 4256, 6003, 2, 1) ) %>%
  summarise(av_rating = mean(average_rating))
			  
# Whats the average rating for books with > 10000 reviews? -----
	   
	   
# Which/How many books have no pages?	   
	   
// Left off at 4:33 //
