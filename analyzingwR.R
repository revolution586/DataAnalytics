library(tidyverse)
library(janitor)

job_posts <- read_csv("job_skills.csv") %>% clean_names()

goodreads <- read_csv("books.csv")

goodreads <- goodreads[-3349,]

#What is the average number of pages?
goodreads %>%
  filter(num_pages != 0) %>%
  summarise( av_pages = mean(num_pages, na.rm = TRUE ))

means(goodreads$num_pages, na.rm = TRUE)

# What is the median number of reviews? ------
median(goodreads$ratings_count, na.rm = TRUE)

goodreads %>%
  filter(ratings_count > 0) %>%
ggplot( aes( log(ratings_count) ) ) +
  geom_histogram() 

# What is the median number of reviews? -----
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
lots_ratings <- goodreads %>%
       filter(ratings_count > 10000)
       
means(lots_ratings$average_rating)
            
lots_ratings %>% 
       ggplot( aes(average_rating) ) + 
       geom_histogram()
       
 lots_ratings %>%
       filter(average_rating < 3) %>%
       select(title)
       
  goodreads %>%
       mutate(high_ratings = ratings_count > 10000 ) %>%
       ggplot( aes(average_rating, colour = high_ratings) ) + 
       geom_density()
       
# From books with no pages, which has the most ratings? --------
goodreads %>%
       filter(num_pages == 0) %>%
       arrange(-ratings_count)
       #filter(ratings_count == max(ratings_count))
       
# average rating by number of pages
goodreads %>%
       filter(ratings_count > 1500) %>%
       ggplot(aes(num_pages, average_rating)) + 
       geom_point()+
       geom_smooth(method = "lm")
     
# job postings ----------
       
job_posts %>%
     mutate(
	     needs_python = str_detect( tolower(minimum_qualifications) , "python") ) %>%
             wants_python = str_detect( tolower(preffered_qualifications), "python" ),
       	     has_python = needs_python | wants_python,
       	     double_count = needs_python & wants_python
       	         ) %>%
     filter(has_python) %>%
     count(category, sort = T )

r_match < - " r,| r | r//. "

job_posts %>%
     mutate(
	     needs_r = str_detect( tolower(minimum_qualifications) , r_match),
             wants_r = str_detect( tolower(preffered_qualifications), r_match),
       	     has_r = needs_r | wants_r,
       	     double_count = needs_r & wants_r
       	         ) %>%
     filter(has_r) %>%
     count(category, sort = T )



job_posts %>%
     mutate(
	     needs_sql = str_detect( tolower(minimum_qualifications) , "sql"),
             wants_sql = str_detect( tolower(preffered_qualifications), "sql" ),
       	     has_sql = needs_sql | wants_sql,
       	     double_count = needs_sql & wants_sql
       	) %>%
	count(needs_sql, wants_sql)
        # filter(has_sql) %>%
        # count(category, sort = T )



       
     
       
       

        
       
