library(tidyverse)
library(palmerpenguins)

penguins_global <- penguins

data_analyst <- read_csv("DataAnalyst.csv")



# islands and species ---------------
penguins_global $>$
	ggplot(aes(species, fill = island)) +
	geom_bar()

penguins_global %>%
	ggplot(aes(island, fill = species)) +
	geom_bar()

# species and body mass --------

penguins_global %>%
	ggplot(aes(body_mass_g, colour = species)) +
	geom_density(alpha = .3)

penguins_global %>%
	ggplot(aes(x = species, y = body_mass_g, colour = sex ))+
	geom_boxplot()

# Adelie penguins by island ----------

adelie_pens <- filter(penguins_global, species = "Adelie")

adelie_pens %>%
	ggplot(aes(body_mass_g, colour = island))+
	geom_density()

t.test( filter(adelie_pens, island == "Dream") $body_mass_g,
	   filter(adelie_pens, island == "Briscoe") $body_mass_g )

# Data Analyst ----

library(janitor)
data_analyst <- data_analyst %>% clean_names()

data_analyst %>%
	filter( str_detect( tolower(job_description), "python") )

data_analyst %>%
	filter( str_detect( tolower(job_description) , " r | r/.") )


lan_flag <- data_analyst %>%
	mutate( has_python = str_detect( tolower(job_description) , "python"),
		   has_r = str_detect( tolower(job_description) , " r | r/.") )


clean_data <- lan_flag %>%
	filter (salary_estimate != "-1") %>%
	mutate(salary_estimate2 = str_remove_all(salary_estimate, "[(Glassdoor est.)]"),
		   salary_estimate2 = str_remove_all(salary_estimate2, "[$]"),
		   salary_estimate2 = str_remove_all(salary_estimate2, "K")
		   ) %>%
	separate(color = salary_estimate2, into = c("lower_bound", "upper_bound"), sep = "-", convert = TRUE)

clean_data %>%
	ggplot(aes(upper_bound, colour = has_r))+
	geom_density()

clean_data %>%
	filter(rating != "-1") %>%
	ggplot(aes(revenue, upper_bound))+
	geom_point() +
	geom_smooth()

clean_data %>%
	filter(sector != "-1") %>%
	mutate(has_tab = str_detect(tolower(job_description), "tableau"),
		   has_excel = str_detect(tolower(job_description), "excel"),
		   has_vba = str_detect(tolower(job_description), "vba"),
		   has_none = sum(has_python, has_r, has_tab, has_excel, has_vba	)
	pivot_longer(contains("has_"), names_to = "technology") %>%
	group_by(sector, technology) %>%
	summarise(num_jobs = sum(value)) %>%
	ggplot(aes(sector, num_jobs, fill = technology))+
	geom_col(position = "dodge")+ scale_x_discrete(labels = abbreviate)
