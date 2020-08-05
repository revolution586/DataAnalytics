library(tidyverse)
library(readxl)
library(janitor)

pth <- "C:\\Users\\Jake\\Downloads\\StudyData.xlsx"

hst_425 <- read_excel(pth, skip = 2, sheet = "HST425") %>% clean_names()
ecn_189 <- read_excel(pth, skip = 2, sheet = "ECN189") %>% clean_names()
ecn_104 <- read_excel(pth, skip = 2, sheet = "ECN104") %>% clean_names()


remove_minutes <- function(x) str_remove_all(x, " Minutes")

dash_to_zero <- function(x) str_replace_all(x, "-", "0")


hst_425_clean <- hst_425 %>% 
  apply(2, remove_minutes ) %>% 
  apply(2, dash_to_zero) %>% 
  apply(2, replace_na, "0" ) %>% 
  apply(2, as.numeric ) %>% 
  as_tibble() %>% 
  mutate(course = "HST425")


ecn_189_clean <- ecn_189 %>% 
  apply(2, remove_minutes ) %>% 
  apply(2, dash_to_zero) %>% 
  apply(2, replace_na, "0" ) %>% 
  apply(2, as.numeric ) %>% 
  as_tibble() %>% 
  mutate(course = "ECN189")


ecn_104_clean <- ecn_104 %>% 
  apply(2, remove_minutes ) %>% 
  apply(2, dash_to_zero) %>% 
  apply(2, replace_na, "0" ) %>% 
  apply(2, as.numeric ) %>% 
  as_tibble() %>% 
  mutate(course = "ECN104")


full_clean_data <- hst_425_clean %>% 
  bind_rows(ecn_189_clean) %>% 
  bind_rows(ecn_104_clean) %>% 
  pivot_longer(c(-course, -day_s), names_to = "study_round", values_to = "exercise_time")


full_clean_data

