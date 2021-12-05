library('readr')
library('tibble')
library('dplyr')
library('tidyr')
library('stringr')
library('purrr') 

source(here::here('R/day_1.R'))

input_1 <- read_lines(here::here('inputs/input_1.txt'))

input_1 %>%
	pure_find_increases()

input_1 %>%
	pure_find_increases_window()


source(here::here('R/day_2.R'))

input_2 <- read_lines(here::here('inputs/input_2.txt'))

input_2 %>%
	apply_transition_cartesian()
 
input_2 %>% 
	apply_transition_submarine()


source(here::here('R/day_3.R'))

input_3 <- read_lines(here::here('inputs/input_3.txt'))

input_3 %>%
	pure_calculate_power_consumption()

input_3 %>%
	pure_calculate_life_support_rating()
