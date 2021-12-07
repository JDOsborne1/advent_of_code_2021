library('tibble')
library('readr')
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

source(here::here('R/day_4.R'))

input_4 <- read_lines(here::here('inputs/input_4.txt'))

input_4 %>% 
	pure_play_bingo() %>%
	pure_score_bingo()

input_4 %>% 
	pure_play_bingo(.thats_playing_to_win_baby = FALSE) %>%
	pure_score_bingo()

source(here::here('R/day_5.R'))

input_5 <- read_lines(here::here('inputs/input_5.txt'))

tic("create line set")
line_set <- input_5 %>%
	create_line_set() 
toc() 
tic("create vent map")
vent_map <- line_set %>%
	create_vent_map()
toc()
tic("find_result")
vent_map %>%
	{sum(.>=2)}
toc()
