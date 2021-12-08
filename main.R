library('tibble')
library('readr')
library('dplyr')
library('tidyr')
library('stringr')
library('purrr') 
library(ggplot2)


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

line_set <- input_5 %>%
	create_line_set(.diagonal_allowed = TRUE) 

vent_map <- line_set %>%
	create_vent_map()

vent_map %>%
  {sum(. >= 2)}



normal_function <- function(x){
    div <- 7
    
    x_mod <- max((x - min(x,9)),0)
  
    (x_mod - (x_mod %% div))/div
  
}

initial_function <- function(x){
  x_mod <- max(min(x, 9),0) 
  div <- 9
  
  (x_mod - (x_mod %% div))/div
  
}

repro_function <- function(x){
  
  1 + initial_function(x) + normal_function(x)
  
}
repro_function_with_recursion <- function(x){
  
  if (x > 1) {
    child_mod <- repro_function_with_recursion(x - 9)
  } else {
    child_mod <- 0
  }
  
  initial_function(x) + normal_function(x) + child_mod
  
}



1:60 %>% 
  enframe(name = NULL) %>% 
  mutate(
    initial_pop = map_dbl(value, initial_function)
    , normal_pop = map_dbl(value, normal_function)
    , repro_pop = map_dbl(value, repro_function)
    , recursive_pop = map_dbl(value, repro_function_with_recursion)
    ) %>% View()
  ggplot(aes(x = value)) + 
  geom_line(aes(y = normal_pop), colour = 'steelblue') + 
  geom_line(aes(y = initial_pop), colour = 'red') + 
  geom_line(aes(y = repro_pop), colour = 'green') +
  geom_line(aes(y = recursive_pop), colour = 'orange')
