library('readr')
library('tibble')
library('dplyr')
library('tidyr')

source(here::here('R/day_1.R'))

input_1 <- read_lines(here::here('inputs/input_1.txt'))

input_1 %>%
	pure_find_increases()

input_1 %>%
	pure_find_increases_window()

