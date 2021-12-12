library('tibble')
library('readr')
library('dplyr')
library('tidyr')
library('stringr')
library('purrr')
library('ggplot2')


source(here::here('R/day_1.R'))
source(here::here('R/day_2.R'))
source(here::here('R/day_3.R'))
source(here::here('R/day_4.R'))
source(here::here('R/day_5.R'))
source(here::here("R/day_6.R"))
source(here::here("R/day_7.R"))
source(here::here("R/day_8.R"))
source(here::here("R/day_9.R"))


# Day 1 -------------------------------------------------------------------


input_1 <- read_lines(here::here('inputs/input_1.txt'))

input_1 %>%
	pure_find_increases()

input_1 %>%
	pure_find_increases_window()



# Day 2 -------------------------------------------------------------------


input_2 <- read_lines(here::here('inputs/input_2.txt'))

input_2 %>%
	apply_transition_cartesian()

input_2 %>%
	apply_transition_submarine()



# Day 3 -------------------------------------------------------------------


input_3 <- read_lines(here::here('inputs/input_3.txt'))

input_3 %>%
	pure_calculate_power_consumption()

input_3 %>%
	pure_calculate_life_support_rating()


# Day 4 -------------------------------------------------------------------



input_4 <- read_lines(here::here('inputs/input_4.txt'))

input_4 %>%
	pure_play_bingo() %>%
	pure_score_bingo()

input_4 %>%
	pure_play_bingo(.thats_playing_to_win_baby = FALSE) %>%
	pure_score_bingo()


# Day 5 -------------------------------------------------------------------


# input_5 <- read_lines(here::here('inputs/input_5.txt'))
#
# line_set <- input_5 %>%
# 	create_line_set(.diagonal_allowed = TRUE)
#
# vent_map <- line_set %>%
# 	create_vent_map()
#
# vent_map %>%
#   {sum(. >= 2)}


# Day 6 -------------------------------------------------------------------



input_6 <- read_lines(here::here('inputs/input_6.txt'))

input_6 %>%
        parse_lanternfish_counters() %>%
        count_lanternfish_pop_efficient(.after = 80)

input_6 |>
        parse_lanternfish_counters() |>
        count_lanternfish_pop_efficient(.after = 256) |>
        as.character()



# Day 7 -------------------------------------------------------------------


input_7 <- read_lines(here::here('inputs/input_7.txt'))


input_7 |>
        parse_crab_positions() |>
        find_minimal_shift()

input_7 |>
        parse_crab_positions() |>
        find_minimal_rocket_shift()


# Day 8 -------------------------------------------------------------------

input_8 <- read_lines(here::here('inputs/input_8.txt'))

input_8 |>
        parse_seven_segment_display() |>
        count_all_output_values()

# input_8 |>
#         parse_seven_segment_display() |>
#         find_and_apply_display_mapping() |>
#         pull(output_result) |>
#         sum()


# Day 9 -------------------------------------------------------------------

input_9 <- read_lines(here::here('inputs/input_9_test.txt'))


