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

source(here::here("R/day_6.R"))

input_6 <- read_lines(here::here('inputs/input_6_test.txt'))

lanternfish_counters <- input_6 |>
        strsplit(",") %>%
        {.[[1]]} %>%
        as.integer() |>
        map(~ 8 - .)

lifecycle_functions <- lanternfish_counters[1] |>
        map(offset_repro_function)



1:60 %>%
  enframe(name = NULL) %>%
  mutate(
    initial_pop = map_dbl(value, initial_function)
    , normal_pop = map_dbl(value, normal_function)
    , repro_pop = map_dbl(value, repro_function)
    , recursive_pop = map_dbl(value, repro_function_with_recursion)
    ) %>% #View()
  ggplot(aes(x = value)) +
  geom_line(aes(y = normal_pop), colour = 'steelblue') +
  geom_line(aes(y = initial_pop), colour = 'red') +
  geom_line(aes(y = repro_pop), colour = 'green') +
  geom_line(aes(y = recursive_pop), colour = 'orange')

test_frame <- tribble(
        ~day_num, ~fish_num
        , 1, 5
        , 2, 6
        , 3, 7
        , 4, 9
        , 5, 10
        , 6, 10
        , 7, 10
        , 8, 10
        , 9, 11
        , 10, 12
        , 11, 15
        , 12, 17
        , 13, 19
        , 14, 20
        , 15, 20
        , 16, 21
        , 17, 22
        , 18, 26
        , 19, 29
        , 20, 34
        )

test_frame |>
        mutate(calculated_pop = map_dbl(day_num, determine_population, lifecycle_functions))


test_frame_small <- tribble(
        ~day_num, ~fish_num
        , 1, 1
        , 2, 1
        , 3, 1
        , 4, 2
        , 5, 2
        , 6, 2
        , 7, 2
        , 8, 2
        , 9, 2
        , 10,2
        , 11,3
        , 12,3
        , 13,4
        , 14,4
        , 15,4
        , 16,4
        , 17,4
        , 18,5
        , 19,5
        , 20,7
        , 21,7
        , 22,8
        , 23,8
        , 24,8
        , 25,9
        , 26,9
        , 27,12
        , 28,12
        , 29,15
        , 30,15
)

test_frame_small |>
        mutate(calculated_pop = map_dbl(day_num, determine_population, lifecycle_functions)) |>
        ggplot(aes(x = day_num)) +
        geom_line(aes(y = fish_num), colour = 'steelblue') +
        geom_line(aes(y = calculated_pop), colour = 'orange')
