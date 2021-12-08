# normal_function <- function(x){
#         div <- 7
#
#         x_mod <- max((x - min(x,9)),0)
#
#         (x_mod - (x_mod %% div))/div
#
# }
#
# initial_function <- function(x){
#         x_mod <- max(min(x, 9),0)
#         div <- 9
#
#         (x_mod - (x_mod %% div))/div
#
# }
#
# repro_function <- function(x){
#
#         1 + initial_function(x) + normal_function(x)
#
# }
# repro_function_with_recursion <- function(x){
#
#         if (x > 1) {
#                 child_mod <- repro_function_with_recursion(x - 9)
#         } else {
#                 child_mod <- 0
#         }
#
#         initial_function(x) + normal_function(x) + child_mod
#
# }
#
# offset_repro_function <- function(.offset){
#         function(x){
#                 repro_function_with_recursion(x + .offset)
#         }
# }
#
# determine_population <- function(x, .lifecycle_functions){
#         pop_output <- .lifecycle_functions |>
#                 map_dbl(meta_functional_application, x) |>
#                 sum()
#
#         length(.lifecycle_functions) + pop_output
# }


iterate_lanternfish_lifecycle <- function(.lanterns){
        new_fish <- sum(.lanterns == 0)

        new_lanterns <- .lanterns - 1
        new_lanterns[new_lanterns < 0] <- 6

        new_lanterns <- new_lanterns |>
                append(rep(8,new_fish))

        new_lanterns
}

parse_lanternfish_counters <- function(.input_list){
        input_6 %>%
                strsplit(",") %>%
                {.[[1]]} %>%
                as.integer()
}
count_lanternfish_pop <- function(.lanternfish_counters, .after, .raw_return = FALSE){
        lantern_pop <- .lanternfish_counters

        for(i in 1:.after){
                lantern_pop <- lantern_pop |>
                        iterate_lanternfish_lifecycle()
        }
        if(.raw_return){
                lantern_pop
        } else {
        length(lantern_pop)
        }


}
count_lanternfish_pop_efficient <- function(.lanternfish_counters, .after){
        lantern_pop <- .lanternfish_counters |>
                enframe(name = NULL) |>
                count(value) |>
                right_join(tibble(value = 0:8), by = 'value') |>
                mutate(across(n, replace_na, 0)) |>
                arrange(value)


        for (i in 1:.after) {
                breeding_pop <- lantern_pop |>
                        filter(value == 0) |>
                        pull(n)

                lantern_pop <- lantern_pop |>
                        mutate(
                                n = lead(n)
                        )
                lantern_pop[9,2] <- breeding_pop
                lantern_pop[7,2] <- lantern_pop[7,2] + breeding_pop

        }

        lantern_pop |>
                pull(n) |>
                sum()

}

