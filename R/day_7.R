parse_crab_positions <- function(.input_list){
        .input_list |>
                str_split(",") |>
                unlist() |>
                as.integer()

}

rocket_fuel_shift <- function(.x){
        sum(0:.x)

}

find_shift <- function(.shift, .crab_positions){
        (.shift - .crab_positions)  |>
                abs() |>
                sum()
}

find_rocket_shift <- function(.shift, .crab_positions){
        (.shift - .crab_positions)  |>
                abs() |>
                map_dbl(rocket_fuel_shift) |>
                sum()
}

find_minimal_shift <- function(.crab_positions){

        possible_positions <- min(.crab_positions):max(.crab_positions)

        optimal_right_shift <- possible_positions |>
                map_dbl(find_shift, .crab_positions) |>
                min()

        optimal_right_shift
}

find_minimal_rocket_shift <- function(.crab_positions){
        possible_positions <- min(.crab_positions):max(.crab_positions)

        optimal_right_shift <- possible_positions |>
                map_dbl(find_rocket_shift, .crab_positions) |>
                min()

        optimal_right_shift


}
