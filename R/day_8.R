
parse_seven_segment_display <- function(.input_lines) {
        .input_lines |>
                enframe(name = NULL
                        , value = 'input_lines') |>
                mutate(
                        signal = str_split(input_lines, "\\|", simplify =  TRUE)[, 1]
                        ,
                        output = str_split(input_lines, "\\|", simplify =  TRUE)[, 2]
                ) |>
                mutate(
                        signal = str_split(signal, "\\s", simplify = FALSE)
                        ,
                        output = str_split(output, "\\s", simplify = FALSE)
                ) |>
                mutate(
                        output = map(output, function(x)
                                x[x != ''])
                        ,
                        signal = map(signal, function(x)
                                x[x != ''])
                )
}


count_all_output_values <- function(
        .parsed_input
){
        .parsed_input |>
                mutate(output_count = map_int(output, count_output_values)) |>
                pull(output_count) |>
                sum()

}

check_in_list <- function(
        .input_value
        , .checklist
){
        .input_value %in% .checklist
}


count_output_values <- function(
        .parsed_output
        , .checklist = c(2,3,4,7)
){
        .parsed_output |>
                map_int(str_length) |>
                check_in_list(.checklist) |>
                as.integer() |>
                sum()

}


only <- function(.the_set_of, .makes_up){
        all(.the_set_of %in% .makes_up) &
                all(.makes_up %in% .the_set_of)
}

str_dismantle <- function(.input_string){
        .input_string |>
                str_split("") |>
                unlist()
}

mapping_digital_display_to_numbers <- function(.display_collection){
        if (only(
                .the_set_of = c("RV1", "RV2", "H1", "H3", "LV1", "LV2")
                , .makes_up = .display_collection
        )) {
                0L
        } else if (only(
                .the_set_of = c("RV1", "RV2")
                , .makes_up = .display_collection
        )) {
                1L
        } else if (only(
                .the_set_of = c("RV1","H1", "H2", "H3", "LV2")
                , .makes_up = .display_collection
        )) {
                2L
        } else if (only(
                .the_set_of = c("RV1", "RV2", "H1", "H2", "H3")
                , .makes_up = .display_collection
        )) {
                3L
        } else if (only(
                .the_set_of = c("RV1", "RV2", "H2", "LV1")
                , .makes_up = .display_collection
        )) {
                4L
        } else if (only(
                .the_set_of = c("H3", "RV2", "H1", "H2", "LV1")
                , .makes_up = .display_collection
        )) {
                5L
        } else if (only(
                .the_set_of = c("H2", "RV2", "H1", "H3", "LV1", "LV2")
                , .makes_up = .display_collection
        )) {
                6L
        } else if (only(
                .the_set_of = c("RV1", "RV2", "H1")
                , .makes_up = .display_collection
        )) {
                7L
        } else if (only(
                .the_set_of = c("RV1", "RV2", "H1", "H2", "H3", "LV1", "LV2")
                , .makes_up = .display_collection
        )) {
                8L
        } else if (only(
                .the_set_of = c("RV1", "RV2", "H1", "H2", "H3", "LV1")
                , .makes_up = .display_collection
        )) {
                9L
        } else {
                NA_integer_
        }

}

drop_element <- function(.input_vector, .item_to_drop){
        modded_vector <- .input_vector
        for (i in 1:length(.item_to_drop)) {
                modded_vector <- modded_vector[modded_vector != .item_to_drop[i]]
        }
        modded_vector
}

find_letter_mapping <- function(.parsed_signal){
        mapping_list <- list(
        a = c("RV1", "RV2", "H1", "H2", "H3", "LV1", "LV2")
        , b = c("RV1", "RV2", "H1", "H2", "H3", "LV1", "LV2")
        , c = c("RV1", "RV2", "H1", "H2", "H3", "LV1", "LV2")
        , d = c("RV1", "RV2", "H1", "H2", "H3", "LV1", "LV2")
        , e = c("RV1", "RV2", "H1", "H2", "H3", "LV1", "LV2")
        , f = c("RV1", "RV2", "H1", "H2", "H3", "LV1", "LV2")
        , g = c("RV1", "RV2", "H1", "H2", "H3", "LV1", "LV2")
        )

        ## First Phase reduction

        for (i in .parsed_signal) {
                if (str_length(i) == 2) {
                        possible_ones <- str_split(i, "") |>
                                unlist()
                        mapping_list[possible_ones] <- mapping_list[possible_ones] |>
                                map(drop_element,  c("H1", "H2", "H3", "LV1", "LV2"))
                }
                if (str_length(i) == 3) {
                        possible_ones <- str_split(i, "") |>
                                unlist()
                        mapping_list[possible_ones] <- mapping_list[possible_ones] |>
                                map(drop_element,  c("H2", "H3", "LV1", "LV2"))
                }
                if (str_length(i) == 4) {
                        possible_ones <- str_split(i, "") |>
                                unlist()
                        mapping_list[possible_ones] <- mapping_list[possible_ones] |>
                                map(drop_element,  c("H1", "H3", "LV2"))
                }
        }

        ## Second Phase reduction

        sorted_signal <- .parsed_signal |>
                str_length() |>
                `names<-`(.parsed_signal) |>
                sort() |>
                names()


        mapping_grid <- mapping_list |>
                expand.grid()

        for (i in sorted_signal) {
                valid_combos <- mapping_grid[str_dismantle(i)] |>
                        unique() |>
                        mutate(rownum = row_number()) |>
                        pivot_longer(-rownum) |>
                        mutate(across(value, as.character)) |>
                        group_by(rownum) |>
                        filter(n() == n_distinct(value)) |>
                        filter(!is.na(mapping_digital_display_to_numbers(value))) |>
                        ungroup() |>
                        pivot_wider() |>
                        select(-rownum) |>
                        as.list() |>
                        map(unique)
                mapping_list[names(valid_combos)] <- valid_combos


        }

        ## Third Phase reduction
        ## exploit the fact that there is only one instance of each number
        mapping_grid <- mapping_list |>
                expand.grid()

        number_possibilities <- list_along(1:length(sorted_signal))
        for (i in 1:length(number_possibilities)) {
                number_possibilities[[i]] <- as.integer(0:9)
        }

        names(number_possibilities) <- sorted_signal

        for (i in sorted_signal) {
                possible_numbers <- mapping_grid[str_dismantle(i)] |>
                        unique() |>
                        mutate(rownum = row_number()) |>
                        pivot_longer(-rownum) |>
                        mutate(across(value, as.character)) |>
                        group_by(rownum) |>
                        filter(!is.na(mapping_digital_display_to_numbers(value))) |>
                        summarise(display_num = mapping_digital_display_to_numbers(value)) |>
                        pull(display_num) |>
                        unique()
                number_possibilities[[i]] <- possible_numbers

        }

        for (i in 1:length(sorted_signal)) {
                taken_numbers <- number_possibilities[map_lgl(number_possibilities,function(x) length(x) == 1)]

                number_possibilities[map_lgl(number_possibilities,function(x) length(x) != 1)] <- number_possibilities[map_lgl(number_possibilities,function(x) length(x) != 1)] |>
                        map(drop_element,taken_numbers)

        }


        for (i in sorted_signal) {
                mapping_grid <- mapping_list |>
                        expand.grid()

                valid_combos <- mapping_grid[str_dismantle(i)] |>
                        unique() |>
                        mutate(rownum = row_number()) |>
                        pivot_longer(-rownum) |>
                        mutate(across(value, as.character)) |>
                        group_by(rownum) |>
                        filter(n() == n_distinct(value)) |>
                        filter(!is.na(mapping_digital_display_to_numbers(value))) |>
                        filter(mapping_digital_display_to_numbers(value) == number_possibilities[[i]]) |>
                        ungroup() |>
                        pivot_wider() |>
                        select(-rownum) |>
                        as.list() |>
                        map(unique)
                mapping_list[names(valid_combos)] <- valid_combos


        }

        mapping_list
}


apply_letter_mapping <- function(.garbled_input, .letter_mapping) {
        letter_list <- .garbled_input |>
                map(str_dismantle)
        output_list <- list()
        for (i in 1:length(letter_list)) {
                output_list[[i]] <- .letter_mapping[letter_list[[i]]]
        }

        output_list |>
                map_int(mapping_digital_display_to_numbers) |>
                as.character() |>
                paste0(collapse = "") |>
                as.integer()
}

find_and_apply_display_mapping <- function(.parsed_input){
        .parsed_input |>
                mutate(letter_mapping = map(signal, find_letter_mapping)) |>
                mutate(output_result = map2_int(output, letter_mapping, apply_letter_mapping))


}
