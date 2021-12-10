
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

# generate_reader_function <- function(
#         .parsed_signal
# ){
#
#                 .parsed_signal[str_length(.parsed_signal) == 2] |>
#                         str_split("") |>
#                         unlist()
#
# }
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




