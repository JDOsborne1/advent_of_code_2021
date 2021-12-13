minimise_navigation_syntax <- function(.input_line) {
        base_length <- stringr::str_length(.input_line)
        new_length <- base_length - 1
        new_string <- .input_line
        while (new_length < base_length) {
                base_length <- stringr::str_length(new_string)
                new_string <- new_string |>
                        stringr::str_remove_all("\\[\\]|\\(\\)|\\<\\>|\\{\\}")
                new_length <- stringr::str_length(new_string)

        }
        new_string
}


find_first_closing <- function(.parsed_line){
        .parsed_line |>
                stringr::str_match("\\>|\\}|\\]|\\)")
}

drop_missing <- function(.parsed_vector){
        .parsed_vector[!is.na(.parsed_vector)]
}

score_illegal_closes <- function(.parsed_vector){
        score_list <- list(
                ">" = 25137L
                , "}" = 1197L
                , "]" = 57L
                , ")" = 3L
        )

        score_list[.parsed_vector]
}

#' Clear Legal Values
#'
#' @description Function to reduce down any legal values found in the input
#'   dataset
#'
#' @param .input_lines
#'
#' @return
#' @export
#'
#' @examples
clear_legal_values <- function(.input_lines){
        .input_lines |>
                purrr::map_chr(minimise_navigation_syntax)
}


#' Find and score the illegal closes
#'
#' @description Function to find and score all the illegal closes in a report
#'
#' @param .parsed_lines
#'
#' @return
#' @export
#'
#' @examples
find_and_score_illegal_closes <- function(.parsed_lines){
        .parsed_lines |>
                purrr::map_chr(find_first_closing) |>
                drop_missing() |>
                score_illegal_closes() |>
                unlist() |>
                sum()

}


#' Find the incomplete lines and score them
#'
#' @description This finds all the incomplete lines and scores them.
#'
#'   First: To repair the navigation subsystem, you just need to figure out the
#'   sequence of closing characters that complete all open chunks in the line.
#'
#'   You can only use closing characters (), ], }, or >), and you must add them
#'   in the correct order so that only legal pairs are formed and all chunks end
#'   up closed.
#'
#'   Then: Start with a total score of 0. Then, for each character, multiply the
#'   total score by 5 and then increase the total score by the point value given
#'   for the character in the following table:
#'
#'   ): 1 point. ]: 2 points. }: 3 points. >: 4 points.
#'
#'   Finally: the winner is found by sorting all of the scores and then taking
#'   the middle score. (There will always be an odd number of scores to
#'   consider.)
#'
#' @param .parsed_lines
#'
#' @return
#' @export
#'
#' @examples
find_and_score_incomplete_lines <- function(.parsed_lines){
        index <- .parsed_lines |>
                purrr::map_chr(find_first_closing) |>
                is.na()
        .parsed_lines[index] |>
                replace_with_closing() |>
                purrr::map_chr(str_reverse) |>
                purrr::map_dbl(score_completion) |>
                median()

}

str_reverse <- function(.a_string){
        .a_string |>
                stringr::str_split("") |>
                unlist() |>
                rev() |>
                paste0(collapse = "")

}

replace_with_closing <- function(.input_string){
        .input_string |>
                stringr::str_replace_all("\\(", "\\)") |>
                stringr::str_replace_all("\\<", "\\>") |>
                stringr::str_replace_all("\\{", "\\}") |>
                stringr::str_replace_all("\\[", "\\]")
}

score_completion <- function(.completion_string){
        split_string <- .completion_string |>
                stringr::str_split("") |>
                unlist()

        mapping_list <- list(
                ")" = 1
                , "]" = 2
                , "}" = 3
                , ">" = 4
                ) |>
                unlist()

        mapping_with_initial_value <- c(0, mapping_list[split_string])

        mapping_with_initial_value |>
                purrr::reduce(.f = function(x,y) (x*5) + y)

        }
