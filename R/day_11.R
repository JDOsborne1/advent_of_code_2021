#' Parse Octopus positions
#'
#' @param .input_lines
#'
#' @return
#' @export
#'
#' @examples
parse_octopus_positions <- function(.input_lines){
        row_width <- str_length(.input_lines[[1]])
        .input_lines |>
                map(str_dismantle) |>
                unlist() |>
                as.integer() |>
                matrix(nrow = row_width) |>
                t()
}


#' Find total Flashes
#'
#' @param .parsed_data
#' @param .after
#'
#' @return
#' @export
#'
#' @examples
find_total_flashes <- function(.parsed_data, .after){
        .parsed_data
}


iterate_octopus_lifecycle <- function(.octopus_matrix){
        iterator <- (.octopus_matrix == .octopus_matrix) |>
                as.integer() |>
                `dim<-`(dim(.octopus_matrix))

        new_octopus <- .octopus_matrix + iterator


        flash_indexes <- new_octopus %>%
                {. > 9} |>
                which(arr.ind = T)

        full_flash_indexes <- flash_indexes

        while (length(flash_indexes) != 0) {
                flash_indexes <- new_octopus %>%
                        {
                                . > 9
                        } |>
                        which(arr.ind = T)


                flash_affected <- flash_indexes |>
                        flash_range(1) |>
                        bound_flash_range(.octopus_matrix)

                index_counts <- flash_affected |>
                        enframe() |>
                        group_by(value) |>
                        count()

                for (i in 1:nrow(index_counts)) {
                        new_octopus[index_counts[i,]$value] <- new_octopus[index_counts[i,]$value] + index_counts[i,]$n
                }



                new_octopus[flash_indexes] <- 0
        }

        new_octopus
}

flash_range <- function(.matrix_of_affected_indicies, .range){
        expansions <- expand.grid(
                (-1 * .range):.range
                , (-1 * .range):.range
        )

        list_of_indicies <- .matrix_of_affected_indicies |>
                map(`+`, expansions)

        do.call(rbind, list_of_indicies) |>
                as.matrix()
}


bound_flash_range <- function(.flash_range, .source_matrix){
        accepted_indicies <- .flash_range |>
                apply(1,max ) |>
                map_lgl(function(x) x <= max(dim(.source_matrix)))
        .flash_range[accepted_indicies,]
}
