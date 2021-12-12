#' @title Parse a Lava Tube Matrix
#' @description Using the input lines, determine the integer matrix of the
#'   height valuels.
#' @param .input_lines The input lines provided by AOC
#' @return a matrix with integer values representing the height of lava tubes
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname parse_lava_tube_matrix
#' @section day_9
parse_lava_tube_matrix <- function(.input_lines) {
        dismantled_string <- .input_lines |>
                map(str_dismantle)

        dismantled_string |>
                unlist() |>
                as.integer() |>
                matrix(ncol = length(dismantled_string)) |>
                t()

}

#' @title Retrieve the lowest point coords
#' @description Using the height matrix of the lava tube, find all the
#'   coordinates of local minima
#' @param .lava_tube_matrix a matrix with integer values representing the height
#'   of lava tubes
#' @return a matrix of indexing values for to identify the local minima
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_lowest_height_coords
#' @export
#' @section day_9
get_lowest_height_coords <- function(.lava_tube_matrix) {
        dims <- dim(.lava_tube_matrix)

        padded_matrix <- rep(NA_integer_, dims[2] + 1) |>
                rbind(
                        rep(NA_integer_, dims[1]) |> cbind(.lava_tube_matrix)
                ) |>
                cbind(rep(NA_integer_, dims[1] + 1)) |>
                rbind(rep(NA_integer_, dims[2] + 2))

        left_shifted_matrix <- padded_matrix[2:(dims[1] + 1), 3:(dims[2] + 2)]
        right_shifted_matrix <- padded_matrix[2:(dims[1] + 1), 1:(dims[2])]
        up_shifted_matrix <- padded_matrix[3:(dims[1] + 2), 2:(dims[2] + 1)]
        down_shifted_matrix <- padded_matrix[1:(dims[1]), 2:(dims[2] + 1)]

        left_shifted_matrix[is.na(left_shifted_matrix)] <- 10L
        right_shifted_matrix[is.na(right_shifted_matrix)] <- 10L
        up_shifted_matrix[is.na(up_shifted_matrix)] <- 10L
        down_shifted_matrix[is.na(down_shifted_matrix)] <- 10L



        (left_shifted_matrix > inital_matrix) & (right_shifted_matrix > inital_matrix) & (up_shifted_matrix > inital_matrix) & (down_shifted_matrix > inital_matrix)

}

#' @title find the total risk level at the local minima
#' @description using the matrix representation of the lava tube, determine the
#'   total risk level, of all the local minima.
#' @param .lava_tube_matrix a matrix with integer values representing the height
#'   of lava tubes
#' @return a single integer value representing the total risk
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname find_lowpoint_risk_level
#' @export
#' @section day_9
find_lowpoint_risk_level <- function(.lava_tube_matrix) {
        coords <- get_lowest_height_coords(.lava_tube_matrix)
~
        sum(.lava_tube_matrix[coords] + 1)

}
