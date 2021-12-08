

parse_vectors <- function(.input_row){
	.input_row %>%
		strsplit(" -> ") %>%
		{.[[1]]} %>%
		map(strsplit, ",") %>%
		map(function(x) x[[1]]) %>%
		map(as.integer)
}

generate_matching_function <- function(.input_limits){
	function(.input_vect){
		x_1 <- .input_limits[[1]][1]
		y_1 <- .input_limits[[1]][2]

		x_2 <- .input_limits[[2]][1]
		y_2 <- .input_limits[[2]][2]

		line_match  <-((  (x_2 - x_1)  ) *  (.input_vect[2]-y_1)) == (y_2 - y_1)  * (.input_vect[1] - x_1  )

		boundaries_match <- ((.input_vect[1] >= min(x_1, x_2)) & (.input_vect[1] <= max(x_1, x_2) )) &  ((.input_vect[2] >= min(y_1, y_2)) & (.input_vect[2] <= max(y_1, y_2) ))

		line_match & boundaries_match

	}

}

check_is_axis_parallel <- function(.input_vect){
		x_1 <- .input_vect[[1]][1]
		y_1 <- .input_vect[[1]][2]

		x_2 <- .input_vect[[2]][1]
		y_2 <- .input_vect[[2]][2]

		(x_1 == x_2) | (y_1 == y_2)


}

get_largest_coordinate <- function(.input_vect){
		x_1 <- .input_vect[[1]][1]
		y_1 <- .input_vect[[1]][2]

		x_2 <- .input_vect[[2]][1]
		y_2 <- .input_vect[[2]][2]

		max(c(x_1,y_1,x_2,y_2))

}

meta_functional_application <- function(.a_function, .value){
	.a_function(.value)
}

create_line_set <- function(.input_list, .diagonal_allowed = FALSE ) {
	.input_list %>%
		map(parse_vectors) %>%
		enframe(name = NULL) %>%
		mutate(
		       xy_line = map_lgl(value, check_is_axis_parallel)
		       , largest_coord = map_int(value, get_largest_coordinate)
		       , matching_functions = map(value,generate_matching_function)
		) %>%
		filter(xy_line | .diagonal_allowed)

}

create_vent_map <- function(.line_set){
	live_line_set <- .line_set %>%
		select(matching_functions) %>%
		pull()

	space_size <- .line_set %>%
		pull(largest_coord) %>%
		max() %>%
		{. + 1}

	base_matrix <- matrix(rep(0, space_size*space_size), space_size)

	for (x in 1:space_size) {
		for (y in 1:space_size ) {
			total_intersections  <- live_line_set %>%
				map_lgl(meta_functional_application, c(x - 1,y - 1)) %>%
				sum(na.rm = T)

			base_matrix[x,y] <- total_intersections

		}
	}

	t(base_matrix)
}


