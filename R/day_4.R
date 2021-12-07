

pure_split_bingo_calls_and_boards <- function(.day_input){
	bingo_call <- .day_input[1]

	bingo_boards <- .day_input[2:length(.day_input)]

	list(bingo_call, bingo_boards)
}

lines_to_matrix <- function(x) {
		    tmp <- tempfile()
		    write_lines(paste0(x, collapse = '\n'), tmp)
		    read_fwf(tmp) %>%
			    as.matrix()
}

has_bingo <- function(.a_matrix){
	row_sum_prod <- .a_matrix  %>% rowSums() %>% prod()
	col_sum_prod <- .a_matrix  %>% colSums() %>% prod()
	(row_sum_prod * col_sum_prod) == 0
}

call_number <- function(.a_matrix, .calling_number){
	.a_matrix[.a_matrix == .calling_number] <- 0
	.a_matrix
}

pure_play_bingo <- function(.bingo_game_setup, .thats_playing_to_win_baby = TRUE){
	split_input <- .bingo_game_setup %>%
		pure_split_bingo_calls_and_boards()

	calls <- split_input[[1]]

	boards <- split_input[[2]] 

	splits <- boards %>%
		{. == ""} %>%
		which() 

	board_length <- splits %>%
		{. - lag(.) } %>%
		median(na.rm=T)


	parsed_boards <- splits %>%
		map(function(x) (x+1):(x+board_length-1)) %>%
		map(function(x) boards[x]) %>%
		map(lines_to_matrix)

	parsed_calls <- calls %>%
		strsplit(",") %>% 
		unlist() %>%
		as.integer()

	test_board <- parsed_boards[[1]]



	playing_boards <- parsed_boards

	for (i in parsed_calls){
		last_board_state <- playing_boards
		playing_boards <- playing_boards %>%
			map(call_number, i)	
	
		if(any(playing_boards %>% map_lgl(has_bingo)) & .thats_playing_to_win_baby){
			print("Bingo")
			winning_board <- playing_boards %>%
				{.[map_lgl(.,has_bingo)]} %>%
				{.[[1]]} 
			break
		} else if(all(playing_boards %>% map_lgl(has_bingo))) {
				   old_winning_boards <- last_board_state %>%
					   map_lgl(has_bingo)
				   new_winning_boards <- playing_boards %>%
					   map_lgl(has_bingo)
				   winning_board <- playing_boards %>%
					   {.[old_winning_boards != new_winning_boards]} %>%
					   {.[[1]]}
				   break	   
		}
	}


	winning_number <- i

	list(winning_board,winning_number)

}

pure_score_bingo <- function(.scoring_list){
	.scoring_list[[1]] %>%
		sum() %>%
		{. * .scoring_list[[2]]}

}
