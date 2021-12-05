pure_convert_diagnostic_code <- function(.input_value){
	.input_value %>%
		strsplit(split = '') %>%
		unlist() %>%
		as.integer()

}

pure_convert_binary_to_int  <- function(.bin_value){
	powers_of_two <- rev(2^(0:(length(.bin_value) - 1)))
	
	.bin_value %*% powers_of_two %>% as.integer()


}

pure_generate_gamma <- function(.input_list){
	.input_list %>%
	reduce(`+`) %>%
	{.>=(length(.input_list)/2)} %>%
	as.integer()
}

pure_generate_epsilon <- function(.input_list){
	.input_list %>%
		pure_generate_gamma() %>%
		{.==0} %>%
		as.integer()
}

pure_calculate_power_consumption <- function(.input_list){
	gamma_val <- .input_list %>%
		map(pure_convert_diagnostic_code) %>%
		pure_generate_gamma() %>%
		pure_convert_binary_to_int()

	epsilon_val <- .input_list %>%
		map(pure_convert_diagnostic_code) %>%
		pure_generate_epsilon() %>%
		pure_convert_binary_to_int()

	gamma_val * epsilon_val
}

pure_narrow_codes_list <- function(.parsed_list, .gamma_val, .index){
	list_flags <- .parsed_list %>%
		map(function(x) x[.index] == .gamma_val[.index]) %>%
		unlist()

	.parsed_list[list_flags]


}


pure_calculate_oxygen_generator_rating <- function(.input_list){
	
	holding_list  <- .input_list 

	for(i in 1:(length(.input_list[[1]])+1)){
		gamma_val  <- pure_generate_gamma(holding_list)
		holding_list <- pure_narrow_codes_list(holding_list, gamma_val, i)		
		if(length(holding_list) == 1){
			break
		}
	}
	holding_list[[1]]

}

pure_calculate_co2_scrubber_rating <- function(.input_list){
	
	holding_list  <- .input_list

	for(i in 1:(length(.input_list[[1]])+1)){
		gamma_val  <- pure_generate_epsilon(holding_list)
		holding_list <- pure_narrow_codes_list(holding_list, gamma_val, i)		
		if(length(holding_list) == 1){
			break
		}
	}
	holding_list[[1]]

}

pure_calculate_life_support_rating <- function(.input_list){
	parsed_input <- .input_list %>%
		map(pure_convert_diagnostic_code)

	co2_rating <- parsed_input %>%
		pure_calculate_co2_scrubber_rating() %>%
		pure_convert_binary_to_int()

	oxygen_rating <- parsed_input %>%
		pure_calculate_oxygen_generator_rating() %>%
		pure_convert_binary_to_int()

	co2_rating * oxygen_rating

}
