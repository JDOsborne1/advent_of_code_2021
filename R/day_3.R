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
	map(pure_convert_diagnostic_code) %>%
	reduce(`+`) %>%
	{.>=(length(input_3)/2)} %>%
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
		pure_generate_gamma() %>%
		pure_convert_binary_to_int()

	epsilon_val <- .input_list %>%
		pure_generate_epsilon() %>%
		pure_convert_binary_to_int()

	gamma_val * epsilon_val
}
