

pure_find_increases <- function(
				.input_lines
				){
	.input_lines %>%
	enframe(name = NULL) %>%
	mutate(across(value, as.integer)) %>%
	mutate(
	       lag_value = lag(value)
	       , increased = lag_value < value
	       ) %>%
	pull(increased) %>%
	sum(na.rm = T)
}


pure_find_increases_window <- function(
				       .input_lines
				       ){
	.input_lines %>%
	enframe(name = NULL) %>%
	mutate(across(value, as.integer)) %>%
	mutate(
		lead_value_1 = lead(value,1)
		, lead_value_2 = lead(value,2)
		, lead_value_3 = lead(value,3)
	) %>%
	mutate(
	        window_value = lead_value_1 + lead_value_2 + lead_value_3
	       ) %>%
	mutate(
	       lag_window = lag(window_value, 1)
	       , increased = lag_window < window_value
	       ) %>%
	filter(!is.na(lead_value_3)) %>%
	pull(increased) %>%
	sum(na.rm = T)
}
