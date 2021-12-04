pos_get_modifier <- function(.mod_string){
	mod_value <- str_match(.mod_string,"\\S*\\s(\\d*)")[,2]
	mod_value <- as.integer(mod_value)

	mod_direction <- case_when(
		str_detect(.mod_string, "forward") ~ c(1,0,0)
		, str_detect(.mod_string, "backward") ~ c(-1,0,0)
		, str_detect(.mod_string, "up") ~ c(0,0,-1)
		, str_detect(.mod_string, "down") ~ c(0,0,1)
	)

	mod_direction * mod_value


}


apply_transition_cartesian <- function(.input_list){
	.input_list %>%	
	map(pos_get_modifier) %>%
	reduce(`+`) %>%
	{.[.!=0]} %>%
	reduce(`*`)
}

apply_transition_submarine <- function(.input_list){
	movements_list <- .input_list %>%
	map(pos_get_modifier)
	       
base_position <- c(0,0,0)

for (i in movements_list){
	base_position[1] = base_position[1] + i[1]
	base_position[2] = base_position[2] + (i[1] * base_position[3])
	base_position[3] = base_position[3] + i[3]
}

base_position[1] * base_position[2] 

}

