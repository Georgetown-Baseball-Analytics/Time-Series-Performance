# DO NOT EDIT THIS SCRIPT DIRECTLY, MAKE A COPY

# Ensure you have installed the packages below. Then, make sure the import function properly imports the data you are trying to analyze.


library(tidyverse)
library(rio)
library(janitor)


import_data <- function() {
	d <- read_csv("../data/data_tonas.csv")
	d <- clean_names(d)
	return(d)
}

clean_inning <- function(d) {
	if (any(str_length(d$inning) < 2) | any(str_length(d$inning) > 3)) {
		print("Check inning variable - string length less than 2 or greater than 3")
	}
	d <- d %>% 
		mutate(
			top_or_bottom_letter = str_remove_all(inning, "[0-9]"),
			top_of_frame = if_else(top_or_bottom_letter == "T", 1, 0),
			inning_num = str_remove_all(inning, "[^0-9]")
		)
}

clean_count <- function(d) {
	d <- d %>% 
		separate("count", into = c("balls", "strikes"), sep = "-", convert = TRUE)
	return(d)
}

add_outing_id <- function(d) {
	d <- d %>% 
		mutate(outing_id = 1)
	
	cur_outing <- d$notes[1]
	cur_outing_num <- 1
	
	for (i in seq_along(d$notes)) {
		if (d$notes[i] == cur_outing) {
			d$outing_id[i] <- cur_outing_num
		}
		
		else {
			cur_outing_num <- cur_outing_num + 1
			d$outing_id[i] <- cur_outing_num
			cur_outing <- d$notes[i]
		}
	}
	return(d)
	
	# does not work if pitcher pitches in multiple consecutive games against the same opponent
}

add_pa_id <- function(d) {
	add_pa_id_by_outing <- function(df) {
		df <- df %>% 
			mutate(pa_id = 1)
		
		cur_pa <- df$title[1]
		cur_pa_num <- 1
		
		for (i in seq_along(df$title)) {
			if (df$title[i] == cur_pa) {
				df$pa_id[i] <- cur_pa_num
			}
			
			else {
				cur_pa_num <- cur_pa_num + 1
				df$pa_id[i] <- cur_pa_num
				cur_pa <- df$title[i]
			}
		}
		return(df)
	}
		
	d <- d %>% 
		group_by(outing_id) %>% 
		nest()
	
	d$data <- map(d$data, add_pa_id_by_outing)
	d <- d %>% 
		unnest(cols = data)
	
	return(d)
}

clean_result <- function(d) {
    d <- d %>% 
        mutate(
            non_bip_result_clean = str_remove_all(non_bip_result, ","),
            non_bip_result = if_else(non_bip_result_clean == "", NA_character_, non_bip_result_clean)
        ) %>% 
        select(-non_bip_result_clean)
    
    return(d)
                
}

clean_data_pipeline <- function(d) {
	
	d <- d %>%
		clean_names() %>% 
		clean_inning() %>% 
		clean_count() %>% 
		add_outing_id() %>% 
		add_pa_id() %>% 
	    clean_result()
	
	return(d)
}

export_data <- function(d) {
	rio::export(d, "../data/tonas_clean.csv")
}

d <- import_data() %>% clean_data_pipeline() %>% export_data()

