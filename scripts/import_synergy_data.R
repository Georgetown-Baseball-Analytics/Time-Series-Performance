# DO NOT EDIT THIS SCRIPT DIRECTLY, MAKE A COPY
#setwd("C:/Users/knmea/Documents/GitHub/Time-Series-Performance/scripts")

# Ensure you have installed the packages below. Then, make sure the import function properly imports the data you are trying to analyze.


library(tidyverse)
library(rio)
library(janitor)

import_data <- function() {
  repeat{
    d <- readline(prompt = "Enter the raw file name: ")
    d <- read.csv(paste("../data/",d,sep = "",collapse = ""), check.names = FALSE)
    d1 <- seq.int(nrow(d))
    d <- cbind(d1,d)
    names(d) <- names(d)[-1]
    d <- d[1:ncol(d)-1]
    return(d)
    break
  }
}  

clean_inning <- function(d) {
	d <- d %>% 
		mutate(
			top_or_bottom_letter = strtrim(inning, 1),
			top_of_frame = if_else(top_or_bottom_letter == "T", 1, 0),
			inning_num = as.numeric(str_remove_all(inning, top_or_bottom_letter))
		)
}

clean_count <- function(d) {
	d <- d %>% 
		separate("count", into = c("balls", "strikes"), sep = "-", convert = TRUE)
	return(d)
}

clean_score <- function(d) {
  d <- d %>% 
    separate(score, c("Away Score", "Home Score"), sep = "-")
  
  d <- d %>% 
    mutate(
      temp = notes
    )
  
  d <- d %>% 
    separate(temp, c("Away Team", "Home Team"), sep = " vs ")
  
  # pitcher_team <- ""
  # 
  # if(d$top_of_frame[1] == 1) {
  #   pitcher_team = d$Home.Team[1]
  # } else {
  #   pitcher_team = d$Away.Team[1]
  # }
  
  d <- d %>% 
    mutate(pitcherteam_score = 0)
  
  for (i in seq_along(d$notes)) {
    if(d$top_of_frame[i] == 0) {
      d$pitcherteam_score[i] <- as.numeric(d$`Away Score`[i])
    }
    else {
      d$pitcherteam_score[i] <- as.numeric(d$`Home Score`[i])
    }
  }
  return(d)
}

add_outing_id <- function(d) {
	d <- d %>% 
		mutate(outing_id = 1)
	
	cur_outing <- d$notes[1]
	cur_outing_num <- 1
	
	cur_inningnum <- d$inning_num[1]
	cur_pitcherteamscore <- d$pitcherteam_score[1]
	cur_outs <- d$outs[1]
	  
	for (i in seq_along(d$notes)) {
	  if(d$inning_num[i] == cur_inningnum) {
	    if (d$notes[i] == cur_outing && d$pitcherteam_score[i] == cur_pitcherteamscore && d$outs[i] >= cur_outs) {
	      d$outing_id[i] <- cur_outing_num
	      cur_inningnum <- d$inning_num[i]
	      cur_outs <- d$outs[i]
	    }
	    else {
	      cur_outing_num <- cur_outing_num + 1
	      d$outing_id[i] <- cur_outing_num
	      cur_outing <- d$notes[i]
	      cur_inningnum <- d$inning_num[i]
	      cur_pitcherteamscore <- d$pitcherteam_score[i]
	      cur_outs <- d$outs[i]
	    }
	  }
	  else {
	    if (d$notes[i] == cur_outing && d$inning_num[i] >= cur_inningnum && d$inning_num[i] <= cur_inningnum + 1 && d$pitcherteam_score[i] >= cur_pitcherteamscore) {
	      d$outing_id[i] <- cur_outing_num
	      cur_inningnum <- d$inning_num[i]
	      cur_pitcherteamscore <- d$pitcherteam_score[i]
	      cur_outs <- d$outs[i]
	    }
	    else {
	      cur_outing_num <- cur_outing_num + 1
	      d$outing_id[i] <- cur_outing_num
	      cur_outing <- d$notes[i]
	      cur_inningnum <- d$inning_num[i]
	      cur_pitcherteamscore <- d$pitcherteam_score[i]
	      cur_outs <- d$outs[i]
	    }
	  }
	}
	return(d)
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
	
	d <- d %>% 
	  mutate(batting_order = case_when(
	    pa_id %% 9 == 0 ~ 9,
	    TRUE ~ pa_id %% 9
	    )
	  )
	
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
	  clean_score %>% 
		add_outing_id() %>% 
		add_pa_id() %>% 
	    clean_result()
	
	return(d)
}

choose_pitcher <- function(d) {
  chosen_pitcher <- readline(prompt = "Enter pitcher name (Last Name, First Initial.):")
  d <- d %>% 
    filter(Pitcher == paste("",chosen_pitcher,sep = "",collapse = ""))
  return(d)
}

export_data <- function(d) {
  e <- readline(prompt = "Enter the clean file name: ")
  print(paste("Exported file as",e))
	rio::export(d, paste("../data/",e,sep = "",collapse = ""))
}

d <- import_data()
d <- choose_pitcher(d)
d <- clean_data_pipeline(d)
d <- export_data(d)
