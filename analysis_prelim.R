#source("Kishor import.R")
library(tidyverse)

d <- read_csv("data_clean.csv")

d <- d %>% 
	mutate(
		strike = case_when(
			pitch_result %in% c("Strike Swinging", "Strike Taken", "Foul", "BIP") ~ 1,
			TRUE ~ 0
		)) %>% 
		group_by(outing_id) %>% 
	mutate(
		pitch_num_by_outing = row_number()) %>% 
	ungroup() %>% 
	mutate(
		pitch_group_by_ten = (pitch_num_by_outing %/% 10) + 1,
		pg_b = pitch_group_by_ten * 10,
		pg_a = pg_b - 9,
		pitch_group = str_c(pg_a, pg_b, sep = "-")
		)

dd <- d %>% 
	group_by(pitch_group) %>% 
	summarize(
		strike_pct = mean(strike),
		count = n(),
	)

pitcher <- unique(d$pitcher)

ggplot(dd) +
	geom_point(aes(x = pitch_group, y = strike_pct)) +
	geom_line(aes(x = pitch_group, y = strike_pct, group = 1)) +
	scale_y_continuous(labels = scales::percent) +
	labs(title = paste(pitcher, "Strike % by Pitch Count"), y = "Strike Percentage", x = "Pitch Number") +
	theme_bw()


			
