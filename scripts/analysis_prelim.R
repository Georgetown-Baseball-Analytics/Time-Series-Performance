#source("Kishor import.R")
library(tidyverse)

d <- read_csv("../data/data_clean.csv")

#Strike % by pitch count

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


#Ks and BBs by pitch count

d <- d %>% 
  mutate(
    strikeout = case_when(
      non_bip_result %in% c("Strikeout,") ~ 1,
      TRUE ~ 0
    )) %>% 
  mutate(
    walk = case_when(
      non_bip_result %in% c("Walk,") ~ 1,
      TRUE ~ 0
    ))


dk <- d %>% 
  group_by(pitch_group) %>% 
  summarize(
    Outcome = sum(strikeout),
    Name = "Strikeout"
  )

dbb <- d %>% 
  group_by(pitch_group) %>% 
  summarize(
    Outcome = sum(walk),
    Name = "Walk"
  ) 

ddd <- rbind(dk, dbb)

ggplot(ddd, aes(x = pitch_group, y = Outcome, group = Name, color = Name)) +
  geom_point() +
  geom_line()
			
