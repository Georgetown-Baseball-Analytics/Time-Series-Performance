#setwd("C:/Users/rajmo/Documents/Baseball/Time-Series-Performance/scripts")
source("import_synergy_data.R")

d <- read.csv(d)
d <- replace(d,d == "", NA)
#Modify data set to include pitch counts

d <- d %>% 
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

#Strike % across pitch counts

d_strikepct <- d %>% 
  mutate(
    strike = case_when(
      pitch_result %in% c("Strike Swinging", "Strike Taken", "Foul", "BIP") ~ 1,
      TRUE ~ 0
    )) 	 %>% 
	group_by(pitch_group) %>% 
	summarize(
		strike_pct = mean(strike),
		count = n(),
	)

pitcher <- unique(d$pitcher)

graph1 <- ggplot(d_strikepct) +
	geom_point(aes(x = pitch_group, y = strike_pct)) +
	geom_line(aes(x = pitch_group, y = strike_pct, group = 1)) +
	scale_y_continuous(labels = scales::percent) +
	labs(title = paste(pitcher, "Strike % by Pitch Count"), y = "Strike Percentage", x = "Pitch Number") +
	theme_bw()

ggplot(d_strikepct) +
  geom_point(aes(x = pitch_group, y = strike_pct)) +
  geom_line(aes(x = pitch_group, y = strike_pct, group = 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste(pitcher, "Strike % by Pitch Count"), y = "Strike Percentage", x = "Pitch Number") +
  theme_bw()

#strike % across innings

d_strikepct_innings <- d %>% 
  mutate(
    strike = case_when(
      pitch_result %in% c("Strike Swinging", "Strike Taken", "Foul", "BIP") ~ 1,
      TRUE ~ 0
    )) 	 %>% 
  group_by(inning_num) %>% 
  summarize(
    strike_pct = mean(strike),
    count = n(),
  )

pitcher <- unique(d$pitcher)

graph2 <- ggplot(d_strikepct_innings) +
  geom_point(aes(x = inning_num, y = strike_pct)) +
  geom_line(aes(x = inning_num, y = strike_pct, group = 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste(pitcher, "Strike % by Inning"), y = "Strike Percentage", x = "Inning Number") +
  theme_bw()

ggplot(d_strikepct_innings) +
  geom_point(aes(x = inning_num, y = strike_pct)) +
  geom_line(aes(x = inning_num, y = strike_pct, group = 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste(pitcher, "Strike % by Inning"), y = "Strike Percentage", x = "Inning Number") +
  theme_bw()

#Ks and BBs by pitch count

d <- d %>% 
  mutate(
    strikeout = case_when(
      non_bip_result %in% c("Strikeout") ~ 1,
      TRUE ~ 0
    )) %>% 
  mutate(
    walk = case_when(
      non_bip_result %in% c("Walk") ~ 1,
      TRUE ~ 0
    ))


dk <- d %>% 
  group_by(pitch_group) %>% 
  summarize(
    Outcome = sum(strikeout),
    Name = "strikeout"
  )

dbb <- d %>% 
  group_by(pitch_group) %>% 
  summarize(
    Outcome = sum(walk),
    Name = "walk"
  ) 

d_ksandbbs <- rbind(dk, dbb)

graph3 <- ggplot(d_ksandbbs, aes(x = pitch_group, y = Outcome, group = Name, color = Name)) +
  geom_point() +
  geom_line()

ggplot(d_ksandbbs, aes(x = pitch_group, y = Outcome, group = Name, color = Name)) +
  geom_point() +
  geom_line()
			
#does he have more strikeouts at the beginning because he threw more pitches


# Batting Average Aggregation ---------------------------------------------

d_ba <- d %>% 
    group_by(outing_id, pa_id) %>% 
    summarize(no_result = sum(!is.na(bip_result)) + sum(!is.na(non_bip_result)))

assertthat::assert_that(all(d_ba$no_result == 1))
# fix later


d_ba <- d %>% 
    filter(!is.na(bip_result) | !is.na(non_bip_result)) %>% 
    group_by(pitch_group, outing_id, pa_id) %>% 
    summarize(
        pa_outcome = if_else(!is.na(bip_result), bip_result, non_bip_result)
    ) %>% 
    mutate( # this is what changes for slugging and OBP
        ba_bin = case_when(
            pa_outcome %in% c(
                "Single",
                "Double",
                "Ground Rule Double",
                "Triple",
                "Home Run"
            ) ~ 1,
            pa_outcome %in% c(
                "Flyout",
                "Strikeout",
                "Groundout",
                "Lineout",
                "Lined Into Double Play",
                "Bunt Groundout",
                "Popout",
                "Field Error",
                "Grounded Into Double Play",
                "Fielder's Choice - Out",
                "Fielder's Choice - Safe"
            ) ~ 0,
            TRUE ~ NA_real_
            # do this for all
        )
    )

d_ba_collapsed <- d_ba %>% 
    group_by(pitch_group) %>% 
    summarize(
        Outcome = mean(ba_bin, na.rm = T),
        Name = ".AVG"
    )


# Slugging Percentage Aggregation --------------------------------------------------------------------

d_slg <- d %>% 
  group_by(outing_id, pa_id) %>% 
  summarize(no_result = sum(!is.na(bip_result)) + sum(!is.na(non_bip_result)))

assertthat::assert_that(all(d_slg$no_result == 1))
# fix later


d_slg <- d %>% 
  filter(!is.na(bip_result) | !is.na(non_bip_result)) %>% 
  group_by(pitch_group, outing_id, pa_id) %>% 
  summarize(
    pa_outcome = if_else(!is.na(bip_result), bip_result, non_bip_result)
  ) %>% 
  mutate( 
    single_bin = case_when(
      pa_outcome %in% c("Single") ~ 1,
      TRUE ~ 0),
    double_bin = case_when(
      pa_outcome %in% c("Double", "Ground Rule Double") ~ 1,
      TRUE ~ 0),
    triple_bin = case_when(
      pa_outcome %in% c("Triple") ~ 1,
      TRUE ~ 0),
    hr_bin = case_when(
      pa_outcome %in% c("Home Run") ~ 1,
      TRUE ~ 0),
    at_bat = case_when(
      pa_outcome %in% c("Walk", "Sacrifice Bunt", "Sacrifice Fly", "Hit By Pitch") ~ 0,
      TRUE ~ 1)
  )

d_slg_collapsed <- d_slg %>% 
  group_by(pitch_group) %>% 
  summarize(
    Outcome = (sum(single_bin) + 2*sum(double_bin) + 3*sum(triple_bin) + 4*sum(hr_bin))/sum(at_bat),
    Name = ".SLG"
  )


# On Base Percentage Aggregation --------------------------------------------------------------------

d_obp <- d %>% 
  group_by(outing_id, pa_id) %>% 
  summarize(no_result = sum(!is.na(bip_result)) + sum(!is.na(non_bip_result)))

assertthat::assert_that(all(d_obp$no_result == 1))
# fix later


d_obp <- d %>% 
  filter(!is.na(bip_result) | !is.na(non_bip_result)) %>% 
  group_by(pitch_group, outing_id, pa_id) %>% 
  summarize(
    pa_outcome = if_else(!is.na(bip_result), bip_result, non_bip_result)
  ) %>% 
  mutate( # this is what changes for slugging and OBP
    obp_bin = case_when(
      pa_outcome %in% c(
        "Single",
        "Double",
        "Ground Rule Double",
        "Triple",
        "Hit By Pitch",
        "Home Run",
        "Walk"
      ) ~ 1,
      pa_outcome %in% c(
        "Sacrifice Bunt"
      ) ~ NA_real_,
      TRUE ~ 0
      # do this for all
    )
  )

d_obp_collapsed <- d_obp %>% 
  group_by(pitch_group) %>% 
  summarize(
    Outcome = mean(obp_bin, na.rm = T),
    Name = ".OBP"
  )

#Graph for performance by pitch count -------------------------------------------------------------

dpitchperf <- rbind(d_ba_collapsed, d_slg_collapsed, d_obp_collapsed)

graph4 <- ggplot(dpitchperf, aes(x = pitch_group, y = Outcome, group = Name, color = Name)) +
  geom_point() +
  geom_line()

ggplot(dpitchperf, aes(x = pitch_group, y = Outcome, group = Name, color = Name)) +
  geom_point() +
  geom_line()

#R markdown attempt

plot_list <- list(graph1, graph2, graph3, graph4)
file <- tempfile()
saveRDS(plot_list, file)
rmarkdown::render('rmarkdownfile.Rmd', params = list(file = file))
                  