#setwd("C:/Users/rajmo/Documents/Baseball/Time-Series-Performance/scripts")
#source("import_synergy_data.R")

#d <- read.csv(d)

#Modify data set to include pitch counts

group_pitch_counts <- function(d) {
  
d <- replace(d,d == "", NA)

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

d <- d %>% 
		group_by(outing_id) %>% 
	mutate(
		pitch_num_by_outing = row_number()) %>% 
	ungroup() %>% 
	mutate(
		pitch_group_by_ten = if_else(is.wholenumber(pitch_num_by_outing/10),(pitch_num_by_outing %/% 10),(pitch_num_by_outing %/% 10) + 1),
		pg_b = pitch_group_by_ten * 10,
		pg_a = pg_b - 9,
		pitch_group = str_c(pg_a, pg_b, sep = "-")
		)

outing_pitch_counts <- d %>% 
  group_by(outing_id) %>% 
  summarise(num_pitches = n())

outing_pitch_counts <- outing_pitch_counts %>% 
  mutate(outing_id_graph = rev(outing_id))

avg_pitches_per_outing <- mean(outing_pitch_counts$num_pitches)
rounded_avg_pitches_per_outing <- round(mean(outing_pitch_counts$num_pitches), digits = 0)
std_pitches_per_outing <- sd(outing_pitch_counts$num_pitches)

return(d)

}

#Number of pitches in each outing

pitches_per_outing <- function(d) {
  pitcher <- unique(d$pitcher)
  
  outing_pitch_counts <- d %>% 
    group_by(outing_id) %>% 
    summarise(num_pitches = n())
  
  outing_pitch_counts <- outing_pitch_counts %>% 
    mutate(outing_id_graph = rev(outing_id))
  
  graph1 <- ggplot(outing_pitch_counts, aes(x = outing_id_graph, y = num_pitches)) +
    geom_point() +
    geom_line() +
    labs(
      title = paste(pitcher, "Number of Pitches in Each Outing"), 
      y = "Number of Pitches Thrown", 
      x = "Outing Number",
    ) +
    theme_bw()
  
  return(graph1)
}

#Strike % across pitch counts

strikepct_pitchcount <- function(d) {

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

outing_pitch_counts <- d %>% 
  group_by(outing_id) %>% 
  summarise(num_pitches = n())

avg_pitches_per_outing <- mean(outing_pitch_counts$num_pitches)
rounded_avg_pitches_per_outing <- round(mean(outing_pitch_counts$num_pitches), digits = 0)
std_pitches_per_outing <- sd(outing_pitch_counts$num_pitches)
upper_bound <- (avg_pitches_per_outing/10+1) + (std_pitches_per_outing/10)
lower_bound <- (avg_pitches_per_outing/10+1) - (std_pitches_per_outing/10)
num_outings <- nrow(outing_pitch_counts)

graph1 <- ggplot(d_strikepct) +
  geom_point(aes(x = pitch_group, y = strike_pct)) +
  geom_line(aes(x = pitch_group, y = strike_pct, group = 1)) +
  geom_vline(aes(x = pitch_group, y = strike_pct), xintercept = avg_pitches_per_outing/10+1, linetype = "dotted", lwd = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(limits = unique(d$pitch_group)) +
  labs(
    title = paste(pitcher, "Strike % by Pitch Count"), 
    y = "Strike Percentage", 
    x = "Pitch Number",
    caption = paste("In this data set,",pitcher, "averaged", rounded_avg_pitches_per_outing, "pitches per outing over", num_outings, "outings.")
    ) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 9)
  )

return(graph1)

}

#strike % across innings

strikepct_innings <- function(d) {

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
  scale_x_continuous(breaks = 1:max(d$inning_num)) +
  labs(title = paste(pitcher, "Strike % by Inning"), y = "Strike Percentage", x = "Inning Number") +
  theme_bw()

return(graph2)
}

#K and BB rate by pitch count

kandbbrate_pitchcount <- function(d) {

d_kandbbrate <- d %>% 
  group_by(outing_id, pa_id) %>% 
  summarize(no_result = sum(!is.na(bip_result)) + sum(!is.na(non_bip_result)))

#assertthat::assert_that(all(d_kandbbrate$no_result == 1))

d_kandbbrate <- d %>% 
  filter(!is.na(bip_result) | !is.na(non_bip_result)) %>% 
  group_by(pitch_group, outing_id, pa_id) %>% 
  summarize(
    pa_outcome = if_else(!is.na(bip_result), bip_result, non_bip_result)
  ) %>% 
  mutate( 
    k_bin = case_when(
      pa_outcome %in% c(
        "Strikeout"
      ) ~ 1,
      TRUE ~ 0
      # do this for all
    ),
    bb_bin = case_when(
      pa_outcome %in% c(
        "Walk"
      ) ~ 1,
      TRUE ~ 0
      # do this for all
    )
  )

d_krate_collapsed <- d_kandbbrate %>% 
  group_by(pitch_group) %>% 
  summarize(
    Outcome = mean(k_bin, na.rm = T),
    Name = "K rate"
  )

d_bbrate_collapsed <- d_kandbbrate %>% 
  group_by(pitch_group) %>% 
  summarize(
    Outcome = mean(bb_bin, na.rm = T),
    Name = "BB rate"
  )

d_kandbbrate_graph <- rbind(d_krate_collapsed,d_bbrate_collapsed)

pitcher <- unique(d$pitcher)

outing_pitch_counts <- d %>% 
  group_by(outing_id) %>% 
  summarise(num_pitches = n())

avg_pitches_per_outing <- mean(outing_pitch_counts$num_pitches)
rounded_avg_pitches_per_outing <- round(mean(outing_pitch_counts$num_pitches), digits = 0)
std_pitches_per_outing <- sd(outing_pitch_counts$num_pitches)
upper_bound <- (avg_pitches_per_outing/10+1) + (std_pitches_per_outing/10)
lower_bound <- (avg_pitches_per_outing/10+1) - (std_pitches_per_outing/10)
num_outings <- nrow(outing_pitch_counts)

graph3 <- ggplot(d_kandbbrate_graph, aes(x = pitch_group, y = Outcome, group = Name, color = Name)) +
  geom_point() +
  geom_line() +
  geom_vline(aes(x = pitch_group, y = strike_pct), xintercept = avg_pitches_per_outing/10+1, linetype = "dotted", lwd = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(limits = unique(d$pitch_group)) +
  labs(
    title = paste(pitcher, "Strikeout and Walk Rate by Pitch Count"), 
    x = "Pitch Number",
    caption = paste("In this data set,",pitcher, "averaged", rounded_avg_pitches_per_outing, "pitches per outing over", num_outings, "outings.")
    ) +
  scale_color_manual(values = c("red", "dark green")) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 9)
  )

return(graph3)

}

#Ks and BBs by pitch count

kandbbcount_pitchcount <- function(d) {

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

graph4 <- ggplot(d_ksandbbs, aes(x = pitch_group, y = Outcome, group = Name, color = Name)) +
  geom_point() +
  geom_line()

return(graph4)

}

# Batting Average Aggregation 

avg_aggregation <- function(d) {

d_ba <- d %>% 
    group_by(outing_id, pa_id) %>% 
    summarize(no_result = sum(!is.na(bip_result)) + sum(!is.na(non_bip_result)))

#assertthat::assert_that(all(d_ba$no_result == 1))


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
                "Fielder's Choice - Safe",
                "Infield Fly Rule"
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
return(d_ba_collapsed)

}

# On Base Percentage Aggregation

obp_aggregation <- function(d) {
 
  d_obp <- d %>% 
    group_by(outing_id, pa_id) %>% 
    summarize(no_result = sum(!is.na(bip_result)) + sum(!is.na(non_bip_result)))
  
  #assertthat::assert_that(all(d_obp$no_result == 1))
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
   
  return(d_obp_collapsed)
}

# Slugging Percentage Aggregation 

slg_aggregation <- function(d) {

d_slg <- d %>% 
  group_by(outing_id, pa_id) %>% 
  summarize(no_result = sum(!is.na(bip_result)) + sum(!is.na(non_bip_result)))

#assertthat::assert_that(all(d_slg$no_result == 1))
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
return(d_slg_collapsed)

}

# OPS Aggregation 

ops_aggregation <- function(d) {

d_obp <- d %>% 
  group_by(outing_id, pa_id) %>% 
  summarize(no_result = sum(!is.na(bip_result)) + sum(!is.na(non_bip_result)))

#assertthat::assert_that(all(d_obp$no_result == 1))
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
    obp = mean(obp_bin, na.rm = T)
  )

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
    slg = (sum(single_bin) + 2*sum(double_bin) + 3*sum(triple_bin) + 4*sum(hr_bin))/sum(at_bat)
  )

d_ops_collapsed <- merge(d_slg_collapsed,d_obp_collapsed)

d_ops_collapsed <- d_ops_collapsed %>% 
  mutate(
    Outcome = obp + slg,
    Name = ".OPS"
  ) %>% 
  select(pitch_group, Outcome, Name)

return(d_ops_collapsed)
}

#AVG and OBP by pitch count

avg_and_obp_pitchcount <- function(d) {
  
dpitchperf <- rbind(avg_aggregation(d), obp_aggregation(d))

pitcher <- unique(d$pitcher)

outing_pitch_counts <- d %>% 
  group_by(outing_id) %>% 
  summarise(num_pitches = n())

avg_pitches_per_outing <- mean(outing_pitch_counts$num_pitches)
rounded_avg_pitches_per_outing <- round(mean(outing_pitch_counts$num_pitches), digits = 0)
std_pitches_per_outing <- sd(outing_pitch_counts$num_pitches)
upper_bound <- (avg_pitches_per_outing/10+1) + (std_pitches_per_outing/10)
lower_bound <- (avg_pitches_per_outing/10+1) - (std_pitches_per_outing/10)
num_outings <- nrow(outing_pitch_counts)

graph5 <- ggplot(dpitchperf, aes(x = pitch_group, y = Outcome, group = Name, color = Name)) +
  geom_point() +
  geom_line() +
  geom_vline(aes(x = pitch_group, y = strike_pct), xintercept = avg_pitches_per_outing/10+1, linetype = "dotted", lwd = 1) +
  scale_x_discrete(limits = unique(d$pitch_group)) +
  labs(
    title = paste(pitcher, ".AVG and .OBP by Pitch Count"), 
    x = "Pitch Number",
    caption = paste("In this data set,",pitcher, "averaged", rounded_avg_pitches_per_outing, "pitches per outing over", num_outings, "outings.")
  ) +
  scale_color_manual(values = c("purple", "orange")) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 9)
  )

return(graph5)
}

#SLG and OPS by pitch count

slg_and_ops_pitchcount <- function(d) {
  
  dpitchperf <- rbind(slg_aggregation(d), ops_aggregation(d))
  
  pitcher <- unique(d$pitcher)
  
  outing_pitch_counts <- d %>% 
    group_by(outing_id) %>% 
    summarise(num_pitches = n())
  
  avg_pitches_per_outing <- mean(outing_pitch_counts$num_pitches)
  rounded_avg_pitches_per_outing <- round(mean(outing_pitch_counts$num_pitches), digits = 0)
  std_pitches_per_outing <- sd(outing_pitch_counts$num_pitches)
  upper_bound <- (avg_pitches_per_outing/10+1) + (std_pitches_per_outing/10)
  lower_bound <- (avg_pitches_per_outing/10+1) - (std_pitches_per_outing/10)
  num_outings <- nrow(outing_pitch_counts)
  
  graph5 <- ggplot(dpitchperf, aes(x = pitch_group, y = Outcome, group = Name, color = Name)) +
    geom_point() +
    geom_line() +
    geom_vline(aes(x = pitch_group, y = strike_pct), xintercept = avg_pitches_per_outing/10+1, linetype = "dotted", lwd = 1) +
    scale_x_discrete(limits = unique(d$pitch_group)) +
    labs(
      title = paste(pitcher, ".SLG and .OPS by Pitch Count"), 
      x = "Pitch Number",
      caption = paste("In this data set,",pitcher, "averaged", rounded_avg_pitches_per_outing, "pitches per outing over", num_outings, "outings.")
    ) +
    theme_bw() +
    scale_color_manual(values = c("blue", "brown")) +
    theme(
      plot.caption = element_text(size = 9)
    )
  
  return(graph5)
}

                  