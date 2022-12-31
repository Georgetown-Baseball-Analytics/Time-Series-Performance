#setwd("C:/Users/rajmo/Documents/Baseball/Time-Series-Performance/scripts")
source("import_synergy_data.R")
source("sabermetric_analysis.R")

d <- read.csv(d)
d <- group_pitch_counts(d)
strikepct_innings(d)
strikepct_pitchcount(d)
kandbbrate_pitchcount(d)
performance_pitchcount(d)

#R markdown code _______________________________________________________________________________________________________

#plot_list <- list(graph1, graph2, graph3, graph4)
#file <- tempfile()
#saveRDS(plot_list, file)
#rmarkdown::render('rmarkdownfile.Rmd', params = list(file = file))