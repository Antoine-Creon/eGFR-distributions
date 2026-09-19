library(shiny)          # app framework
library(shinydashboard) # dashboard layout, value and info boxes
library(ggplot2)        # percentile plot
library(dplyr)          # filtering and summarizing inside the reactives

# global.R is sourced once per R process, before ui.R and server.R, and its
# objects are visible to both. The data used to be read separately in each of
# those two files - the same 487,512 rows parsed twice per process, and once
# for nothing, since ui.R never touched them.
#
# The .rds is a serialized copy of the two CSVs in data/: readRDS takes 0.08 s
# where read_delim on the CSVs takes 1.4 s, and it carries the tibbles with the
# exact column types read_delim produced, so nothing downstream changes.
#
# The path is "../data" and not "data": runApp() sets the working directory to
# the app directory, which is code/, so a bare "data/" would resolve to
# code/data/. here::here() is not used either - it anchors on .git, which a
# deployment bundle does not contain.
egfr_data <- readRDS(file.path("..", "data", "egfr_data.rds"))

distribs_for_plot <- egfr_data$plot
full_perc <- egfr_data$indiv

rm(egfr_data)

# Prepare plotting
custom_colors <- c("#332288", "#117733", "#44AA99", "#88CCEE", "#DDCC77")

custom_lines <- c("solid", "solid", "solid", "solid", "solid")

legend <- c(
  "90th percentile",
  "75th percentile",
  "50th percentile (median)",
  "25th percentile",
  "10th percentile"
)
