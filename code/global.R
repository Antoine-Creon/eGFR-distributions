################################################################################
# GLOBAL #######################################################################
################################################################################

library(shiny) # app framework
library(bslib) # Bootstrap 5 page, cards, value boxes, dark mode
library(bsicons) # icons for the value boxes
library(ggplot2) # percentile chart
library(ggiraph) # renders that chart as an interactive SVG
library(dplyr) # filtering and summarizing inside the reactives

egfr_data_path <- c(
  "egfr_data.rds",
  file.path("data", "egfr_data.rds"),
  file.path("..", "data", "egfr_data.rds")
)
egfr_data_path <- egfr_data_path[file.exists(egfr_data_path)]

if (length(egfr_data_path) == 0) {
  stop(
    "egfr_data.rds not found beside the app files, in data/, or in ../data/. ",
    "Working directory: ",
    getwd()
  )
}

egfr_data <- readRDS(egfr_data_path[1])

distribs_curves <- egfr_data$curves
full_perc <- egfr_data$indiv

rm(egfr_data)


## PLOTTING CONSTANTS ----------------------------------------------------------

band_palette <- list(
  light = list(
    outer = "#cde2fb",
    inner = "#86b6ef",
    median = "#1c5cab",
    point = "#eda100",
    ink = "#1a1a19",
    muted = "#6b6b68",
    grid = "#e4e3e0",
    surface = "#ffffff"
  ),
  dark = list(
    outer = "#184f95",
    inner = "#2a78d6",
    median = "#86b6ef",
    point = "#c98500",
    ink = "#f0efec",
    muted = "#a3a29e",
    grid = "#33332f",
    surface = "#1a1a19"
  )
)

band_labels <- c(
  outer = "10th-90th percentile",
  inner = "25th-75th percentile",
  median = "Median (50th)"
)

# Clinical reference lines. 60 is the CKD threshold when it persists 3 months.
egfr_reference_lines <- c(60, 90)

equation_map <- c(
  "CKD-EPI 2009" = "ckd_epi_2009",
  "CKD-EPI 2021" = "ckd_epi_2021",
  "EKFC" = "ekfc",
  "Revised Lund-Malmö" = "lund_malmo"
)

sex_map <- c("Male" = 0, "Female" = 1)


## HELPERS ---------------------------------------------------------------------

ext_link <- function(href, label) {
  tags$a(href = href, label, target = "_blank", rel = "noopener noreferrer")
}


## THEME  ----------------------------------------------------------------------

app_theme <- bs_theme(
  version = 5,
  preset = "shiny",
  base_font = font_collection(
    "system-ui",
    "Segoe UI",
    "Helvetica Neue",
    "Arial",
    "sans-serif"
  ),
  primary = "#1c5cab"
)
