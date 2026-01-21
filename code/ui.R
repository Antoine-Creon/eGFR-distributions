library(ggplot2)
library(readxl)
library(here)
library(shiny)
library(shinydashboard)
library(thematic)
library(bslib)
library(bsicons)
library(tidyverse)

distribs_for_plot <- read_delim("distribs_for_plot.csv")
full_perc <- read_delim("distribs_for_indiv.csv")

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


################################################################################
###                                   UI                                     ###
################################################################################

dashboardPage(
  dashboardHeader(title = "eGFR Percentile Explorer"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator", tabName = "Calculator", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Disclaimer", tabName = "disclaimer", icon = icon("info-circle"))
    )
  ),

  dashboardBody(
    tabItems(
      # Dashboard Tab
      tabItem(
        tabName = "Calculator",
        fluidRow(
          box(
            title = "About",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            "This tool helps visualize population-based estimated glomerular
                    filtration rate distributions based on age, sex and estimating equation.
                    Enter a value to compare it to the population. \nThis tool is primarily intended for physicians, but patients may discuss it with their doctors."
          ),

          box(
            title = "Input Parameters",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            selectInput(
              "sex",
              "Sex:",
              choices = c("Male", "Female"),
              selected = "Female"
            ),
            numericInput("age", "Age:", value = 50, min = 40, max = 100),
            numericInput("egfr", "eGFR:", value = 90, min = 10, max = 129),
            selectInput(
              "equation",
              "Equation:",
              choices = c(
                "CKD-EPI 2009",
                "CKD-EPI 2021",
                "EKFC",
                "Revised Lund-Malmö"
              ),
              selected = "CKD-EPI 2021"
            ) #,
            # actionButton("update", "Update Plot", class = "btn btn-primary")
          ),

          box(
            title = "Population-based eGFR distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotOutput("Plot")
          ),

          box(
            title = "eGFR Percentile and interpretation",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            valueBoxOutput("percentileBox", width = 4),
            valueBoxOutput("interpretationBox", width = 8)
          )
        )
      ),

      # About Tab

      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "Publication",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tags$div(
              "Yang Y. et al., Population-based estimated glomerular filtration rate distributions and associated health outcomes 
                provide opportunities for early identification of and primary prevention of chronic kidney disease. Kidney International. 
                2026; 0(0):S0085-2538(25)00989-5. doi:10.1016/j.kint.2025.11.009.",
              tags$br(),
              tags$a(
                href = "https://www.kidney-international.org/article/S0085-2538(25)00989-5/fulltext",
                "Read the full publication here.",
                target = "_blank"
              )
            )
          ),

          box(
            title = "The team",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            "This tool was developed as part of a study led by Yuanhang Yang and supervised by Juan Jesus Carrero
                    at Karolinska Institutet, in collaboration with Antoine Créon, Andrew S Levey,
                    Anne-Laure Faucon, Aurora Caldinelli, Marie Evans, Arvid Sjölander, Alberto Ortiz and Edouard L. Fu. \n",
            tags$a(
              href = "https://ki.se/en/research/research-areas-centres-and-networks/research-groups/cardio-renal-epidemiology-juan-jesus-carreros-research-group?auHash=hhoISq-w5-ly2G1-qevFvi_lSMvpgmWlID4z8IesSCg#tab-start",
              "Visit the JJ Carrero group webpage.",
              target = "_blank"
            )
          ),

          box(
            title = "Developer",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tags$p(
              tags$a(
                href = "https://github.com/Antoine-Creon",
                "Antoine Créon.",
                target = "_blank"
              )
            ),
            tags$p(
              "If you experience any issue with the app, please ",
              tags$a(
                href = "https://github.com/Antoine-Creon/eGFR-distributions/issues",
                "fill an issue on GitHub",
                target = "_blank"
              ),
              "."
            )
          )
        )
      ),
      tabItem(
        tabName = "disclaimer",
        fluidRow(
          box(
            title = "Copyright",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            " Copyright (C) 2025  Antoine CREON, JJ CARRERO group at Karolinska Institutet \n

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.
\n
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
\n
    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see https://www.gnu.org/licenses/."
          ),

          box(
            title = "Limitations of liability",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            "When you access this website, you agree that the authors and their institutions shall not be liable to you for any loss or injury caused in procuring, compiling, or delivering the information gained from the site. In no event will the authors and their institutions be liable to you or anyone else for any action taken by you on the basis of such information or for any incidental, consequential, special, or similar damages."
          ),

          box(
            title = "Disclaimer",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            "This website is provided on an “as is” basis. The authors and their institutions disclaim all responsibility for any loss, injury, claim, liability, or damage of any kind resulting from, arising out of, or any way related to any errors in or omissions from this Web site and the content, including but not limited to technical inaccuracies and typographical errors. The authors and their institutions does not warrant or present that the information available on or through the site will be correct, accurate, timely, or otherwise reliable. The authors and their institutions may make improvements and/or changes to its features, functionality, or content at any time."
          ),

          box(
            title = "Not medical advice",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            "The content contained on this site is not intended to and does not constitute medical advice, and no doctor/patient relationship is formed. The accuracy, completeness, adequacy, or currency of the content is not warranted or guaranteed. The use of information on the site or materials linked from the site is at the user’s own risk. The contents of the site, such as text, graphics, images and other materials are informational purposes only. The content is not intended to be a substitute for professional medical advice, diagnosis, or treatment. Users should always seek the advice of physicians or other qualified health providers with any questions regarding a medical condition. Users should never disregard professional medical advice or delay in seeking it because of something on the site."
          )
        )
      )
    )
  )
)
