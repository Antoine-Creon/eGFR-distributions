################################################################################
# UI ###########################################################################
################################################################################

page_navbar(
  title = "eGFR Percentile Explorer",
  theme = app_theme,
  id = "main_nav",
  window_title = "eGFR Percentile Explorer",
  # Only the Calculator fills the window. About and Disclaimer are prose and
  # should scroll normally.
  fillable = "Calculator",
  header = tags$head(tags$style(HTML(
    "
    #Plot, #Plot > div { height: 100% !important; width: 100% !important; }
    #Plot svg.ggiraph-svg { height: 100% !important; width: 100% !important; }

    /* Below bslib's sm breakpoint the sidebar layout becomes a flow layout,
       which drops the flex fill - the chart card would otherwise collapse to
       its 300px floor and letterbox the taller mobile SVG. */
    @media (max-width: 575.98px) {
      .chart-body { min-height: 420px; }
    }
  "
  ))),

  ## Calculator ----------------------------------------------------------
  nav_panel(
    title = "Calculator",
    icon = bs_icon("graph-up"),

    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "Input parameters",
        width = 300,
        # On a phone the collapsed sidebar is an unlabeled toggle button, so
        # the inputs are invisible until tapped. "always-above" stacks them
        # over the chart and hides the toggle; desktop is unaffected.
        open = list(desktop = "open", mobile = "always-above"),
        selectInput(
          "sex",
          "Sex",
          choices = c("Male", "Female"),
          selected = "Female"
        ),
        numericInput("age", "Age (years)", value = 50, min = 40, max = 100),
        numericInput(
          "egfr",
          "eGFR (mL/min/1.73m²)",
          value = 90,
          min = 10,
          max = 129
        ),
        selectInput(
          "equation",
          "Equation",
          choices = c(
            "CKD-EPI 2009",
            "CKD-EPI 2021",
            "EKFC",
            "Revised Lund-Malmö"
          ),
          selected = "CKD-EPI 2021"
        ),

        tags$hr(class = "my-2"),

        tags$p(
          class = "small text-muted mb-0",
          "This tool helps visualize population-based estimated glomerular
                    filtration rate distributions based on age, sex and estimating equation.
                    Enter a value to compare it to the population. \nThis tool is primarily intended for physicians, but patients may discuss it with their doctors."
        )
      ),

      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(8, 4)),

        card(
          full_screen = TRUE,
          card_header(
            "Population-based estimated glomerular filtration rate distribution"
          ),
          card_body(
            class = "chart-body",
            padding = 0,
            min_height = "300px",
            girafeOutput("Plot", width = "100%", height = "100%")
          )
        ),

        layout_columns(
          col_widths = c(12, 12),
          row_heights = c(1, 2),
          value_box(
            title = "Percentile for this eGFR",
            value = textOutput("percentileValue"),
            showcase = bs_icon("bar-chart-line-fill"),
            showcase_layout = showcase_left_center(),
            theme = "primary"
          ),
          card(
            card_header("Interpretation"),
            card_body(
              class = "small",
              uiOutput("interpretation"),
              padding = 12
            )
          )
        )
      )
    )
  ),

  ## About ---------------------------------------------------------------
  nav_panel(
    title = "About",
    icon = bs_icon("info-circle"),

    layout_columns(
      col_widths = 12,

      card(
        card_header("Where these distributions come from"),
        card_body(
          tags$p(
            "The percentile curves are drawn from the Stockholm CREAtinine
            Measurements (SCREAM) project, a health care utilization database
            covering the residents of the Stockholm region, Sweden."
          ),
          tags$p(
            "Between 2006 and 2021 the cohort captured 1,179,501 adults aged
            40 to 100 years, about 80% of the region's population in that age
            range, contributing 6,914,993 annual eGFR measurements from
            routine outpatient creatinine tests."
          ),
          tags$p(
            tags$strong(
              "These are population-based distributions, not
            normative ones."
            ),
            " Nobody was excluded for having diabetes, hypertension or
            cardiovascular disease."
          ),
          tags$p(
            class = "mb-0 text-muted",
            "The data describe the Stockholm region, and extrapolation to
            other regions or countries should be done with caution.
            Distributions below age 40 are not available: routine creatinine
            testing is too infrequent in younger adults to support them."
          )
        )
      ),

      card(
        card_header("How to read the percentiles"),
        card_body(
          tags$p(
            "In the study behind this tool, an eGFR below the 25th percentile
            for a person's age and sex was associated with an increased rate of
            kidney failure with replacement therapy, and both low and high
            percentiles were associated with increased mortality. This held
            even among people whose eGFR was above 60 mL/min/1.73m²."
          )
        )
      ),

      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(6, 6)),

        card(
          card_header("Publication"),
          card_body(
            tags$p(
              class = "mb-2",
              "Yang Y. et al., Population-based estimated glomerular filtration rate distributions and associated health outcomes
                provide opportunities for early identification of and primary prevention of chronic kidney disease. Kidney International.
                2026; 0(0):S0085-2538(25)00989-5. ",
              ext_link(
                "https://doi.org/10.1016/j.kint.2025.11.009",
                "doi:10.1016/j.kint.2025.11.009"
              )
            ),
            tags$p(
              class = "mb-3",
              ext_link(
                "https://www.kidney-international.org/article/S0085-2538(25)00989-5/fulltext",
                "Read the full publication here."
              )
            ),
            tags$p(
              class = "mb-0 small text-muted",
              tags$strong("Citing this tool: "),
              "please cite the article above, together with the tool's address,
              ",
              ext_link(
                "https://scream.meb.ki.se/egfr-percentiles/",
                "scream.meb.ki.se/egfr-percentiles"
              ),
              "."
            )
          )
        ),

        card(
          card_header("The estimating equations"),
          card_body(
            tags$p(
              class = "small",
              "The distribution changes with the
              equation, so compare a value against the curves for the equation
              it was calculated with."
            ),
            tags$ul(
              class = "small mb-0 ps-3",
              tags$li(
                tags$strong("CKD-EPI 2009"),
                " - Levey AS, et al. Ann Intern Med. 2009;150:604-612.
                      Applied here assuming all individuals are non-Black."
              ),
              tags$li(
                tags$strong("CKD-EPI 2021"),
                " - Inker LA, et al. N Engl J Med. 2021;385:1737-1749."
              ),
              tags$li(
                tags$strong("EKFC"),
                " - Pottel H, et al. Ann Intern Med. 2021;174:183-191."
              ),
              tags$li(
                tags$strong("Revised Lund-Malmö"),
                " - Björk J, et al. Scand J Clin Lab Invest.
                      2011;71:232-239."
              )
            )
          )
        )
      ),

      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(6, 6)),

        card(
          card_header("The team"),
          card_body(
            tags$p(
              class = "mb-2",
              "This tool was developed as part of a study at Karolinska
              Institutet, Stockholm."
            ),
            tags$ul(
              class = "mb-3 ps-3",
              tags$li(tags$strong("Study lead: "), "Yuanhang Yang"),
              tags$li(
                tags$strong("Senior author and supervisor: "),
                "Juan Jesus Carrero"
              ),
              tags$li(
                tags$strong("Collaborators: "),
                "Antoine Créon, Andrew S Levey, Anne-Laure Faucon,
                      Aurora Caldinelli, Marie Evans, Arvid Sjölander,
                      Alberto Ortiz and Edouard L. Fu"
              )
            ),
            tags$p(
              class = "mb-0",
              ext_link(
                "https://ki.se/en/research/research-areas-centres-and-networks/research-groups/cardio-renal-epidemiology-juan-jesus-carreros-research-group?auHash=hhoISq-w5-ly2G1-qevFvi_lSMvpgmWlID4z8IesSCg#tab-start",
                "Visit the JJ Carrero group webpage."
              )
            )
          )
        ),

        card(
          card_header("Developer"),
          card_body(
            tags$p(
              class = "mb-2",
              ext_link("https://github.com/Antoine-Creon", "Antoine Créon"),
              "."
            ),
            tags$p(
              class = "mb-0",
              "If you experience any issue with the app, please ",
              ext_link(
                "https://github.com/Antoine-Creon/eGFR-distributions/issues",
                "file an issue on GitHub"
              ),
              "."
            )
          )
        )
      )
    )
  ),

  ## Disclaimer ----------------------------------------------------------
  nav_panel(
    title = "Disclaimer",
    icon = bs_icon("exclamation-triangle"),

    accordion(
      open = FALSE,

      accordion_panel(
        "Copyright",
        icon = bs_icon("c-circle"),
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

      accordion_panel(
        "Limitations of liability",
        icon = bs_icon("shield-exclamation"),
        "When you access this website, you agree that the authors and their institutions shall not be liable to you for any loss or injury caused in procuring, compiling, or delivering the information gained from the site. In no event will the authors and their institutions be liable to you or anyone else for any action taken by you on the basis of such information or for any incidental, consequential, special, or similar damages."
      ),

      accordion_panel(
        "Disclaimer",
        icon = bs_icon("exclamation-triangle"),
        "This website is provided on an “as is” basis. The authors and their institutions disclaim all responsibility for any loss, injury, claim, liability, or damage of any kind resulting from, arising out of, or any way related to any errors in or omissions from this Web site and the content, including but not limited to technical inaccuracies and typographical errors. The authors and their institutions does not warrant or present that the information available on or through the site will be correct, accurate, timely, or otherwise reliable. The authors and their institutions may make improvements and/or changes to its features, functionality, or content at any time."
      ),

      accordion_panel(
        "Not medical advice",
        icon = bs_icon("heart-pulse"),
        "The content contained on this site is not intended to and does not constitute medical advice, and no doctor/patient relationship is formed. The accuracy, completeness, adequacy, or currency of the content is not warranted or guaranteed. The use of information on the site or materials linked from the site is at the user’s own risk. The contents of the site, such as text, graphics, images and other materials are informational purposes only. The content is not intended to be a substitute for professional medical advice, diagnosis, or treatment. Users should always seek the advice of physicians or other qualified health providers with any questions regarding a medical condition. Users should never disregard professional medical advice or delay in seeking it because of something on the site."
      )
    )
  ),

  nav_spacer(),
  nav_item(input_dark_mode(id = "color_mode"))
)
