# Define Server

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

function(input, output, session) {

  # Define mapping for sex input
  sex_map <- c("Male" = 0, "Female" = 1)
  equation_map <- c("CKD-EPI 2009" = "ckd_epi_2009",
                    "CKD-EPI 2021" = "ckd_epi_2021",
                    "EKFC" = "ekfc",
                    "Revised Lund-Malmö" = "lund_malmo")

  # Reactive dataset with mapped values
  filtered_data <- reactive({
    req(input$sex, input$equation)  # Ensure inputs exist

    distribs_for_plot %>%
      filter(female == sex_map[input$sex], equation == equation_map[input$equation])
  })

  filtered_percentile <- reactive({
    req(input$age, input$egfr, input$sex, input$equation)  # Ensure inputs exist

    closest_percentile <- full_perc %>%
      filter(female == sex_map[input$sex], equation == equation_map[input$equation]) |>
      filter(min(abs(age - input$age)) == abs(age - input$age)) |>
      filter(min(abs(eGFR - input$egfr)) == abs(eGFR - input$egfr)) |>
      mutate(percentile = if_else(percentile < 50, min(percentile), max(percentile))) |>
      group_by(age, eGFR) |>
      slice(1) |> # will output 2 values if the 50th percentile is included, otherwise 1
      pull(percentile) |>
      median()

    if (length(closest_percentile) == 0) return(NA)
    closest_percentile
  })

  # Placeholder for plot
  output$Plot <- renderPlot({

    data <- filtered_data()

    # Draw a plot
    data |>
      ggplot(aes(x = age)) +
      geom_smooth(aes(y = p90, color = legend[1], linetype = legend[1]), method = "loess", se = FALSE, size = 0.5) +
      geom_smooth(aes(y = p75, color = legend[2], linetype = legend[2]), method = "loess", se = FALSE, size = 0.5) +
      geom_smooth(aes(y = p50, color = legend[3], linetype = legend[3]), method = "loess", se = FALSE, size = 1.5) +
      geom_smooth(aes(y = p25, color = legend[4], linetype = legend[4]), method = "loess", se = FALSE, size = 0.5) +
      geom_smooth(aes(y = p10, color = legend[5], linetype = legend[5]), method = "loess", se = FALSE, size = 0.5) +

      geom_point(aes(x = input$age, y = input$egfr), color = "darkorange", size = 4) +

      scale_x_continuous(limits = c(40, 100), breaks = seq(40, 100, by = 5)) +
      scale_y_continuous(limits = c(20, 120), breaks = seq(20, 120, by = 10)) +
      scale_color_manual(values = custom_colors, breaks = legend) +
      scale_linetype_manual(values = custom_lines, breaks = legend) +

      geom_hline(yintercept = c(60, 90), linetype = "dashed", color = "grey60", size = 1) +

      theme_minimal() +
      theme(
        panel.border = element_rect(color = "black", size = 1, fill = NA),
        plot.title = element_text(size = 18, hjust = 0.5, vjust = 1), # Center the title
        panel.grid.major = element_line(color = "grey80", size = 0.2),  # Lighter, thinner major gridlines
        panel.grid.minor = element_line(color = "grey90", size = 0.05),  # Subtle minor gridlines

        # Improved Legend Customization
        legend.position = c(0.9955, 0.9965), # Move legend closer to the top-right corner
        legend.justification = c("right", "top"), # Anchor the legend to its top-right corner
        legend.text = element_text(size = 8), # Change legend text size
        legend.margin = margin(1, 2, 1, 1), # Adjust margins (top, right, bottom, left)
        legend.key.size = unit(0.13, "in"), # Control size of legend keys
        legend.key.width = unit(0.4, "in"), # Add space between keys and text
        legend.spacing.y = unit(0, "in"), # Reduce vertical spacing between items
        legend.text.align = 0, # Align legend text to the left
        legend.background = element_rect(fill = "white", color = "white", size = 0.2) # Box around the legend
      ) +
      labs(
        #title = "(A) Male",
        #title = "(B) Female",
        x = "Age (years)",
        y = expression("eGFR (mL/min/1.73m"^2*")"),
        color = NULL, # Removes the legend title for the "color" aesthetic
        linetype = NULL # Removes the legend title for "linetype"
      )
  })

  output$percentileBox <- renderValueBox({
    percentile <- filtered_percentile()
    valueBox(paste0(percentile, "th"),
             "Percentile",
             icon = icon("stats", lib = "glyphicon"),
             color = "blue"
             )
  })

  output$interpretationBox <- renderInfoBox({
    data <- filtered_data()
    percentile <- filtered_percentile()

    if (input$age < 40 || input$age > 100) {
      return(infoBox(
        title = "Incorrect input",
        value = "Please enter an age between 40 and 100.",
        icon = icon("exclamation-triangle"),
        color = "navy",
        fill = TRUE
      ))
    }

    status_text <-  if (input$egfr < 60) {
      HTML("This eGFR is below 60 ml/min/1.73 m<sup>2</sup>. If it persists for 3 months,
           it meets the criteria for chronic kidney disease. In such cases, consider following KDIGO guidelines.")

    } else if (percentile < 25) {
      HTML(
        "This eGFR is above 60 ml/min/1.73 m<sup>2</sup>, but it is in a low percentile
           of the distribution. In this situation, consider repeating the test, using more
           accurate methods (such as combining creatinine and cystatin C), and
           closer monitoring to detect risk factors for development of chronic kidney disease."
      )

    }  else if (percentile > 75) {
      HTML(
        "This eGFR is above 60 ml/min/1.73 m<sup>2</sup>, but it is in a high percentile
           of the distribution. Consider investigating reasons for potential overestimation of GFR,
           such as low muscle mass, and the use of more accurate estimates of GFR,
           such as combined creatinine–cystatin C–based equations."
      )

    } else {
      HTML("This eGFR is above 60 ml/min/1.73 m<sup>2</sup> and within the normal variation
           of eGFR for age and sex. If comorbid conditions are present (such as diabetes,
           hypertension, or cardiovascular disease), consider evaluating kidney function again next year.")
    }

    infoBox(
      " ",
      status_text,
      fill = FALSE,
      icon = icon("hand-point-right", lib = "font-awesome"),
      color = "blue"
    )
  })
}
