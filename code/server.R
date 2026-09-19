################################################################################
# SERVER #######################################################################
################################################################################

function(input, output, session) {
  age_in <- reactive(input$age) |> debounce(200)
  egfr_in <- reactive(input$egfr) |> debounce(200)

  color_mode <- reactive({
    if (identical(input$color_mode, "dark")) "dark" else "light"
  })

  # Reactive dataset with mapped values
  filtered_curves <- reactive({
    req(input$sex, input$equation) # Ensure inputs exist

    distribs_curves |>
      filter(
        female == sex_map[[input$sex]],
        equation == equation_map[[input$equation]]
      )
  })

  filtered_percentile <- reactive({
    req(age_in(), egfr_in(), input$sex, input$equation) # Ensure inputs exist

    a <- age_in()
    g <- egfr_in()

    closest_percentile <- full_perc %>%
      filter(
        female == sex_map[input$sex],
        equation == equation_map[input$equation]
      ) |>
      filter(min(abs(age - a)) == abs(age - a)) |>
      filter(min(abs(eGFR - g)) == abs(eGFR - g)) |>
      mutate(
        percentile = if_else(percentile < 50, min(percentile), max(percentile))
      ) |>
      group_by(age, eGFR) |>
      slice(1) |> # will output 2 values if the 50th percentile is included, otherwise 1
      pull(percentile) |>
      median()

    if (length(closest_percentile) == 0) {
      return(NA)
    }
    closest_percentile
  })

  # -- Hover readout ---
  hover_columns <- reactive({
    cv <- filtered_curves()
    ages <- seq(ceiling(min(cv$age)), floor(max(cv$age)))
    at <- function(col) round(approx(cv$age, cv[[col]], xout = ages)$y)

    data.frame(
      age = ages,
      p10 = at("p10"),
      p25 = at("p25"),
      p50 = at("p50"),
      p75 = at("p75"),
      p90 = at("p90")
    )
  })

  output$Plot <- renderGirafe({
    cv <- filtered_curves()
    hv <- hover_columns()
    pal <- band_palette[[color_mode()]]
    a <- age_in()
    g <- egfr_in()
    pct <- filtered_percentile()

    y_range <- c(20, 120)

    tip <- paste0(
      "<b>Age ",
      hv$age,
      "</b><br/>",
      "90th&nbsp;&nbsp;",
      hv$p90,
      "<br/>",
      "75th&nbsp;&nbsp;",
      hv$p75,
      "<br/>",
      "<b>50th&nbsp;&nbsp;",
      hv$p50,
      "</b><br/>",
      "25th&nbsp;&nbsp;",
      hv$p25,
      "<br/>",
      "10th&nbsp;&nbsp;",
      hv$p10
    )
    # The patient's own column also reports where they sit.
    if (!is.null(a) && !is.na(a) && a %in% hv$age) {
      tip[hv$age == a] <- paste0(
        tip[hv$age == a],
        "<br/><span style='color:",
        pal$point,
        "'><b>This patient&nbsp;&nbsp;",
        g,
        " (",
        pct,
        "th)</b></span>"
      )
    }
    hv$tooltip <- tip

    p <- ggplot(cv, aes(x = age)) +

      # -- Ordered percentiles read as nested bands, the way a growth chart
      #    does, instead of five curves the reader has to match to a legend ---
      geom_ribbon(aes(ymin = p10, ymax = p90, fill = band_labels[["outer"]])) +
      geom_ribbon(aes(ymin = p25, ymax = p75, fill = band_labels[["inner"]])) +
      geom_line(
        aes(y = p50, color = band_labels[["median"]]),
        linewidth = 1.1
      ) +

      geom_hline(
        yintercept = egfr_reference_lines,
        linetype = "dashed",
        color = pal$muted,
        linewidth = 0.4
      ) +
      annotate(
        "text",
        x = min(cv$age) + 0.6,
        y = 62,
        hjust = 0,
        vjust = 0,
        label = "Chronic kidney disease threshold (60 mL/min/1.73m²)",
        size = 3.1,
        color = pal$muted
      ) +

      # The only interactive layer - see hover_columns() above.
      geom_rect_interactive(
        data = hv,
        inherit.aes = FALSE,
        aes(
          xmin = age - 0.5,
          xmax = age + 0.5,
          ymin = y_range[1],
          ymax = y_range[2],
          tooltip = tooltip,
          data_id = age
        ),
        fill = pal$ink,
        alpha = 0.01
      ) +

      # A surface-colored ring keeps the point legible wherever it lands.
      geom_point(
        aes(x = a, y = g),
        shape = 21,
        size = 4.2,
        fill = pal$point,
        color = pal$surface,
        stroke = 1.4
      ) +

      scale_x_continuous(breaks = seq(40, 100, by = 10)) +
      scale_y_continuous(breaks = seq(20, 120, by = 20)) +
      coord_cartesian(xlim = c(40, 100), ylim = y_range, expand = FALSE) +

      scale_fill_manual(
        values = setNames(
          c(pal$outer, pal$inner),
          unname(band_labels[c("outer", "inner")])
        ),
        breaks = unname(band_labels[c("outer", "inner")])
      ) +
      scale_color_manual(
        values = setNames(pal$median, band_labels[["median"]])
      ) +
      guides(fill = guide_legend(order = 1), color = guide_legend(order = 2)) +

      labs(
        x = "Age (years)",
        y = expression("eGFR (mL/min/1.73m"^2 * ")"),
        fill = NULL,
        color = NULL
      ) +

      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = pal$surface, color = NA),
        panel.background = element_rect(fill = pal$surface, color = NA),
        panel.grid.major = element_line(color = pal$grid, linewidth = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(color = pal$muted),
        axis.title = element_text(color = pal$muted),
        legend.position = "bottom",
        legend.text = element_text(color = pal$ink),
        legend.key.size = unit(0.14, "in"),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(6, 12, 2, 6)
      )

    girafe(
      ggobj = p,
      width_svg = 8,
      height_svg = 5.4,
      options = list(
        # Painting the hovered column is what turns it into a crosshair.
        opts_hover(
          css = paste0(
            "fill-opacity:0.14;fill:",
            pal$ink,
            ";stroke:none;cursor:crosshair;"
          )
        ),
        opts_tooltip(
          css = paste0(
            "background:",
            pal$surface,
            ";color:",
            pal$ink,
            ";border:1px solid ",
            pal$grid,
            ";border-radius:6px;padding:8px 10px;",
            "font-family:system-ui,sans-serif;font-size:12px;",
            "box-shadow:0 2px 8px rgba(0,0,0,.18);"
          ),
          offx = 12,
          offy = 12
        ),
        opts_zoom(min = 1, max = 4),
        opts_toolbar(
          saveaspng = TRUE,
          position = "topright",
          hidden = "selection"
        ),
        opts_sizing(rescale = TRUE)
      )
    )
  }) |>
    bindCache(input$sex, input$equation, age_in(), egfr_in(), color_mode())

  output$percentileValue <- renderText({
    paste0(filtered_percentile(), "th")
  })

  output$interpretation <- renderUI({
    percentile <- filtered_percentile()

    if (age_in() < 40 || age_in() > 100) {
      return(div(
        class = "alert alert-warning mb-0",
        bsicons::bs_icon("exclamation-triangle-fill"),
        " Please enter an age between 40 and 100."
      ))
    }

    status_text <- if (egfr_in() < 60) {
      HTML(
        "This eGFR is below 60 ml/min/1.73 m<sup>2</sup>. If it persists for 3 months,
           it meets the criteria for chronic kidney disease. In such cases, consider following KDIGO guidelines."
      )
    } else if (percentile < 25) {
      HTML(
        "This eGFR is above 60 ml/min/1.73 m<sup>2</sup>, but it is in a low percentile
           of the distribution. In this situation, consider repeating the test, using more
           accurate methods (such as combining creatinine and cystatin C), and
           closer monitoring to detect risk factors for development of chronic kidney disease."
      )
    } else if (percentile > 75) {
      HTML(
        "This eGFR is above 60 ml/min/1.73 m<sup>2</sup>, but it is in a high percentile
           of the distribution. Consider investigating reasons for potential overestimation of GFR,
           such as low muscle mass, and the use of more accurate estimates of GFR,
           such as combined creatinine–cystatin C–based equations."
      )
    } else {
      HTML(
        "This eGFR is above 60 ml/min/1.73 m<sup>2</sup> and within the normal variation
           of eGFR for age and sex. If comorbid conditions are present (such as diabetes,
           hypertension, or cardiovascular disease), consider evaluating kidney function again next year."
      )
    }

    div(status_text)
  })
}
