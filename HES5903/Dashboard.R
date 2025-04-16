library(shiny)
library(bslib)
library(dplyr)
library(fmsb)    
library(scales)
library(tidyr)
library(ggplot2)

# ---------- Global Section ----------
data <- read.csv("Data/strengthMetrics.csv", stringsAsFactors = FALSE)
data$UniqueID <- 1:nrow(data)

# Determine the player type based on pitch and bat speed.
data <- data %>%
  mutate(player_type = ifelse(!is.na(pitch_speed_mph) & pitch_speed_mph != "" &
                                (is.na(bat_speed_mph) | bat_speed_mph == ""),
                              "Pitcher",
                              ifelse(!is.na(bat_speed_mph) & bat_speed_mph != "" &
                                       (is.na(pitch_speed_mph) | pitch_speed_mph == ""),
                                     "Hitter",
                                     NA)))

# Create a display name (e.g., "Pitcher 1", "Hitter 2")
data <- data %>%
  group_by(player_type) %>%
  mutate(display_name = ifelse(!is.na(player_type), paste(player_type, row_number()), NA)) %>%
  ungroup()

# Create a combined "speed" metric.
data$speed <- ifelse(!is.na(data$pitch_speed_mph) & data$pitch_speed_mph != "",
                     data$pitch_speed_mph, data$bat_speed_mph)

# Define the key KPI columns.
kpi_cols <- c("speed",
              "peak_power_.w._mean_cmj",
              "peak_power_.w._mean_sj",
              "net_peak_vertical_force_.n._max_imtp",
              "best_rsi_.jump_height.contact_time._.m.s._mean_ht",
              "body_weight_.lbs.")

# Normalize each KPI on a 0–100 scale based on their percentile rank.
data <- data %>%
  mutate(across(all_of(kpi_cols), ~ percent_rank(.) * 100, .names = "norm_{.col}"))

# For selection, sample 10 athletes per group ("Pitcher" and "Hitter")
athlete_choices <- data %>%
  filter(player_type %in% c("Pitcher", "Hitter")) %>%
  group_by(player_type) %>%
  sample_n(10) %>%
  ungroup()

# --- The “Don’t-Change” Code for Normative Data and Chart Creation --- #

position_normative_data <- data.frame(
  Metric = c("Jump Height", "Jump Height", "Jump Height", "Jump Height",
             "Relative Power", "Relative Power", "Relative Power", "Relative Power",
             "mRSI", "mRSI", "mRSI", "mRSI",
             "TimetoTakeoff", "TimetoTakeoff", "TimetoTakeoff", "TimetoTakeoff",
             "Jump Momentum", "Jump Momentum", "Jump Momentum", "Jump Momentum"),
  Classification = c("Poor", "Moderate", "Proficient", "Elite",
                     "Poor", "Moderate", "Proficient", "Elite",
                     "Poor", "Moderate", "Proficient", "Elite",
                     "Poor", "Moderate", "Proficient", "Elite",
                     "Poor", "Moderate", "Proficient", "Elite"),
  Range = c("33.00-39.99", "40.00-44.99", "45.00-49.99", "50.00-56.00",
            "47.0-56.49", "56.50-62.19", "62.20-67.19", "67.20-79.5",
            "0.34-0.449", "0.45-0.539", "0.54-0.609", "0.61-0.77",
            "0.97-1.25", "0.85-0.967", "0.78-0.849", "0.67-0.779",
            "225-255.99", "256-277.99", "278-305.99", "306-340")
)

pitcher_normative_data <- data.frame(
  Metric = c("Jump Height", "Jump Height", "Jump Height", "Jump Height",
             "Relative Power", "Relative Power", "Relative Power", "Relative Power",
             "mRSI", "mRSI", "mRSI", "mRSI",
             "TimetoTakeoff", "TimetoTakeoff", "TimetoTakeoff", "TimetoTakeoff",
             "Jump Momentum", "Jump Momentum", "Jump Momentum", "Jump Momentum"),
  Classification = c("Poor", "Moderate", "Proficient", "Elite",
                     "Poor", "Moderate", "Proficient", "Elite",
                     "Poor", "Moderate", "Proficient", "Elite",
                     "Poor", "Moderate", "Proficient", "Elite",
                     "Poor", "Moderate", "Proficient", "Elite"),
  Range = c("36.00-41.99", "42.00-44.99", "45.00-49.99", "50.00-58.00",
            "51.40-57.49", "57.50-61.19", "61.20-66.99", "67.00-79.50",
            "0.37-0.479", "0.48-0.549", "0.55-0.619", "0.62-0.79",
            "0.94-1.15", "0.85-0.939", "0.78-0.849", "0.67-0.779",
            "235-270.99", "271-289.49", "289.5-307.99", "308-355")
)

create_chart <- function(metric_data, value, metric_name, selected_athlete, value2, unit) {
  # Calculate total width and define sections.
  overall_min <- min(metric_data$Range_Low)
  overall_max <- max(metric_data$Range_High)
  total_width <- overall_max - overall_min
  section_width <- total_width / 4
  
  metric_data <- metric_data %>%
    arrange(match(Classification, c("Poor", "Moderate", "Proficient", "Elite"))) %>%
    mutate(
      xmin = overall_min + (0:(nrow(metric_data) - 1)) * section_width,
      xmax = xmin + section_width
    )
  
  highlight_class <- if (metric_name == "Time to Takeoff") {
    if (value > overall_max) "Poor" else if (value < overall_min) "Elite" else {
      metric_data %>% filter(value >= Range_Low & value <= Range_High) %>% pull(Classification)
    }
  } else {
    if (value < overall_min) "Poor" else if (value > overall_max) "Elite" else {
      metric_data %>% filter(value >= Range_Low & value <= Range_High) %>% pull(Classification)
    }
  }
  
  metric_data <- metric_data %>%
    mutate(fill_color = ifelse(Classification == highlight_class, Classification, "white"))
  
  marker_percentage <- metric_data %>%
    filter(Classification == highlight_class) %>%
    mutate(
      scaled_value = if (metric_name == "Time to Takeoff") {
        (Range_High - value) / (Range_High - Range_Low)
      } else {
        (value - Range_Low) / (Range_High - Range_Low)
      },
      marker_position = xmin + scaled_value * section_width
    ) %>%
    pull(marker_position)
  
  marker_position <- max(min(marker_percentage, overall_max), overall_min)
  barrier_positions <- metric_data$xmax[-nrow(metric_data)]
  
  chart <- ggplot() +
    geom_rect(aes(xmin = overall_min, xmax = overall_max, ymin = 0.4, ymax = 0.6), fill = "grey40") +
    geom_rect(data = metric_data,
              aes(xmin = xmin, xmax = xmax, ymin = 0.4, ymax = 0.6, fill = fill_color),
              alpha = 0.8) +
    geom_segment(data = data.frame(x = barrier_positions),
                 aes(x = x, xend = x, y = 0.3, yend = 0.7),
                 color = "black", linewidth = 0.8) +
    geom_point(data = data.frame(x = marker_position, y = 0.5),
               aes(x = x, y = y),
               color = "black", size = 7, shape = 18, fill = "black") +
    scale_fill_manual(values = c("Poor" = "#E63946",
                                 "Moderate" = "#e68a00",
                                 "Proficient" = "#2ECC71",
                                 "Elite" = "#3498DB",
                                 "white" = "white")) +
    labs(
      title = bquote(bold(.(metric_name)) ~ "-" ~ .(value2) ~ .(unit)),
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.text.x = element_text(size = 15, face = "bold"),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = (metric_data$xmin + metric_data$xmax) / 2,
                       labels = metric_data$Classification)
  
  print(chart)
}

# --- End of “Don’t-Change” Code --- #

# ---------- UI Section ----------
ui <- page_navbar(
  title = "HES 5903: Baseball Strength Assessment Dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",
    base_font = font_google("Inter Tight")
  ),
  header = tagList(
    tags$style("h3 {font-family: 'Inter Tight'; font-weight:600; font-size:1.9em; line-height: 25px;}"),
    tags$style("h4 {font-family: 'Inter Tight'; font-weight:300; font-size:1.5em; line-height: 1px;}")
  ),
  nav_panel("Athlete Profile",
            fluidRow(
              column(2,
                     div(
                       style = "background: #841617; color: white; padding: 20px;
                                border-radius: 8px; box-shadow: 0px 4px 8px rgba(0,0,0,0.5);",
                       tags$div(
                         style = "color: white; margin-bottom: 15px;",
                         selectInput("athleteName", "Select Athlete:",
                                        choices = unique(athlete_choices$display_name))
                       ),
                       tags$div(
                         style = "text-align: center;",
                         imageOutput("headshot", height = "auto")
                       ),
                       br(),
                       h4("Position:", style = "color: white;"),
                       br(),
                       tags$h3(style = "color: white;", textOutput("athletePos", container = span)),
                       br(),
                       h4("Playing Level:", style = "color: white;"),
                       br(),
                       tags$h3(style = "color: white;", textOutput("athleteLevel", container = span)),
                       br(),
                       h4("Body Weight:", style = "color: white;"),
                       br(),
                       tags$h3(style = "color: white;", textOutput("athleteBW", container = span))
                     )
              ),
              # In column 3 we show some CMJ metric charts.
              column(3,
                     div(style = "text-align: center;",
                         h3("Performance Metrics"),
                         br(),
                         br(),
                         h4("Recorded Velocity"),
                         br(),
                         br(),
                         br(),
                         h4("Predicted Velocity"),
                         br(),
                         br(),
                         br(),
                         h4("Absolute Power"),
                         br(),
                         br(),
                         br(),
                         h4("Ballistic Power"),
                         br(),
                         br(),
                         br(),
                         h4("Concentric Power"),
                         br(),
                         br(),
                         br(),
                         plotOutput("jumpHeightChart", height = "90px"),
                         br(),
                         plotOutput("mrsiChart", height = "90px"),
                         br(),
                         plotOutput("relPowerChart", height = "90px")
                     )
              ),
              # column(1),
              column(6,
                     div(style = "text-align: center;",
                         fluidRow(
                           div(style = "display: inline-block; width: 60%; margin: 10px;",
                               selectizeInput("comparisonPlayingLevels", "Compare by Playing Level:",
                                              choices = NULL, multiple = TRUE)
                           ),
                           div(style = "display: inline-block; width: 30%; margin: 10px;",
                               selectizeInput("comparisonSpeedGroups", "Compare by Speed Group:",
                                              choices = NULL, multiple = TRUE)
                           )
                         ),
                         plotOutput("radarPlot", width = '100%')
                     )
              )
              
              
            )
  ),
  nav_panel("Team Report")
)

# ---------- Server Section ----------
server <- function(input, output, session) {
  
  # Reactive for the selected athlete.
  selected_player <- reactive({
    req(input$athleteName)
    data %>% filter(display_name == input$athleteName)
  })
  
  # Render the athlete headshot.
  output$headshot <- renderImage({
    list(src = paste0("Data/Recruit.jpg"),
         height = "350")
  }, deleteFile = FALSE)
  
  output$athletePos <- renderText({
    req(selected_player())
    paste(selected_player()$player_type)
  })
  
  output$athleteLevel <- renderText({
    req(selected_player())
    paste(selected_player()$playing_level)
  })
  
  output$athleteBW <- renderText({
    req(selected_player())
    paste(round(selected_player()$body_weight_.lbs., 1), "lbs")
  })
  
  # Update the choices for playing level comparisons.
  observe({
    updateSelectizeInput(session, "comparisonPlayingLevels",
                         choices = sort(unique(data$playing_level)),
                         server = TRUE)
  })
  
  # Update the choices for speed group comparisons based on the main athlete's speed type.
  observe({
    req(selected_player())
    athlete <- selected_player()[1, ]
    if (!is.na(athlete$pitch_speed_mph) && athlete$pitch_speed_mph != "") {
      updateSelectizeInput(session, "comparisonSpeedGroups",
                           choices = sort(unique(data$pitch_speed_mph_group)),
                           server = TRUE)
    } else {
      updateSelectizeInput(session, "comparisonSpeedGroups",
                           choices = sort(unique(data$bat_speed_mph_group)),
                           server = TRUE)
    }
  })
  
  # Reactive for data filtered by a given playing level (returns a list of averages)
  playing_level_avg <- reactive({
    req(input$comparisonPlayingLevels)
    norm_cols <- paste0("norm_", kpi_cols)
    averages <- lapply(input$comparisonPlayingLevels, function(level) {
      df <- data %>% filter(playing_level == level)
      if(nrow(df) > 0) colMeans(df[, norm_cols], na.rm = TRUE)
      else rep(NA, length(norm_cols))
    })
    names(averages) <- input$comparisonPlayingLevels
    averages
  })
  
  # Reactive for data filtered by a given speed group (returns a list of averages)
  speed_group_avg <- reactive({
    req(input$comparisonSpeedGroups)
    norm_cols <- paste0("norm_", kpi_cols)
    # Determine which speed group column to use based on the selected athlete.
    athlete <- selected_player()[1, ]
    if (!is.na(athlete$pitch_speed_mph) && athlete$pitch_speed_mph != "") {
      group_col <- "pitch_speed_mph_group"
    } else {
      group_col <- "bat_speed_mph_group"
    }
    averages <- lapply(input$comparisonSpeedGroups, function(grp) {
      df <- data %>% filter((!!sym(group_col)) == grp)
      if(nrow(df) > 0) colMeans(df[, norm_cols], na.rm = TRUE)
      else rep(NA, length(norm_cols))
    })
    names(averages) <- input$comparisonSpeedGroups
    averages
  })
  
  # Radar plot for overall normalized KPI values with comparisons.
  output$radarPlot <- renderPlot({
    req(selected_player())
    norm_cols <- paste0("norm_", kpi_cols)
    
    # Main athlete's normalized KPI vector.
    athlete_values <- selected_player()
    main_norm <- as.numeric(athlete_values %>% select(all_of(norm_cols)))
    
    # Initialize series matrix with the main athlete.
    series_matrix <- main_norm
    series_labels <- c(athlete_values$display_name)
    
    # Append playing level averages if provided.
    if (!is.null(input$comparisonPlayingLevels) && length(input$comparisonPlayingLevels) > 0) {
      pl_avgs <- playing_level_avg()
      for (lvl in names(pl_avgs)) {
        series_matrix <- rbind(series_matrix, pl_avgs[[lvl]])
        series_labels <- c(series_labels, paste("Level Avg:", lvl))
      }
    }
    
    # Append speed group averages if provided.
    if (!is.null(input$comparisonSpeedGroups) && length(input$comparisonSpeedGroups) > 0) {
      sg_avgs <- speed_group_avg()
      for (grp in names(sg_avgs)) {
        series_matrix <- rbind(series_matrix, sg_avgs[[grp]])
        series_labels <- c(series_labels, paste("Speed Group Avg:", grp))
      }
    }
    
    # Construct full radar data matrix with fixed maximum and minimum rows.
    max_vals <- rep(100, length(norm_cols))
    min_vals <- rep(0, length(norm_cols))
    radar_data <- rbind(max_vals, min_vals, series_matrix)
    rownames(radar_data) <- c("Max", "Min", series_labels)
    radar_data <- as.data.frame(radar_data)
    
    # Create a color vector for the series (first series is main athlete, others are comparisons).
    num_series <- nrow(radar_data) - 2  # Exclude max and min rows.
    # For simplicity, main athlete in blue and others in a generated palette.
    series_colors <- if(num_series == 1) {
      "red4"
    } else {
      c("red4", rainbow(num_series - 1))
    }
    pfcolors <- sapply(series_colors, function(col) alpha(col, 0.7))
    
    # Title: list main athlete and names of comparisons.
    compare_title <- if(length(series_labels) > 1)
      paste("vs", paste(series_labels[-1], collapse = ", ")) else ""
    final_title <- paste("IQR Chart for", athlete_values$display_name, compare_title)
    
    op <- par(mar = c(1, 1, 1, 1))
    fmsb::radarchart(
      radar_data,
      axistype = 1,
      pcol = series_colors,
      pfcol = pfcolors,
      plwd = 3,
      cglcol = "black",
      cglty = 2,
      axislabcol = "gray40",
      vlabels = c("Speed", "Ballistic Power", "Concentric Power", "Absolute Power", "Reactivity", "Body Weight"),
      caxislabels = c("0", "25", "50", "75", "100"),
      cglwd = 1,
      vlcex = 1.5,
      title = final_title
    )
    par(op)
  } , width = 800, height = 700)
  
  # ----------------- CMJ Metric Chart Outputs ----------------- #
  
  output$jumpHeightChart <- renderPlot({
    req(selected_player())
    jump_height <- selected_player()[["jump_height_.imp.mom._.cm._mean_cmj"]]
    
    # Choose normative data based on player type.
    normative_data <- if(selected_player()[["player_type"]] == "Pitcher")
      pitcher_normative_data else position_normative_data
    
    metric_data <- normative_data %>% 
      filter(Metric == "Jump Height") %>%
      mutate(
        Range_Low = as.numeric(gsub("-.*", "", Range)),
        Range_High = as.numeric(gsub(".*-", "", Range)),
        Range_High = ifelse(is.na(Range_High), Range_Low, Range_High)
      )
    
    chart <- create_chart(metric_data, jump_height, "Jump Height",
                          selected_player()$display_name, jump_height, "cm")
    print(chart)
  })
  
  output$mrsiChart <- renderPlot({
    req(selected_player())
    mRSI <- selected_player()[["rsi.modified_.m.s._mean_cmj"]]
    
    normative_data <- if(selected_player()[["player_type"]] == "Pitcher")
      pitcher_normative_data else position_normative_data
    
    metric_data <- normative_data %>% 
      filter(Metric == "mRSI") %>%
      mutate(
        Range_Low = as.numeric(gsub("-.*", "", Range)),
        Range_High = as.numeric(gsub(".*-", "", Range)),
        Range_High = ifelse(is.na(Range_High), Range_Low, Range_High)
      )
    
    chart <- create_chart(metric_data, mRSI, "mRSI",
                          selected_player()$display_name, mRSI, "")
    print(chart)
  })
  
  output$relPowerChart <- renderPlot({
    req(selected_player())
    rel_power <- selected_player()[["peak_power_._bm_.w.kg._mean_cmj"]]
    
    normative_data <- if(selected_player()[["player_type"]] == "Pitcher")
      pitcher_normative_data else position_normative_data
    
    metric_data <- normative_data %>% 
      filter(Metric == "Relative Power") %>%
      mutate(
        Range_Low = as.numeric(gsub("-.*", "", Range)),
        Range_High = as.numeric(gsub(".*-", "", Range)),
        Range_High = ifelse(is.na(Range_High), Range_Low, Range_High)
      )
    
    chart <- create_chart(metric_data, rel_power, "Relative Power",
                          selected_player()$display_name, rel_power, "W/kg")
    print(chart)
  })
  
}

# ---------- Run the Application ----------
shinyApp(ui = ui, server = server)
