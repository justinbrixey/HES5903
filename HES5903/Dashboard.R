library(shiny)
library(bslib)
library(dplyr)
library(fmsb)    
library(scales)
library(tidyr)
library(ggplot2)
library(gt)
library(shinyWidgets)
library(janitor)

# ---------- Global Section ----------

data <- read.csv("Data/strengthMetrics.csv", stringsAsFactors = FALSE) %>%
  mutate(
    UniqueID    = row_number(),
    player_type = case_when(
      !is.na(pitch_speed_mph) & (is.na(bat_speed_mph) | bat_speed_mph == "") ~ "Pitcher",
      !is.na(bat_speed_mph)   & (is.na(pitch_speed_mph) | pitch_speed_mph == "") ~ "Hitter",
      TRUE                                                                    ~ NA_character_
    )
  ) %>%
  group_by(player_type) %>%
  mutate(display_name = paste(player_type, row_number())) %>%
  ungroup() 

kpi_cols1 <- c("bat_speed_mph",
              "pitch_speed_mph",
              "peak_power_.w._mean_cmj",
              "peak_power_.w._mean_sj",
              "net_peak_vertical_force_.n._max_imtp",
              "best_rsi_.jump_height.contact_time._.m.s._mean_ht",
              "body_weight_.lbs.")

data <- data %>%
  mutate(across(all_of(kpi_cols1), ~ percent_rank(.) * 100, .names = "norm_{.col}"))

data <- data %>%
  drop_na(player_type) %>%
  mutate(
    speed = case_when(
      player_type == "Pitcher" ~ norm_pitch_speed_mph,
      player_type == "Hitter"  ~ norm_bat_speed_mph
    )
  )

kpi_cols <- c("speed",
              "peak_power_.w._mean_cmj",
              "peak_power_.w._mean_sj",
              "net_peak_vertical_force_.n._max_imtp",
              "best_rsi_.jump_height.contact_time._.m.s._mean_ht",
              "body_weight_.lbs.")

athlete_choices <- data %>%
  filter(player_type %in% c("Pitcher", "Hitter")) %>%
  filter(if_all(all_of(kpi_cols), ~ !is.na(.) & . != "")) %>%
  group_by(player_type) %>%
  sample_n(10) %>%
  ungroup()


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

pitch_model <- readRDS("Models/pitch_speed_model.rds")
bat_model   <- readRDS("Models/bat_speed_model.rds")

top_features <- c(
  "net_peak_vertical_force_.n._max_imtp",
  "body_weight_.lbs.",
  "best_rsi_.jump_height.contact_time._.m.s._mean_ht",
  "force_at_200ms_.n._max_imtp",
  "jump_height_.imp.mom._.cm._mean_sj",
  "peak_power_.w._mean_sj",
  "concentric_peak_force_.n._mean_cmj",
  "eccentric_peak_force_.n._mean_cmj",
  "jump_height_.imp.mom._.cm._mean_cmj",
  "peak_power_.w._mean_cmj"
)

create_chart <- function(metric_data, value, metric_name, selected_athlete, value2, unit) {
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
    scale_fill_manual(values = c("Poor" = "red1",
                                 "Moderate" = "orange1",
                                 "Proficient" = "#2ECC71",
                                 "Elite" = "#7DF9FF",
                                 "white" = "white")) +
    labs(
      title = bquote(bold(.(metric_name)) ~ "-" ~ .(value2) ~ .(unit)),
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.background  = element_rect(fill = "#FDF9D8", color = NA),
      panel.background = element_rect(fill = "#FDF9D8", color = NA),
      plot.title = element_text(hjust = 0.5, size = 17),
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


# ---------- UI Section ----------
ui <- page_navbar(
  title = "Baseball Strength Assessment Dashboard",
  theme = bs_theme(
    version    = 5,
    bootswatch = "cyborg",
    primary    = "#841617",
    secondary = "#323232",
    base_font  = font_google("Inter Tight")
  ),
  header = tagList(
    tags$style("h3 {font-weight: bolder; font-size: 1.2em; margin-bottom: 10px;}"),
    tags$style("h4 {font-weight: 200; font-size: 1.1em;}")
  ),
  
  # -------- Athlete Profile --------
  nav_panel("Athlete Profile",
            fluidRow(
              column(2,
                       div(
                         style = "background: #FDF9D8; color: white; padding: 20px; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.5);",
                         tags$div(
                           style = "color: #323232; margin-bottom: 15px;",
                           selectInput("athleteName", "Select Athlete:",
                                       choices = unique(athlete_choices$display_name))
                         ),
                         tags$div(
                             style = "text-align: center;",
                             imageOutput("headshot", height = "auto")
                             ),
                         br(),
                         h4("Name:", style = "color: #323232;"),
                         tags$h3(style = "color: #841617;", textOutput("athleteName", container = span)),
                           h4("Playing Level:", style = "color: #323232;"),
                           tags$h3(style = "color: #841617;", textOutput("athleteLevel", container = span)),
                           h4("Position:", style = "color: #323232;"),
                           tags$h3(style = "color: #841617;", textOutput("athletePos", container = span)),
                           h4("Body Weight:", style = "color: #323232;"),
                           tags$h3(style = "color: #841617;", textOutput("athleteBW", container = span))
                     ),
                     br(),
                     br(),
              ),
              column(4,
                     div(
                       style = "background: #FDF9D8; padding: 5px; border-radius: 8px;",
                       div(style = "text-align: center;",
                           h3("Velocity Metrics", style = "color: #323232;"),
                           fluidRow(
                             column(1),
                             column(5,
                                    div(
                                      style = "background: #323232; border-radius: 8px;",
                                      h4("Recorded Velocity", style = "color: #FDF9D8;"),
                                      tags$h3(style = "color: #FDF9D8;", textOutput("athleteVeloR")))),
                             column(5,
                                    div(
                                      style = "background: #841617; border-radius: 8px;",
                                      h4("Predicted Velocity", style = "color: #FDF9D8;"),
                                      tags$h3(style = "color: #FDF9D8;", textOutput("athleteVeloP")))),
                             column(1)
                           ),
                           h4("Recommendation: ", textOutput("athleteRec", container = span), style = "color: #323232;")                       ),
                     ),
                     br(),
                     div(
                       style = "background: #FDF9D8; padding: 5px; border-radius: 8px;",
                       div(style = "text-align: center;",
                           h3("Stength Metrics", style = "color: #323232;"),
                           fluidRow(
                             column(6,
                                    h4("IMTP:", style = "color: #323232;"),
                                    h4("Absolute Power", style = "color: #323232;"),
                                    tags$h3(style = "color: #841617;", textOutput("athleteIMTP")),
                                    h4("RHT:", style = "color: #323232;"),
                                    h4("Reactivity", style = "color: #323232;"),
                                    tags$h3(style = "color: #841617;", textOutput("athleteHop"))
                                    ),
                             column(6,
                                    h4("CMJ:", style = "color: #323232;"),
                                    h4("Ballistic Power", style = "color: #323232;"),
                                    tags$h3(style = "color: #841617;", textOutput("athleteCMJ")),
                                    h4("SJ:", style = "color: #323232;"),
                                    h4("Concentric Power", style = "color: #323232;"),
                                    tags$h3(style = "color: #841617;", textOutput("athleteSJ")))
                                    )
                           )
                       ),
                     br(),
                     div(
                       style = "background: #FDF9D8; padding: 5px; border-radius: 8px;",
                       div(
                         style = "text-align: center;",
                       h3("CMJ Ratings", style = "color: #323232;"),
                       plotOutput("jumpHeightChart", height = "90px"),
                       plotOutput("mrsiChart",       height = "90px"),
                       plotOutput("relPowerChart",   height = "90px")
                       )
                     ),
              ),
              column(6,
                     div(
                       style = "background: #FDF9D8; padding: 10px; border-radius: 8px;",
                       div(style = "text-align: center;",
                           h3("Radar Plot", style = "color: #323232;"),
                           fluidRow(
                             div(style = "display: inline-block; width: 45%; margin: 10px; color: #323232;",
                                 selectizeInput("comparisonPlayingLevels", "Compare by Playing Level:",
                                                choices = NULL, multiple = TRUE)
                             ),
                             div(style = "display: inline-block; width: 45%; margin: 10px; color: #323232",
                                 selectizeInput("comparisonSpeedGroups", "Compare by Speed Group:",
                                                choices = NULL, multiple = TRUE)
                             )
                           ),
                           plotOutput("radarPlot", width = '100%', height = "510px")
                       ),
                       br(),
                       br()
                     )
              )
            )
  ),
  
  # -------- Team Report --------
  nav_panel("Team Report",
            fluidRow(
              column(4,
                     div(style = "background: #FDF9D8; padding: 10px; border-radius: 8px; color: #323232",
                         selectInput(
                           inputId = "testSelect",
                           label   = "Test Type:",
                           choices = c(
                             "Countermovement Jump"     = "cmj",
                             "Squat Jump"               = "sj",
                             "Isometric Mid Thigh Pull" = "imtp"
                           ),
                           selected = "cmj",
                           width = "100%"
                         )
                     )
              ),
              column(4,
                     div(style = "background: #FDF9D8; padding: 10px; border-radius: 8px; color: #323232",
                         selectInput(
                           inputId = "PositionFilter",
                           label   = "Position:",
                           choices = sort(unique(data$player_type)),
                           selected = unique(data$player_type),
                           multiple = TRUE,
                           width = "100%"
                         )
                     )
              ),
              column(4,
                     div(style = "background: #FDF9D8; padding: 10px; border-radius: 8px; color: #323232",
                         selectInput(
                           inputId = "levelFilter",
                           label   = "Playing Level:",
                           choices = sort(unique(data$playing_level)),
                           selected = unique(data$playing_level),
                           multiple = TRUE,
                           width = "100%"
                         )
                     )
              )
            ),
            fluidRow(
              column(12,
                     div(style = "background: #841617; padding: 10px; border-radius: 8px;",
                         gt_output("teamTable")
                     )
              )
            )
  )
  
)


# ---------- Server Section ----------
server <- function(input, output, session) {

  selected_player <- reactive({
    req(input$athleteName)
    data %>% filter(display_name == input$athleteName)
  })

  output$headshot <- renderImage({
    list(src = paste0("Data/Recruit.jpg"),
         height = "250")
  }, deleteFile = FALSE)
  
  output$athleteName <- renderText({
    req(selected_player())
    paste(selected_player()$display_name)
  })
  
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
  
  output$athleteVeloR <- renderText({
    req(selected_player())
    player <- selected_player()
    
    if (player$player_type == "Pitcher") {
      paste(round(player$pitch_speed_mph, 1), "mph")
    } else if (player$player_type == "Hitter") {
      paste(round(player$bat_speed_mph, 1), "mph")
    } else {
      "N/A"
    }
  })
  
  predictedVelo <- reactive({
    req(selected_player())
    player <- selected_player()[1, ]
    
    feat_df  <- player[ , top_features, drop = FALSE ]
    feat_mat <- as.matrix(feat_df)
    
    model <- if (player$player_type == "Pitcher") pitch_model else bat_model
    
    as.numeric(predict(model, newdata = feat_mat))
  })
  
  output$athleteVeloP <- renderText({
    mph <- round(predictedVelo(), 1)
    paste0(mph, " mph")
  })
  
  output$athleteRec <- renderText({
    req(selected_player())
    player <- selected_player()[1, ]
    
    actual <- if (player$player_type == "Pitcher") {
      player$pitch_speed_mph
    } else {
      player$bat_speed_mph
    }
    
    if (actual > predictedVelo()) {
      "Strength Training Needed"
    } else {
      "Skill Specific Training Needed"
    }
  })
  
  output$athleteIMTP <- renderText({
    req(selected_player())
    paste(round(selected_player()$net_peak_vertical_force_.n._max_imtp, 1), "newtons")
  })
  
  output$athleteHop <- renderText({
    req(selected_player())
    paste(round(selected_player()$best_rsi_.jump_height.contact_time._.m.s._mean_ht, 1), "m/s")
  })
  
  output$athleteCMJ <- renderText({
    req(selected_player())
    paste(round(selected_player()$peak_power_.w._mean_cmj, 1), "watts")
  })
  
  output$athleteSJ <- renderText({
    req(selected_player())
    paste(round(selected_player()$peak_power_.w._mean_sj, 1), "watts")
  })
  
  observe({
    updateSelectizeInput(session, "comparisonPlayingLevels",
                         choices = sort(unique(data$playing_level)),
                         server = TRUE)
  })
  
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

  playing_level_avg <- reactive({
    req(input$comparisonPlayingLevels)
    norm_cols <- c("speed",
                   "norm_peak_power_.w._mean_cmj",
                   "norm_peak_power_.w._mean_sj",
                   "norm_net_peak_vertical_force_.n._max_imtp",
                   "norm_best_rsi_.jump_height.contact_time._.m.s._mean_ht",
                   "norm_body_weight_.lbs.")
    averages <- lapply(input$comparisonPlayingLevels, function(level) {
      df <- data %>% filter(playing_level == level)
      if(nrow(df) > 0) colMeans(df[, norm_cols], na.rm = TRUE)
      else rep(NA, length(norm_cols))
    })
    names(averages) <- input$comparisonPlayingLevels
    averages
  })

  speed_group_avg <- reactive({
    req(input$comparisonSpeedGroups)
    norm_cols <- c("speed",
                   "norm_peak_power_.w._mean_cmj",
                   "norm_peak_power_.w._mean_sj",
                   "norm_net_peak_vertical_force_.n._max_imtp",
                   "norm_best_rsi_.jump_height.contact_time._.m.s._mean_ht",
                   "norm_body_weight_.lbs.")
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
  
  output$radarPlot <- renderPlot({
    req(selected_player())
    norm_cols <- c("speed",
                   "norm_peak_power_.w._mean_cmj",
                   "norm_peak_power_.w._mean_sj",
                   "norm_net_peak_vertical_force_.n._max_imtp",
                   "norm_best_rsi_.jump_height.contact_time._.m.s._mean_ht",
                   "norm_body_weight_.lbs.")
    
    athlete_values <- selected_player()
    main_norm <- as.numeric(athlete_values %>% select(all_of(norm_cols)))
    
    series_matrix <- main_norm
    series_labels <- c(athlete_values$display_name)
    
    if (!is.null(input$comparisonPlayingLevels) && length(input$comparisonPlayingLevels) > 0) {
      pl_avgs <- playing_level_avg()
      for (lvl in names(pl_avgs)) {
        series_matrix <- rbind(series_matrix, pl_avgs[[lvl]])
        series_labels <- c(series_labels, paste("Level Avg:", lvl))
      }
    }
    
    if (!is.null(input$comparisonSpeedGroups) && length(input$comparisonSpeedGroups) > 0) {
      sg_avgs <- speed_group_avg()
      for (grp in names(sg_avgs)) {
        series_matrix <- rbind(series_matrix, sg_avgs[[grp]])
        series_labels <- c(series_labels, paste("Speed Group Avg:", grp))
      }
    }
    
    max_vals <- rep(100, length(norm_cols))
    min_vals <- rep(0, length(norm_cols))
    radar_data <- rbind(max_vals, min_vals, series_matrix)
    rownames(radar_data) <- c("Max", "Min", series_labels)
    radar_data <- as.data.frame(radar_data)
    
    num_series <- nrow(radar_data) - 2  
    series_colors <- if(num_series == 1) {
      "red4"
    } else {
      c("red4", rainbow(num_series - 1))
    }
    pfcolors <- sapply(series_colors, function(col) alpha(col, 0.7))
    
    compare_title <- if(length(series_labels) > 1)
      paste("vs", paste(series_labels[-1], collapse = ", ")) else ""
    final_title <- paste("IQR for", athlete_values$display_name, compare_title)
    
    op <- par(mar = c(1, 1, 1, 1))
    old_par <- par(bg = "#FDF9D8")
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
    par(old_par)
  } , width = 600, height = 500)
  
  output$jumpHeightChart <- renderPlot({
    req(selected_player())
    jump_height_cm <- selected_player()[["jump_height_.imp.mom._.cm._mean_cmj"]]
    jump_height <- round((jump_height_cm * 0.393701), 1)
    
    normative_data <- if(selected_player()[["player_type"]] == "Pitcher")
      pitcher_normative_data else position_normative_data
    
    metric_data <- normative_data %>% 
      filter(Metric == "Jump Height") %>%
      mutate(
        Range_Low = as.numeric(gsub("-.*", "", Range)),
        Range_High = as.numeric(gsub(".*-", "", Range)),
        Range_High = ifelse(is.na(Range_High), Range_Low, Range_High)
      )
    
    chart <- create_chart(metric_data, jump_height_cm, "Jump Height",
                          selected_player()$display_name, jump_height, "Inches")
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
  
  filtered_team_data <- reactive({
    req(input$levelFilter)
    data %>% 
      filter(playing_level %in% input$levelFilter,
             player_type    %in% input$PositionFilter) %>%
      janitor::clean_names()  
  })
  
  
  output$teamTable <- render_gt({
    req(input$testSelect)
    df <- filtered_team_data()
    suffix <- input$testSelect
    
    # find the cleaned names that end with "_cmj", "_sj", or "_imtp"
    matches <- grep(paste0("_", suffix, "$"), names(df), value = TRUE)
    
    # pick your columns (use the cleaned names)
    cols <- c("display_name", "playing_level", "body_weight_lbs", matches)
    df_sel <- df[, cols, drop = FALSE]
    
    gt(df_sel) %>%
      tab_header(
        title    = "Team Report",
        subtitle = switch(suffix,
                          cmj  = "Countermovement Jump Metrics",
                          sj   = "Squat Jump Metrics",
                          imtp = "Isometric Mid Thigh Pull Metrics")
      ) %>%
      fmt_number(columns = matches, decimals = 1)
  })
  
  
}

# ---------- Run ----------
shinyApp(ui = ui, server = server)
