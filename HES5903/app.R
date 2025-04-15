# Load required libraries
library(shiny)
library(dplyr)
library(fmsb)    # For the radar chart
library(scales)  # For transparency adjustments

# ---------- Global Section ----------
# Read in the data (assumes file is at the specified location)
data <- read.csv("Data/strengthMetrics.csv", stringsAsFactors = FALSE)

# Create a UniqueID column as the row number (this remains unchanged for internal use)
data$UniqueID <- 1:nrow(data)

# Dynamically assign the player type:
#   - "Pitcher" if the player has a non-missing, non-empty pitch_speed_mph AND bat_speed_mph is missing or empty.
#   - "Hitter" if the player has a non-missing, non-empty bat_speed_mph AND pitch_speed_mph is missing or empty.
#   - Otherwise (if both are provided), assign NA.
data <- data %>%
  mutate(player_type = ifelse(!is.na(pitch_speed_mph) & pitch_speed_mph != "" & 
                                (is.na(bat_speed_mph) | bat_speed_mph == ""),
                              "Pitcher",
                              ifelse(!is.na(bat_speed_mph) & bat_speed_mph != "" & 
                                       (is.na(pitch_speed_mph) | pitch_speed_mph == ""),
                                     "Hitter",
                                     NA)))

# Create a display name for selection:
# For each non-missing player_type, assign a name like "Pitcher 1", "Pitcher 2", etc.
data <- data %>%
  group_by(player_type) %>%
  mutate(display_name = ifelse(!is.na(player_type), paste(player_type, row_number()), NA)) %>%
  ungroup()

# Create a combined "speed" metric:
# Use pitch_speed_mph if available (non-missing and non-empty), otherwise use bat_speed_mph.
data$speed <- ifelse(!is.na(data$pitch_speed_mph) & data$pitch_speed_mph != "",
                     data$pitch_speed_mph, data$bat_speed_mph)

# Define the important KPI columns for the radar chart.
# Here we use the combined "speed" along with the others.
kpi_cols <- c("speed",
              "peak_power_.w._mean_cmj",
              "peak_power_.w._mean_sj",
              "net_peak_vertical_force_.n._max_imtp",
              "best_rsi_.jump_height.contact_time._.m.s._mean_ht",
              "body_weight_.lbs.")

# Normalize each KPI to a 0-100 scale using percentile rank.
# (A score of 75 means the athlete is in the 75th percentile for that KPI.)
data <- data %>%
  mutate(across(all_of(kpi_cols), ~ percent_rank(.) * 100, .names = "norm_{.col}"))

# ---------- UI Section ----------
ui <- fluidPage(
  titlePanel("Athlete KPI Radar Chart"),
  sidebarLayout(
    sidebarPanel(
      # Select input for athlete selection (only non-dual players: those with non-NA player_type)
      selectizeInput("athlete", "Select Athlete:", choices = NULL),
      # Select input for playing level comparison
      selectizeInput("playing_level", "Select Playing Level:", choices = NULL),
      # Select input for speed group comparison (updates based on athlete's speed type)
      selectizeInput("speed_group", "Select Speed Group Comparison:", choices = NULL)
    ),
    mainPanel(
      br(),
      plotOutput("radarPlot")
    )
  )
)

# ---------- Server Section ----------
server <- function(input, output, session) {
  
  # Update the athlete selectize input with only players who have a non-missing player_type.
  observe({
    athlete_choices <- data %>% filter(!is.na(player_type))
    updateSelectizeInput(
      session, "athlete",
      choices = setNames(athlete_choices$UniqueID, athlete_choices$display_name),
      server = TRUE
    )
  })
  
  # Update the playing level selectize input with the unique playing levels.
  updateSelectizeInput(
    session, "playing_level",
    choices = sort(unique(data$playing_level)),
    server = TRUE
  )
  
  # Reactive expression for the selected athlete's data.
  selected_data <- reactive({
    req(input$athlete)
    data %>% filter(UniqueID == as.numeric(input$athlete))
  })
  
  # Update the speed group selectize input based on the selected athlete's available speed.
  observe({
    req(selected_data())
    athlete_row <- selected_data()[1, ]
    if (!is.na(athlete_row$pitch_speed_mph) && athlete_row$pitch_speed_mph != "") {
      # If the athlete has pitch_speed_mph, update choices using the pitch_speed_mph_group column.
      updateSelectizeInput(
        session, "speed_group",
        choices = sort(unique(data$pitch_speed_mph_group)),
        selected = athlete_row$pitch_speed_mph_group,
        server = TRUE
      )
    } else {
      # Otherwise, update choices using the bat_speed_mph_group column.
      updateSelectizeInput(
        session, "speed_group",
        choices = sort(unique(data$bat_speed_mph_group)),
        selected = athlete_row$bat_speed_mph_group,
        server = TRUE
      )
    }
  })
  
  # Reactive expression for data filtered by the chosen playing level.
  selected_level_data <- reactive({
    req(input$playing_level)
    data %>% filter(playing_level == input$playing_level)
  })
  
  # Reactive expression for data filtered by the chosen speed group.
  selected_speedgroup_data <- reactive({
    req(input$speed_group)
    athlete_row <- selected_data()[1, ]
    if (!is.na(athlete_row$pitch_speed_mph) && athlete_row$pitch_speed_mph != "") {
      data %>% filter(pitch_speed_mph_group == input$speed_group)
    } else {
      data %>% filter(bat_speed_mph_group == input$speed_group)
    }
  })
  
  # Render the radar chart.
  output$radarPlot <- renderPlot({
    req(selected_data(), selected_level_data(), selected_speedgroup_data())
    
    # Get the selected athlete's row.
    athlete_values <- selected_data()
    
    # Use the KPI columns defined above.
    new_kpi_cols <- kpi_cols
    norm_cols <- paste0("norm_", new_kpi_cols)
    
    # Get the athlete's normalized KPI values.
    athlete_norms <- athlete_values %>% select(all_of(norm_cols))
    
    # Compute the average normalized KPI values for the chosen playing level.
    level_avg <- colMeans(selected_level_data()[, norm_cols], na.rm = TRUE)
    
    # Compute the average normalized KPI values for the chosen speed group.
    speed_group_avg <- colMeans(selected_speedgroup_data()[, norm_cols], na.rm = TRUE)
    
    # Define fixed maximum and minimum values for the radar chart axes (0 to 100).
    max_vals <- rep(100, length(norm_cols))
    min_vals <- rep(0, length(norm_cols))
    
    # Construct the radar chart data:
    #   Row 1: Maximum values (100) for each KPI.
    #   Row 2: Minimum values (0) for each KPI.
    #   Row 3: Selected athlete's normalized KPI values.
    #   Row 4: Average values for the chosen playing level.
    #   Row 5: Average values for the chosen speed group.
    radar_data <- rbind(max_vals, min_vals, 
                        as.numeric(athlete_norms[1,]),
                        level_avg,
                        speed_group_avg)
    rownames(radar_data) <- c("Max", "Min",
                              athlete_values$display_name,
                              paste("Level Avg:", input$playing_level),
                              paste("Speed Group Avg:", input$speed_group))
    
    # Convert the radar data to a data frame (required by radarchart).
    radar_data <- as.data.frame(radar_data)
    
    op <- par(mar = c(1, 1, 1, 1))
    fmsb::radarchart(
      radar_data,
      axistype = 1,
      pcol = c("blue", "red", "green"),
      pfcol = c(alpha("blue", 0.5), alpha("red", 0.5), alpha("green", 0.5)),
      plwd = 2,
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      # Fixed grid labels at 25, 50, 75, and 100.
      caxislabels = c("25", "50", "75", "100"),
      cglwd = 0.8,
      vlcex = 0.8,
      title = paste("KPI Radar Chart for", athlete_values$display_name,
                    "\nPlaying Level:", input$playing_level,
                    " | Speed Group:", input$speed_group)
    )
    
    legend("bottomright",
           legend = c(athlete_values$display_name,
                      paste("Level Avg:", input$playing_level),
                      paste("Speed Group Avg:", input$speed_group)),
           bty = "n",
           pch = 20,
           col = c("blue", "red", "green"),
           text.col = "black",
           cex = 1, pt.cex = 1.5)
    par(op)
  })
}

# ---------- Run the Shiny App ----------
shinyApp(ui = ui, server = server)
