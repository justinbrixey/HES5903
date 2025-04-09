# Load required libraries
library(shiny)
library(dplyr)
library(fmsb)    # For the radar chart
library(scales)  # For transparency adjustments

# ---------- Global Section ----------
# Read in the data (assumes file is at the specified location)
data <- read.csv("Data/strengthMetrics.csv", stringsAsFactors = FALSE)

# Create an AthleteID column so that Athlete 1 corresponds to row 1, etc.
data$AthleteID <- 1:nrow(data)

# Create a combined "speed" metric:
# Use pitch_speed_mph if available (non-missing and non-empty), otherwise use bat_speed_mph.
data$speed <- ifelse(!is.na(data$pitch_speed_mph) & data$pitch_speed_mph != "",
                     data$pitch_speed_mph, data$bat_speed_mph)

# Define the important KPI columns for the radar chart.
# Here, we use the combined "speed" metric along with the others.
kpi_cols <- c("speed",
              "peak_power_.w._mean_cmj",
              "peak_power_.w._mean_sj",
              "net_peak_vertical_force_.n._max_imtp",
              "best_rsi_.jump_height.contact_time._.m.s._mean_ht",
              "body_weight_.lbs.")

# Normalize each KPI to a 0-100 scale using percentile rank.
# A score of 75 means the athlete is at the 75th percentile for that KPI.
data <- data %>%
  mutate(across(all_of(kpi_cols), ~ percent_rank(.) * 100, .names = "norm_{.col}"))

# ---------- UI Section ----------
ui <- fluidPage(
  titlePanel("Athlete KPI Radar Chart"),
  sidebarLayout(
    sidebarPanel(
      # Select input for athlete selection
      selectizeInput("athlete", "Select Athlete:", choices = NULL),
      # Select input for playing level comparison
      selectizeInput("playing_level", "Select Playing Level:", choices = NULL),
      # NEW: Select input for speed group comparison (will update based on athlete's speed type)
      selectizeInput("speed_group", "Select Speed Group Comparison:", choices = NULL)
    ),
    mainPanel(
      plotOutput("radarPlot")
    )
  )
)

# ---------- Server Section ----------
server <- function(input, output, session) {
  
  # Update the athlete selectize input with choices from the data.
  updateSelectizeInput(
    session, "athlete",
    choices = setNames(data$AthleteID, paste("Athlete", data$AthleteID)),
    server = TRUE
  )
  
  # Update the playing level selectize input with the unique playing levels.
  updateSelectizeInput(
    session, "playing_level",
    choices = sort(unique(data$playing_level)),
    server = TRUE
  )
  
  # Reactive expression for the selected athlete's data.
  selected_data <- reactive({
    req(input$athlete)
    data %>% filter(AthleteID == as.numeric(input$athlete))
  })
  
  # Update the speed group selectize input based on the athlete's speed type.
  observe({
    req(selected_data())
    # Determine whether to use pitch or bat speed groups
    athlete_row <- selected_data()[1, ]
    if (!is.na(athlete_row$pitch_speed_mph) && athlete_row$pitch_speed_mph != "") {
      # Using pitch speed: update speed_group choices with pitch_speed_mph_group values.
      updateSelectizeInput(
        session, "speed_group",
        choices = sort(unique(data$pitch_speed_mph_group)),
        selected = athlete_row$pitch_speed_mph_group,
        server = TRUE
      )
    } else {
      # Otherwise, use bat speed: update speed_group choices with bat_speed_mph_group values.
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
    # Determine which grouping column to use based on the athlete's speed type.
    athlete_row <- selected_data()[1, ]
    if (!is.na(athlete_row$pitch_speed_mph) && athlete_row$pitch_speed_mph != "") {
      # Use pitch speed group.
      data %>% filter(pitch_speed_mph_group == input$speed_group)
    } else {
      # Use bat speed group.
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
    # Extract the normalized KPI columns (e.g., "norm_speed", "norm_peak_power_.w._mean_cmj", etc.)
    norm_cols <- paste0("norm_", new_kpi_cols)
    
    # Get the athlete's normalized KPI values.
    athlete_norms <- athlete_values %>% select(all_of(norm_cols))
    
    # Compute the average normalized KPI values for the selected playing level.
    level_avg <- colMeans(selected_level_data()[, norm_cols], na.rm = TRUE)
    
    # Compute the average normalized KPI values for the selected speed group.
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
                              paste("Athlete", athlete_values$AthleteID),
                              paste("Level Avg:", input$playing_level),
                              paste("Speed Group Avg:", input$speed_group))
    
    # Convert to a data frame as required by radarchart().
    radar_data <- as.data.frame(radar_data)
    
    # Draw the radar chart with three polygons:
    #   - Athlete in blue,
    #   - Playing level average in red,
    #   - Speed group average in green.
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
      # Fixed concentric grid labels: 25, 50, 75, 100.
      caxislabels = c("25", "50", "75", "100"),
      cglwd = 0.8,
      vlcex = 0.8,
      title = paste("KPI Radar Chart for Athlete", athlete_values$AthleteID,
                    "\nPlaying Level:", input$playing_level,
                    " | Speed Group:", input$speed_group)
    )
    
    # Add a legend to distinguish the polygons.
    legend("bottomright",
           legend = c(paste("Athlete", athlete_values$AthleteID),
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
