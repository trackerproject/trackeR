#' Insert map
create_map <- function() {
  insertUI(
    selector = ".content",
    where = "beforeEnd",
    ui = conditionalPanel(
      condition = "output.cond == true",
      div(class = "plots", fluidRow(shinydashboard::box(
        status = "primary",
        width = 12,
        collapsible = TRUE,
        title = tagList(icon("map"), "Map"),
        shinycssloaders::withSpinner(leaflet::leafletOutput("map", width = "auto", height = "430px"), size = 2),
        absolutePanel(
          top = 70, right = 60,
          actionButton(
            "plotSelectedWorkouts", "Plot selected workouts",
            style = "color: #fff; background-color: #4FBF85; border-color: #00AB66"
          )
        )
      )))
    )
  )
}

#' Insert summary boxes
create_summary_boxes <- function() {
  insertUI(
  selector = ".content",
  where = "beforeEnd",
  ui = conditionalPanel(
    condition = "output.cond == true",
    div(class = "plots", fluidRow(
      shinydashboard::valueBoxOutput("averageDistance", width = 3),
      shinydashboard::valueBoxOutput("averageDuration", width = 3),
      shinydashboard::valueBoxOutput("averagePace", width = 3),
      shinydashboard::valueBoxOutput("averageHeartRate", width = 3)
    ))
  )
  )
}

#' Create workout plots
create_workout_plots <- function(i) {
  name <- switch(as.character(i),
      "distance" = "Distance",
      "duration" = "Duration",
      "avgSpeed" = "Average Speed",
      "avgPace" = "Average Pace",
      "avgCadence" = "Average Cadence",
      "avgPower" = "Average Power",
      "avgHeartRate" = "Average Heart Rate",
      "wrRatio" = "Work-to-rest Ratio"
    )

  insertUI(
    selector = ".content",
    where = "beforeEnd",
    ui = conditionalPanel(
      condition = "output.cond == true",
      div(class = "plots", id = paste0("box", i), fluidRow(
        shinydashboard::box(
          status = "primary",
          width = 12,
          collapsible = TRUE,
          # height = "250px",
          title = tagList(shiny::icon(create_icon(i)), name),
          plotly::plotlyOutput(i, width = "auto", height = "180px")
        )
      ))
    )
  )
}

#' Create selected_workouts plot
create_selected_workout_plot <- function(id, data) {
  insertUI(
    selector = ".content",
    where = "beforeEnd",
    ui = conditionalPanel(
      condition = "output.cond == false",
      div(class = "plots", id = id, fluidRow(
        shinydashboard::box(
          status = "primary",
          width = 12,
          # height = "350px",
          collapsible = TRUE,
          title = tagList(
            shiny::icon("gear"),
            switch(id, "pace" = paste0("Pace"),
              "heart.rate" = paste0("Heart Rate"),
              "altitude" = paste0("Altitude"),
              "work_capacity" = paste0("Work Capacity")
            )
          ),
          div(
            style = "overflow-x: scroll",
            plotly::plotlyOutput(paste0("plot_", id), width = if (length(data$selected_sessions) > 2) {
              paste0(toString(750 * length(as.vector(data$selected_sessions))), "px")
            } else {
              "auto"
            } , height = "250px")
          )
        )
      ))
    )
  )
}

#' Create time in zones plot
create_time_in_zones_plot <- function() {
  ## Time in Zones
  insertUI(
    selector = ".content",
    where = "beforeEnd",
    ui = conditionalPanel(
      condition = "output.cond == false",
      div(
        class = "plots", id = "zones",
        fluidRow(shinydashboard::box(
          status = "primary",
          width = 12,
          collapsible = TRUE,
          title = tagList(shiny::icon("gear"), "Time in Zones"),
          selectizeInput(
            inputId = "zones_for_plot",
            label = "Select zone metrics to plot:",
            multiple = TRUE,
            choices = c(
              "Altitude" = "altitude",
              "Speed" = "speed",
              "Pace" = "pace"
            ),
            selected = c("speed")
          ),
          uiOutput("time_in_zones")
        ))
      )
    )
  )
}

#' Create concentration profile plot
create_concentration_profile_plot <- function() {
  ## Other metrics - Work capacity, Distribution profile, Concentration profile
  insertUI(
    selector = ".content",
    where = "beforeEnd",
    ui = conditionalPanel(
      condition = "output.cond == false",
      div(
        class = "plots", id = "concentration_profiles",
        fluidRow(shinydashboard::box(
          status = "primary",
          width = 12,
          collapsible = TRUE,
          title = tagList(shiny::icon("gear"), "Profiles"),
          selectizeInput(
            inputId = "concentration_profile_metrics_for_plot",
            label = "Concentration profiles:",
            multiple = TRUE,
            choices = c(
              "Altitude" = "altitude",
              "Speed" = "speed",
              "Pace" = "pace"
            ),
            selected = "speed"
          ),
          uiOutput("concentration_profiles")
        ))
      )
    )
  )
}


