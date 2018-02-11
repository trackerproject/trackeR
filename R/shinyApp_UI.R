#' Insert map
create_map <- function() {
  insertUI(
    selector = ".content",
    where = "beforeEnd",
    ui = conditionalPanel(
      condition = "output.cond == true",
      div(class = "main_plots", fluidRow(shinydashboard::box(
        status = "primary",
        width = 12,
        collapsible = TRUE,
        title = tagList(icon("map"), "Map"),
        shinycssloaders::withSpinner(leaflet::leafletOutput("map", width = "auto", height = "430px"), size = 2)
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
    div(class = "main_plots", fluidRow(
      shinydashboard::valueBoxOutput("avgDistance_box", width = 3),
      shinydashboard::valueBoxOutput("avgDuration_box", width = 3),
      shinydashboard::valueBoxOutput("avgPace_box", width = 3),
      shinydashboard::valueBoxOutput("avgHeartRate_box", width = 3)
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
      div(class = "main_plots", id = paste0("box", i), fluidRow(
        shinydashboard::box(
          status = "primary",
          width = 12,
          collapsible = TRUE,
          # height = "250px",
          title = tagList(shiny::icon(create_icon(i)), name),
          plotly::plotlyOutput(paste0(i, '_plot'), width = "auto", height = "180px")
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
            plotly::plotlyOutput(paste0(id, "_plot"), width = if (length(data$selectedSessions) > 2) {
              paste0(toString(500 * length(as.vector(data$selectedSessions))), "px")
            } else {
              "auto"
            } , height = "250px")
          )
        )
      ))
    )
  )
}


#' Create concentration profile plot and time in zones plot UI.
create_box <- function(title, inputId, label, plotId, choices) {
 ## Other metrics - Work capacity, Distribution profile, Concentration profile
  insertUI(
    selector = ".content",
    where = "beforeEnd",
    ui = conditionalPanel(
      condition = "output.cond == false",
      div(
        class = "plots",
        fluidRow(shinydashboard::box(
          status = "primary",
          width = 12,
          collapsible = TRUE,
          title = tagList(shiny::icon("gear"), title),
          selectizeInput(
            inputId = inputId,
            label = paste0(label, ':'),
            multiple = TRUE,
            choices = choices,
            selected = "speed"
          ),
          uiOutput(plotId)
        ))
      )
    )
  )
}

#' Create a return button from selected workouts plot
create_option_button <- function() {
  insertUI(
           selector = ".content",
           where = "afterBegin",
           ui = fluidRow(shinydashboard::box(
                                            status = "primary",
                                            width = "12",
                                            collapsible = TRUE,
                                            title = tagList('Options'),
                                            conditionalPanel(
                                                condition = "output.cond == false",
                                                 actionButton(
                                                  "return_to_main_page", "Go back",
                                                  style = "color: #fff; background-color: #4FBF85; border-color: #00AB66"
                                                  )),
                                            conditionalPanel(
                                                condition = "output.cond == true",
                                                 actionButton(
                                                  "plotSelectedWorkouts", "Plot Selected workouts",
                                                  style = "color: #fff; background-color: #4FBF85; border-color: #00AB66"
                                                  ))

                                 )))

}


