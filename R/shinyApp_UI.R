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
        collapsed = TRUE,
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
create_selected_workout_plot <- function(id) {
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
              "work_capacity" = paste0("Work Capacity"),
              "speed" = paste0("Speed")
            )
          ),
          div(
            style = "overflow-x: scroll",
            uiOutput(paste0(id, "_plot"))
          )
        )
      ))
    )
  )
}


#' Create concentration profile plot UI.
create_profiles_box <- function(title, inputId, label, plotId, choices) {
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
          fluidRow(
          column(3,
          selectizeInput(
            inputId = inputId,
            label = paste0(label, ':'),
            multiple = TRUE,
            choices = choices,
            selected = "speed"
          ))),
          uiOutput(plotId)
        ))
      )
    )
  )
}

#' Create time in zones plot UI.
create_zones_box <- function(title, inputId, label, plotId, choices) {
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
          fluidRow(
          column(3, selectizeInput(
            inputId = inputId,
            label = paste0(label, ':'),
            multiple = TRUE,
            choices = choices,
            selected = "speed"
          )),
          column(3, selectizeInput(
                         inputId = 'n_zones',
                         label = 'Select number of zones:',
                         multiple = FALSE,
                         choices = c(
                                    '2' = 2,
                                    '3' = 3,
                                    '4' = 4,
                                    '5' = 5,
                                    '6' = 6,
                                    '7' = 7,
                                    '8' = 8,
                                    '9' = 9
                                    ),
                         selected = '6'
                         ))),
          uiOutput(plotId)
        ))
      )
    )
  )
}

#' Create a return button from selected workouts plot
create_option_box <- function() {
  insertUI(
           selector = ".content",
           where = "afterBegin",
           ui = div(class = "main_plots", fluidRow(shinydashboard::box(
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

                                 ))))

}

#' Create a summary and timeline boxes
#'
create_summary_timeline_boxes <- function() {
  insertUI(
           selector=".content",
           where="beforeEnd",
           ui=div(class = "main_plots", fluidRow(
                        shinydashboard::box(
                          id = 'summary_box',
                          status = "primary",
                          width = 6,
                          title = tagList(shiny::icon("reorder"), "Summary of selected workouts"),
                          DT::dataTableOutput("summary", height = "auto"),
                          collapsible = FALSE
                        ),
                        shinydashboard::box(
                          id = "workout_timeline_box",
                          status = "primary",
                          width = 6,
                          collapsible = TRUE,
                          collapsed = FALSE,
                          title = tagList(shiny::icon("calendar", lib = "glyphicon"), "Workout Timeline"),
                          plotly::plotlyOutput("timeline_plot", height = "365px")
                        )
                       ))
           )
}

#' Generate a modal window where user can chage units of measurement.
#'
#' @param data An object of class \code{reactivevalues}.
show_change_unit_window <- function(data) {
  showModal(modalDialog(
    title = "Change units",
    radioButtons(
      "altitudeUnits", "Altitude:",
      c(
        "m" = "m",
        "km" = "km",
        "mi" = "mi",
        "ft" = "ft"
      ), inline = TRUE,
      selected = get_selected_units('altitude', data)
    ),
    radioButtons(
      "distanceUnits", "Distance:",
      c(
        "m" = "m",
        "km" = "km",
        "mi" = "mi",
        "ft" = "ft"
      ), inline = TRUE,
      selected = get_selected_units('distance', data)
    ),
    radioButtons(
      "speedUnits", "Speed:",
      c(
        "m/s" = "m_per_s",
        "km/h" = "km_per_h",
        "ft/min" = "ft_per_min",
        "ft/s" = "ft_per_s",
        "mi/h" = "mi_per_h"
      ), inline = TRUE,
      selected = get_selected_units('speed', data)
    ),
    radioButtons(
      "cadenceUnits", "Cadence:",
      c(
        "steps/min" = "steps_per_min",
        "revolutions/min" = "rev_per_min"
      ), inline = TRUE,
      selected = get_selected_units('cadence', data)
    ),
    radioButtons(
      "powerUnits", "Power:",
      c(
        "W" = "W",
        "kW" = "kW"
      ), inline = TRUE,
      selected = get_selected_units('power', data)
    ),
    radioButtons(
      "paceUnits", "Pace:",
      c(
        "min/km" = "min_per_km",
        "min/mi" = "min_per_mi",
        "s/min" = "s_per_m"
      ), inline = TRUE,
      selected = get_selected_units('pace', data)
    ),
    radioButtons(
      "durationUnits", "Duration:",
      c(
        "seconds" = "s",
        "minutes" = "min",
        "hours" = "h"
      ), inline = TRUE,
      selected = get_selected_units('duration', data)
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("updateUnits", "Apply")
    )
  )
  )
}

