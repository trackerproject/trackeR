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
        shinycssloaders::withSpinner(plotly::plotlyOutput("map",
          width = "auto",
          height = "700px"
        ),
        size = 2
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
#' @param feature A character. The metric that is plotted, selected from \code{\link{choices}}.
create_workout_plots <- function(feature) {
  name <- switch(as.character(feature),
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
      condition = paste0("output.", feature, " == false"),
      div(class = "main_plots", id = paste0("box", feature), fluidRow(
        shinydashboard::box(
          status = "primary",
          width = 12,
          collapsible = TRUE,
          # height = "250px",
          title = tagList(shiny::icon(create_icon(feature)), name),
          plotly::plotlyOutput(paste0(feature, "_plot"),
            width = "auto",
            height = "180px"
          )
        )
      ))
    )
  )
}

#' Create selected_workouts plot
#' @param id A character. The ID of the plot.
#' @param collapsed A logical. Whether or not the UI box should be collapsed.
create_selected_workout_plot <- function(id, collapsed = FALSE) {
  insertUI(
    selector = ".content",
    where = "beforeEnd",
    ui = conditionalPanel(
      condition = paste0("output.", id, " == false"),
      div(class = "plots", id = id, fluidRow(
        shinydashboard::box(
          status = "primary",
          width = 12,
          # height = "350px",
          collapsible = TRUE,
          collapsed = collapsed,
          title = tagList(
            shiny::icon("gear"),
            switch(id, "pace" = paste0("Pace"),
            "heart.rate" = paste0("Heart Rate"),
            "altitude" = paste0("Altitude"),
            "power" = paste0("Power"),
            "speed" = paste0("Speed"),
            "cadence" = paste0("Cadence")
            )
          ),
          shinyWidgets::dropdownButton(
            circle = TRUE, status = "info", up = TRUE,
            icon = icon("wrench"), width = "300px",

            tooltip = shinyWidgets::tooltipOptions(title = "Click to see inputs !"),
            div(
              class = "form-group shiny-input-container", id = "processed_path",
              tags$label("Press button to detect changepoints:"),
              div(class = "input-group", actionButton(paste0("detect_changepoints", id),
                label = "Detect changepoints", style = "color: #fff; background-color: #6FB1E7; border-color: #5093E3"
              ))
            )
            ,
            selectizeInput(
              inputId = paste0("n_changepoints", id),
              label = "Maximum # of changepoints:",
              multiple = FALSE,
              choices = c(
                "1" = 1,
                "2" = 2,
                "3" = 3,
                "4" = 4,
                "5" = 5,
                "6" = 6,
                "7" = 7,
                "8" = 8,
                "9" = 9,
                "10" = 10,
                "11" = 11,
                "12" = 12
              ),
              selected = "4"
            )
          ),
          hr(),
          div(
            style = "overflow-x: scroll",
            uiOutput(paste0(id, "_plot"))
          )
        )
      ))
    )
  )
}

#' Create work capacity plot
#' @param id A character. The ID of the plot.
#' @param collapsed A logical. Whether or not the UI box should be collapsed.
create_work_capacity_plot <- function(id, collapsed = FALSE) {
  insertUI(
    selector = ".content",
    where = "beforeEnd",
    ui = conditionalPanel(
      condition = "output.work_capacity == false",
      div(class = "plots", id = id, fluidRow(
        shinydashboard::box(
          status = "primary",
          width = 12,
          # height = "350px",
          collapsible = TRUE,
          collapsed = collapsed,
          title = tagList(
            shiny::icon("gear"),
            switch(id, "pace" = paste0("Pace"),
            "heart.rate" = paste0("Heart Rate"),
            "altitude" = paste0("Altitude"),
            "work_capacity" = paste0("Work Capacity"),
            "speed" = paste0("Speed")
            )
          ),
          conditionalPanel(
            condition = "output.work_capacity_cycling == false",
            fluidRow(
              column(
                2,
                # Wrap the button in the function `withBusyIndicatorUI()`
                div(
                  tags$label("Press button to update:"),
                  div(withBusyIndicatorUI(actionButton(
                    "cycling_update_power",
                    "Update critical power",
                    class = "btn-primary",
                    style = "color: #fff; background-color: #6FB1E7; border-color: #5093E3"
                  )))
                )
              ),
              column(
                2,
                numericInput(
                  min = 2, max = 10, step = 0.1,
                  inputId = "critical_power_cycling",
                  label = "Critical power [J]", value = 3
                )
              )
            ),
            div(
              style = "overflow-x: scroll",
              uiOutput(paste0("cycling_work_capacity", "_plot"))
            )
          ),
          conditionalPanel(
            condition = "output.work_capacity_running == false",
            fluidRow(
              column(
                2,
                # Wrap the button in the function `withBusyIndicatorUI()`
                div(
                  tags$label("Press button to update:"),
                  div(withBusyIndicatorUI(actionButton(
                    "running_update_power",
                    "Update critical speed",
                    class = "btn-primary",
                    style = "color: #fff; background-color: #6FB1E7; border-color: #5093E3"
                  )))
                )
              ),
              column(
                2,
                numericInput(
                  min = 0.01, max = 6.5, step = 0.1,
                  inputId = "critical_power_running",
                  label = "Critical speed [m/s]", value = 4
                )
              )
            ),
            div(
              style = "overflow-x: scroll",
              uiOutput(paste0("running_work_capacity", "_plot"))
            )
          )
        )
      ))
    )
  )
}


#' Create concentration profile plot UI.
#' @param inputId A character. The ID of the user input for the metrics that should be plotted
#' @param plotId A character. The ID of the plot.
#' @param choices A vector of the metrics a user can select to be plotted, selected from \code{\link{metrics}}.
create_profiles_box <- function(inputId, plotId, choices) {
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
          collapsed = FALSE,
          title = tagList(shiny::icon("gear"), "Concentration profiles"),
          fluidRow(
            column(
              2,
              selectizeInput(
                inputId = inputId,
                label = "Select profile metrics to plot:",
                multiple = TRUE,
                choices = choices,
                selected = "speed"
              )
            )
          ),
          uiOutput(plotId)
        ))
      )
    )
  )
}

#' Create time in zones plot UI.
#' @param inputId A character. The ID of the user input for the metrics that should be plotted.
#' @param plotId A character. The ID of the plot.
#' @param choices A vector of the metrics a user can select to be plotted, selected from \code{\link{metrics}}.
create_zones_box <- function(inputId, plotId, choices) {
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
          collapsed = FALSE,
          title = tagList(shiny::icon("gear"), "Time in Zones"),
          fluidRow(
            column(2, selectizeInput(
              inputId = inputId,
              label = "Select zone metrics to plot:",
              multiple = TRUE,
              choices = choices,
              selected = "speed"
            )),
            column(2, selectizeInput(
              inputId = "n_zones",
              label = "Select number of zones:",
              multiple = FALSE,
              choices = c(
                "2" = 2,
                "3" = 3,
                "4" = 4,
                "5" = 5,
                "6" = 6,
                "7" = 7,
                "8" = 8,
                "9" = 9
              ),
              selected = "6"
            ))
          ),
          uiOutput(plotId)
        ))
      )
    )
  )
}

#' Create a return button from selected workouts plot
#' @param sport_options A vector of sports identified from the uploaded sessions.
create_option_box <- function(sport_options) {
  insertUI(
    selector = ".content",
    where = "afterBegin",
    ui = div(class = "option_boxes", fluidRow(
      # shinydashboard::box(
      #   status = "primary",
      #   width = 3,
      #   collapsible = TRUE,
      #   title = tagList("Options"),
      column(
        12,
        fluidRow(
          column(2,
        conditionalPanel(
          condition = "output.cond == false",
          shinyWidgets::actionBttn(
            inputId = "return_to_main_page",
            label = "Go back",
            style = "pill",
            color = "success"
          )
        ),
        conditionalPanel(
          condition = "output.cond == true",
          shinyWidgets::actionBttn(
            inputId = "plotSelectedWorkouts",
            label = "Plot workouts",
            style = "pill",
            color = "success"
          )
        )), column(4, 
        shinyWidgets::actionBttn(
          inputId = "resetSelection",
          label = "Reset session selection",
          style = "unite",
          color = "danger"
        ))),
        style='padding-bottom:2%;')
    ), fluidRow(
      # ),
      shinydashboard::box(height = '112px',
        status = "primary",
        width = 2,
        collapsible = TRUE,
        title = tagList("Other tools"),
        shinyWidgets::actionBttn(inputId = "showModalUnits", label = "Change units", icon = icon("balance-scale"), style = 'unite', color = 'primary')
      ),
      shinydashboard::box(
        status = "primary",
        width = 4,
        collapsible = TRUE,
        title = tagList("Select variables to display"),
        shinyWidgets::pickerInput(
          inputId = "metricsSelected",
          # label = "Select metrics",
          choices = c(
            "Distance" = "distance",
            "Duration" = "duration",
            "Average speed" = "avgSpeed",
            "Average pace" = "avgPace",
            "Average cadence" = "avgCadence",
            "Average power" = "avgPower",
            "Average heart rate" = "avgHeartRate",
            "Work to rest ratio" = "wrRatio"
          ), options = list(`actions-box` = TRUE, `style` = "btn-info"),
          multiple = TRUE, selected = c("distance", "duration", "avgPace")
        )
      ),
      shinydashboard::box(
        status = "primary",
        width = 6,
        collapsible = TRUE,
        title = tagList("Classified sports"),
        shinyWidgets::checkboxGroupButtons(
          inputId = "sports", 
          # label = "Select from identified sports: ",
          choices = sport_options, selected = sport_options,
          justified = TRUE, status = "info",
          checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
        )
      )
    ))
  )
}


#' Create a summary and timeline boxes
#'
create_summary_timeline_boxes <- function() {
  insertUI(
    selector = ".content",
    where = "beforeEnd",
    ui = div(class = "main_plots", fluidRow(
      shinydashboard::box(
        id = "summary_box",
        status = "primary",
        width = 6,
        title = tagList(
          shiny::icon("reorder"),
          "Summary of selected workouts"
        ),
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
      ),
      inline = TRUE,
      selected = get_selected_units("altitude", data)
    ),
    radioButtons(
      "distanceUnits", "Distance:",
      c(
        "m" = "m",
        "km" = "km",
        "mi" = "mi",
        "ft" = "ft"
      ),
      inline = TRUE,
      selected = get_selected_units("distance", data)
    ),
    radioButtons(
      "speedUnits", "Speed:",
      c(
        "m/s" = "m_per_s",
        "km/h" = "km_per_h",
        "ft/min" = "ft_per_min",
        "ft/s" = "ft_per_s",
        "mi/h" = "mi_per_h"
      ),
      inline = TRUE,
      selected = get_selected_units("speed", data)
    ),
    radioButtons(
      "cadenceUnits", "Cadence:",
      c(
        "steps/min" = "steps_per_min",
        "revolutions/min" = "rev_per_min"
      ),
      inline = TRUE,
      selected = get_selected_units("cadence", data)
    ),
    radioButtons(
      "powerUnits", "Power:",
      c(
        "W" = "W",
        "kW" = "kW"
      ),
      inline = TRUE,
      selected = get_selected_units("power", data)
    ),
    radioButtons(
      "paceUnits", "Pace:",
      c(
        "min/km" = "min_per_km",
        "min/mi" = "min_per_mi",
        "s/min" = "s_per_m"
      ),
      inline = TRUE,
      selected = get_selected_units("pace", data)
    ),
    radioButtons(
      "durationUnits", "Duration:",
      c(
        "seconds" = "s",
        "minutes" = "min",
        "hours" = "h"
      ),
      inline = TRUE,
      selected = get_selected_units("duration", data)
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("updateUnits", "Apply")
    )
  ))
}
