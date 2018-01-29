jscode <- "
    shinyjs.collapse = function(boxid) {
    $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
    };
    // shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_selected-A', 'null'); }
    shinyjs.reset_page = function() { location.reload(); }
"

ui <- shinydashboard::dashboardPage(
  title = "TrackeR",
  skin = "black",
  shinydashboard::dashboardHeader(title = span(tagList(icon("dashboard"), "trackeR dashboard"))),
  shinydashboard::dashboardSidebar(
    tags$head(tags$script("
                            function hideElement(i) {
                                var x = document.getElementById(i);
                                x.style.display = 'none';
                            }
                           ")),
    tags$head(tags$style(".warningMessage {
                          font-size: 20px;
                         }
                         hr {
                         border-top: 1px solid;
                         }
                          a#download_data {
                            color: #333;
                          }
                         ")),
    shinydashboard::sidebarMenu(
      ## menuItem('', tabName = 'dashboard', icon = icon('dashboard')),
      ## hr(),
      ## fileInput('processed_data_path', 'Load processed data'),
      div(
        class = "form-group shiny-input-container", id = "processed_path",
        tags$label("Processed data file"),
        div(class = "input-group", shinyFiles::shinyFilesButton("processed_data_path", "Select file...", "Select processed data file", multiple = FALSE))
      ),
      div(
        class = "form-group shiny-input-container", id = "raw_directory",
        tags$label("New raw data directory"),
        div(class = "input-group", shinyFiles::shinyDirButton("raw_data_directory", "Select directory...", "Select new raw data directory"))
      ),
      actionButton("uploadButton", "Load data", icon("upload"), style = "color: #fff; background-color: #6FB1E7; border-color: #5093E3"),
      hr(),
      div(
        selectInput(
          "sportSelected", "Select sport", multiple = FALSE,
          c(
            "Running" = "running",
            "Cycling" = "cycling"
          )
        ),
        selectizeInput(
          "metricsSelected", "Select metrics", multiple = TRUE,
          choices = c(
            "Distance" = "distance",
            "Duration" = "duration",
            "Average speed" = "avgSpeed",
            "Average pace" = "avgPace",
            "Average cadence" = "avgCadence",
            "Average power" = "avgPower",
            "Average heart rate" = "avgHeartRate",
            "Work to rest ratio" = "wrRatio"
          ),
          selected = c("duration", "distance", "avgPace")
        ),
        ## In color: qualitative, set 2 form choose_palette(gui = "shiny")
        ## Out color: qualitative, even darker form choose_palette(gui = "shiny")
        div(style = "display: inline-block;vertical-align:top; width: 100px;", actionButton("plotButton", "Plot", icon("area-chart"), style = "color: #fff; background-color: #4FBF85; border-color: #00AB66", width = "80px")),
        div(style = "display: inline-block;vertical-align:top; width: 120px;", actionButton("changeUnits", "Units", icon("balance-scale"), width = "80px"))
      ),
      hr(),
      div(
        class = "form-group shiny-input-container", tags$label("Download data"),
        div(class = "input-group", downloadButton("download_data", "Download procesed data"))
      ),
      hr(),
      div(style = "display: inline-block;vertical-align:top; width: 100px;", actionButton("resetButton", "Reset", icon("eraser"), style = "color: #fff; background-color: #ED90A4; border-color: #E16A86", width = "80px")),
      hr()
    ),
    div(
      class = "form-group shiny-input-container",
      p("Design and original development", br(), a("Robin Hornak"), br(), a("Ioannis Kosmidis", href = "http://www.ucl.ac.uk/~ucakiko")),
      p(
        "Licence",
        a(
          "GPL3", href = "https://www.gnu.org/licenses/gpl-3.0.en.html",
          br(),
          a("Bugs, issues, feature requests", href = "https://github.com/hfrick/trackeR/issues")
        )
      )
    )
  ),
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jscode),
    fluidRow(
      shinydashboard::box(
        status = "primary",
        width = 6,
        title = tagList(shiny::icon("reorder"), "Summary of selected workouts"),
        DT::dataTableOutput("summary", height = "auto"),
        collapsible = TRUE,
        collapsed = TRUE
      ),
      shinydashboard::box(
        status = "primary",
        width = 6,
        collapsible = TRUE,
        collapsed = TRUE,
        title = tagList(shiny::icon("calendar", lib = "glyphicon"), "Workout Timeline"),
        plotOutput("timeline_plot", height = "350px")
      )
    )
  )
)
