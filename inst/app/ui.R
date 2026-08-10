library(shiny)
library(bslib)

# theme
theme_choices <- c(
  "Darkly (default)" = "darkly",
  "Cyborg" = "cyborg",
  "Superhero" = "superhero",
  "Slate" = "slate",
  "Flatly" = "flatly",
  "Zephyr" = "zephyr",
  "Cosmo" = "cosmo",
  "Minty" = "minty"
)

ui <- fluidPage(
  theme = bs_theme(version = 5, preset = "darkly"),
  titlePanel("Package Systematic Review"),
  sidebarLayout(
    # side bar
    sidebarPanel(
      textInput("query", "Search Query", value = ""),
      numericInput("n", "Number of packages", value = 20, min = 1, max = NA),
      actionButton("search", "Search"),
      hr(),
      dateRangeInput(
        "date_filter",
        "Last release between",
        start = NULL,
        end = NULL
      ),
      hr(),
      h4("Progress"),
      verbatimTextOutput("summary"),
      actionButton("save_sheet", "Save to Sheet", class = "btn-primary btn-sm"),
      downloadButton("download", "Download CSV")
    ),

    # main panel
    mainPanel(
      uiOutput("pkg_card"),
      uiOutput("controls")
    )
  ),
  # Theme picker
  tags$div(
    style = paste(
      "position: fixed; bottom: 16px; right: 16px; z-index: 1050;",
      "background: var(--bs-body-bg); padding: 6px 10px;",
      "border-radius: 6px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);"
    ),
    selectInput(
      "app_theme",
      "Theme",
      choices = theme_choices,
      selected = "darkly",
      width = "160px"
    )
  ),
  includeCSS("styles.css")
)
