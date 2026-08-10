library(shiny)

ui <- fluidPage(
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
  includeCSS("styles.css")
)
