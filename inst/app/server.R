library(shiny)
library(pkgsearch)
library(stringr)

link <- function(x) {
  urls <- str_split_1(x, ",\\s*|\\s+")
  tagList(lapply(urls, function(u) {
    tags$span(a(href = u, target = "_blank", u), " ")
  }))
}

highlight <- function(text, query) {
  txt <- htmltools::htmlEscape(text)
  pattern <- paste0("(", str_replace_all(query, " ", "|"), ")")
  highlight_txt <- str_replace_all(
    txt,
    regex(pattern, ignore_case = TRUE),
    "<mark>\\1</mark>"
  )
  HTML(highlight_txt)
}

server <- function(input, output, session) {
  # set up reactive values
  results <- reactiveVal(NULL)
  idx <- reactiveVal(1)
  decisions <- reactiveVal(list())
  active_query <- reactiveVal("")

  # search (pkgsearch)
  observeEvent(input$search, {
    req(nzchar(input$query))
    results(pkg_search(input$query, size = input$n))
    active_query(input$query)
    idx(1)
  })

  current <- reactive({
    res <- results()
    req(res, idx() >= 1, idx() <= nrow(res))
    res[idx(), ]
  })

  # package card
  output$pkg_card <- renderUI({
    res <- results()
    if (is.null(res)) {
      return(p("Search for packages to begin reviewing process."))
    }
    pk <- current()
    d <- decisions()[[pk$package]]
    status <- if (is.null(d)) {
      span("Undecided", style = "color: grey;")
    } else if (d$decision == "include") {
      span("INCLUDED", style = "color: green; font-weight: bold;")
    } else {
      span(
        paste0("EXCLUDED - ", d$reason),
        style = "color: red; font-weight: bold;"
      )
    }
    rel_date <- as.Date(pk$date)
    days_old <- as.integer(Sys.Date() - rel_date)
    # information panel
    wellPanel(
      h3(
        sprintf(
          "%s (%d of %d)",
          pk$package,
          idx(),
          nrow(res)
        )
      ),
      h4(
        "Status: ",
        status
      ),
      p(strong("Title: "), pk$title),
      p(
        strong("Description: "),
        highlight(pk$description, active_query())
      ),
      p(strong("Version: "), as.character(pk$version)),
      p(
        strong("Last release: "),
        sprintf("%s (%d days ago)", rel_date, days_old)
      ),
      p(strong("License: "), pk$license),
      p(strong("Maintainer: "), pk$maintainer_name),
      p(strong("Downloads (last month): "), pk$downloads_last_month),
      p(strong("Reverse dependencies: "), pk$revdeps),
      p(
        strong("URL: "),
        if (is.null(pk$url)) "-" else link(pk$url)
      ),
      p(
        strong("Bug reports: "),
        if (is.null(pk$bugreports)) "-" else link(pk$url)
      )
    )
  })

  # navigation + decision controls
  output$controls <- renderUI({
    req(results())
    prev_reasons <- unique(unlist(lapply(decisions(), \(x) {
      if (x$decision == "exclude" && nzchar(x$reason)) x$reason
    })))
    tagList(
      fluidRow(
        column(6, actionButton("prev_btn", "< Previous")),
        column(6, actionButton("next_btn", "Next >"))
      ),
      hr(),
      selectizeInput(
        "reason",
        "Exclusion reason (optional)",
        choices = c("", prev_reasons),
        selected = "",
        options = list(
          create = TRUE,
          placeholder = "Pick a previous reason or type a new one"
        )
      ),
      actionButton("include", "Include", class = "btn-success"),
      actionButton("exclude", "Exclude", class = "btn-danger")
    )
  })

  observeEvent(input$prev_btn, if (idx() > 1) idx(idx() - 1))
  observeEvent(input$next_btn, if (idx() < nrow(results())) idx(idx() + 1))

  record <- function(decision) {
    pk <- current()
    d <- decisions()
    d[[pk$package]] <- list(
      decision = decision,
      reason = if (decision == "exclude") input$reason else "",
      title = pk$title,
      version = as.character(pk$version),
      date = as.character(as.Date(pk$date))
    )
    decisions(d)
    updateTextInput(session, "reason", value = "")
    if (idx() < nrow(results())) idx(idx() + 1)
  }

  observeEvent(input$include, record("include"))
  observeEvent(input$exclude, record("exclude"))

  # summary
  output$summary <- renderText({
    d <- decisions()
    inc <- names(d)[vapply(d, function(x) x$decision == "include", logical(1))]
    sprintf(
      "Reviewed: %d\nIncluded: %d\nExcluded: %d\n\n%s",
      length(d),
      length(inc),
      length(d) - length(inc),
      paste(inc, collapse = "\n")
    )
  })

  # download handler
  output$download <- downloadHandler(
    filename = "package_review.csv",
    content = function(file) {
      d <- decisions()
      if (length(d) == 0) {
        write.csv(data.frame(), file, row.names = FALSE)
        return(invisible())
      }
      df <- do.call(
        rbind,
        lapply(names(d), function(pkg) {
          data.frame(package = pkg, d[[pkg]], stringsAsFactors = FALSE)
        })
      )
      write.csv(df, file, row.names = FALSE)
    }
  )
}
