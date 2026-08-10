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

  terms <- query |>
    str_remove_all(regex("\\b(and|or|not)\\b", ignore_case = TRUE)) |>
    str_remove_all("[()]") |>
    str_split_1("\\s+")
  terms <- terms[nzchar(terms)]

  if (length(terms) == 0) {
    return(HTML(txt))
  }

  escaped_terms <- str_replace_all(
    terms,
    "([\\\\^$.|?*+()\\[\\]{}])",
    "\\\\\\1"
  )
  pattern <- paste0("(", paste(escaped_terms, collapse = "|"), ")")
  highlight_txt <- str_replace_all(
    txt,
    regex(pattern, ignore_case = TRUE),
    "<mark>\\1</mark>"
  )
  HTML(highlight_txt)
}

show_val <- function(x) if (is.null(x) || is.na(x)) "-" else x

na_if_null <- function(x) if (is.null(x)) NA else x

decisions_to_df <- function(d) {
  field <- function(name) {
    vapply(
      d,
      function(x) {
        val <- x[[name]]
        if (is.null(val) || length(val) == 0) {
          NA_character_
        } else {
          as.character(val)
        }
      },
      character(1)
    )
  }
  data.frame(
    package = names(d),
    decision = field("decision"),
    reason = field("reason"),
    title = field("title"),
    version = field("version"),
    date = field("date"),
    stringsAsFactors = FALSE
  )
}

server <- function(input, output, session) {
  # theme picker
  observeEvent(input$app_theme, {
    session$setCurrentTheme(
      bslib::bs_theme(version = 5, preset = input$app_theme)
    )
  })

  # set up reactive values
  results <- reactiveVal(NULL)
  idx <- reactiveVal(1)
  decisions <- reactiveVal(list())
  active_query <- reactiveVal("")

  # loading the data
  if (nzchar(app_sheet)) {
    df <- googlesheets4::read_sheet(app_ss, sheet = app_sheet)
    if (nrow(df) > 0) {
      decisions(setNames(
        lapply(seq_len(nrow(df)), function(i) {
          list(
            decision = na_if_null(df$decision[i]),
            reason = na_if_null(df$reason[i]),
            title = na_if_null(df$title[i]),
            version = na_if_null(df$version[i]),
            date = na_if_null(df$date[i])
          )
        }),
        df$package
      ))
    }
  }

  # search (pkgsearch)
  observeEvent(input$search, {
    req(nzchar(input$query))
    res <- advanced_search(input$query, size = input$n)[]
    results(res)
    active_query(input$query)
    rel_dates <- as.Date(res$date)
    updateDateRangeInput(
      session,
      "date_filter",
      start = min(rel_dates, na.rm = TRUE),
      end = max(rel_dates, na.rm = TRUE),
      min = min(rel_dates, na.rm = TRUE),
      max = max(rel_dates, na.rm = TRUE)
    )
    idx(1)
  })

  filtered <- reactive({
    res <- results()
    if (is.null(res)) {
      return(NULL)
    }
    keep <- rep(TRUE, nrow(res))
    if (!is.null(input$date_filter) && !anyNA(input$date_filter)) {
      rel_dates <- as.Date(res$date)
      keep <- !is.na(rel_dates) &
        rel_dates >= input$date_filter[1] &
        rel_dates <= input$date_filter[2]
    }
    out <- res[keep, , drop = FALSE]
    out
  })

  observeEvent(input$date_filter, idx(1))

  current <- reactive({
    res <- filtered()
    req(res, idx() >= 1, idx() <= nrow(res))
    res[idx(), ]
  })

  # package card
  output$pkg_card <- renderUI({
    if (is.null(results())) {
      return(p("Search for packages to begin reviewing process."))
    }
    res <- filtered()
    if (nrow(res) == 0) {
      return(p("No packages match the current date filter."))
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
      p(strong("Title: "), show_val(pk$title)),
      p(
        strong("Description: "),
        if (is.na(pk$description)) {
          "-"
        } else {
          highlight(pk$description, active_query())
        }
      ),
      p(strong("Version: "), as.character(pk$version)),
      p(
        strong("Last release: "),
        sprintf("%s (%d days ago)", rel_date, days_old)
      ),
      p(strong("License: "), show_val(pk$license)),
      p(strong("Maintainer: "), show_val(pk$maintainer_name)),
      p(strong("Downloads (last month): "), show_val(pk$downloads_last_month)),
      p(strong("Reverse dependencies: "), show_val(pk$revdeps)),
      p(
        strong("URL: "),
        if (is.null(pk$url) || is.na(pk$url)) "-" else link(pk$url)
      ),
      p(
        strong("Bug reports: "),
        if (is.null(pk$bugreports) || is.na(pk$bugreports)) {
          "-"
        } else {
          link(pk$bugreports)
        }
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
        column(4, actionButton("prev_btn", "< Previous")),
        column(4, actionButton("next_btn", "Next >")),
        column(
          4,
          actionButton("first_unreviewed_btn", "First unreviewed")
        )
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
  observeEvent(input$next_btn, if (idx() < nrow(filtered())) idx(idx() + 1))

  observeEvent(input$first_unreviewed_btn, {
    res <- filtered()
    req(res)
    reviewed <- names(decisions())
    unreviewed <- which(!res$package %in% reviewed)
    if (length(unreviewed) > 0) {
      idx(unreviewed[1])
    } else {
      showNotification("All packages have been reviewed.", type = "message")
    }
  })

  record <- function(decision) {
    pk <- current()
    d <- decisions()
    d[[pk$package]] <- list(
      decision = decision,
      reason = if (decision == "exclude") na_if_null(input$reason) else "",
      title = na_if_null(pk$title),
      version = na_if_null(as.character(pk$version)),
      date = na_if_null(as.character(as.Date(pk$date)))
    )
    decisions(d)
    updateTextInput(session, "reason", value = "")
    if (idx() < nrow(filtered())) idx(idx() + 1)
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

  current_decisions <- reactive({
    d <- decisions()
    res <- results()
    if (is.null(res)) {
      return(list())
    }
    d[names(d) %in% res$package]
  })

  # save to google sheet
  observeEvent(input$save_sheet, {
    d <- current_decisions()
    req(length(d) > 0)
    df <- decisions_to_df(d)
    googlesheets4::sheet_write(
      df,
      ss = app_ss,
      sheet = paste0("new-", app_sheet)
    )
  })

  # download handler
  output$download <- downloadHandler(
    filename = "package_review.csv",
    content = function(file) {
      d <- current_decisions()
      if (length(d) == 0) {
        write.csv(data.frame(), file, row.names = FALSE)
        return(invisible())
      }
      write.csv(decisions_to_df(d), file, row.names = FALSE)
    }
  )
}
