initial_review_app <- function(ss = NULL, sheet = NULL) {
  if (!is.null(sheet) & !googlesheets4::gs4_has_token()) {
    googlesheets4::gs4_auth()

    app_ss <<- ss

    app_sheet <<- sheet
  }

  if (is.null(sheet)) {
    app_ss <<- ""

    app_sheet <<- ""
  }

  app_dir <- system.file("app", package = "pkgscreen")

  shiny::runApp(app_dir, display.mode = "normal")
}
