initial_review_app <- function(sheet = NULL) {
  if (!is.null(sheet) & !googlesheets4::gs4_has_token()) {
    googlesheets4::gs4_auth()

    app_sheet <<- sheet
  }

  if (is.null(sheet)) {
    app_sheet <<- ""
  }

  app_dir <- system.file("app", package = "pkgscreen")

  shiny::runApp(app_dir, display.mode = "normal")
}
