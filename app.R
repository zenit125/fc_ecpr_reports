# =============================================================================
# FCECPR Training Registration Form: report app
#
# A point-and-click front end for the same reports run_reports.R makes.
# Responses come straight from Qualtrics through the qualtRics package.
#
# Settings come from environment variables (on Posit Connect Cloud, set them
# as secret variables; locally, put them in a .Renviron file next to this one):
#   QUALTRICS_API_KEY    your Qualtrics API token
#   QUALTRICS_BASE_URL   your data-center URL, e.g. "yourorg.ca1.qualtrics.com"
#   QUALTRICS_SURVEY_ID  the survey ID, e.g. "SV_xxxxxxxxxxxxxxx"
#   APP_PASSWORD         password people type to open the app (strongly advised)
#   DEMO_DATA_DIR        optional: a folder of exported CSVs to use instead of
#                        the API (for trying the app without credentials)
# =============================================================================

library(shiny)
library(bslib)
library(DT)

source("R/functions.R", local = TRUE)
project_dir <- normalizePath(".")
survey_docx <- list.files("survey", pattern = "\\.docx$", full.names = TRUE)[1]

env <- function(x) { v <- Sys.getenv(x); if (nzchar(v)) v else NULL }
cfg <- list(
  survey_id = env("QUALTRICS_SURVEY_ID"),
  password  = env("APP_PASSWORD"),
  demo_dir  = env("DEMO_DATA_DIR"),
  has_api   = !is.null(env("QUALTRICS_API_KEY")) && !is.null(env("QUALTRICS_BASE_URL")) &&
              !is.null(env("QUALTRICS_SURVEY_ID"))
)
data_source <- if (!is.null(cfg$demo_dir)) "demo" else if (cfg$has_api) "api" else "none"

# One download shared by everyone using the app; refreshed on request or
# when older than 30 minutes, so the API is not called on every visit.
cache <- new.env()
get_exports <- function(force = FALSE) {
  fresh <- !is.null(cache$time) && difftime(Sys.time(), cache$time, units = "mins") < 30
  if (!force && fresh) return(cache)
  if (data_source == "api") {
    ex <- fetch_qualtrics_exports(cfg$survey_id)
    cache$num <- ex$numeric; cache$lab <- ex$labels
    cache$source <- "Qualtrics API"
  } else if (data_source == "demo") {
    f <- resolve_input_files(list(data_dir = cfg$demo_dir, survey_dir = "survey"))
    cache$num <- read_qualtrics_csv(f$numeric)
    cache$lab <- if (!is.null(f$labels)) read_qualtrics_csv(f$labels) else NULL
    cache$source <- "Demo files"
  } else {
    stop("The app is not connected to Qualtrics yet. Ask the app administrator to set ",
         "QUALTRICS_API_KEY, QUALTRICS_BASE_URL and QUALTRICS_SURVEY_ID.")
  }
  cache$time <- Sys.time()
  cache
}

theme <- bs_theme(version = 5, primary = "#1F4E79", base_font = font_collection("Segoe UI", "Helvetica", "Arial", "sans-serif"),
                  "font-size-base" = "0.95rem")

# ---- UI ---------------------------------------------------------------------

main_ui <- page_sidebar(
  title = "FCECPR training application reports",
  theme = theme,
  sidebar = sidebar(
    width = 320,
    h6("1. Get responses"),
    actionButton("refresh", "Get latest responses from Qualtrics", icon = icon("rotate"),
                 class = "btn-primary"),
    uiOutput("data_status"),
    hr(),
    h6("2. Choose dates"),
    suppressWarnings(dateRangeInput("dates", NULL, start = NA, end = NA, format = "mm/dd/yyyy")),
    radioButtons("date_field", NULL,
                 choices = c("Date submitted" = "recorded", "Date started" = "start"),
                 inline = TRUE),
    actionLink("all_dates", "Reset to all dates"),
    hr(),
    accordion(
      open = FALSE,
      accordion_panel(
        "More settings",
        textAreaInput("exclude_ids", "Leave out these response IDs (one per line)", rows = 3),
        numericInput("quit_cutoff", "Count as 'quit' if progress is below (%)", 80, 1, 100),
        numericInput("trim_hours", "Leave completion times over this many hours out of averages", 4, 1, 200),
        sliderInput("low_rate", "Flag questions answered by fewer than", 50, 100, 80, post = "%")
      )
    )
  ),
  navset_card_tab(
    id = "tabs",
    nav_panel(
      "Overview",
      uiOutput("filter_note"),
      layout_columns(
        fill = FALSE,
        value_box("Responses", textOutput("n_all"), theme = "primary"),
        value_box("Submitted", textOutput("n_done"), theme = "success"),
        value_box("Started but not submitted", textOutput("n_open"), theme = "warning")
      ),
      DTOutput("overview_table")
    ),
    nav_panel(
      "Application reviews",
      p("One Word file per response, for reviewing the application. Select rows to download those, or download them all."),
      div(class = "d-flex gap-2 mb-2",
          downloadButton("dl_selected", "Download selected", class = "btn-primary"),
          downloadButton("dl_all_apps", "Download all (zip)")),
      DTOutput("app_table")
    ),
    nav_panel(
      "Survey functioning",
      p("How the form itself is working: time to complete, how many people quit and where, and how often each question is answered."),
      p("The methods report explains how each table and chart is calculated and shows the code."),
      div(class = "d-flex gap-2",
          downloadButton("dl_functioning", "Survey functioning report", class = "btn-primary"),
          downloadButton("dl_methods", "Methods and code"))
    ),
    nav_panel(
      "Feedback",
      p("Comments from the feedback section, word for word, with names, emails and phone numbers removed. No response IDs are shown."),
      downloadButton("dl_feedback", "Open-ended feedback report", class = "btn-primary")
    ),
    nav_panel(
      "Help",
      h5("Using the app"),
      tags$ol(
        tags$li("Click ", strong("Get latest responses from Qualtrics"), ". This takes up to a minute."),
        tags$li("Pick a date range if you want one. Leave it blank for all responses."),
        tags$li("Open a tab and click a download button. The reports are Word files.")
      ),
      h5("What the reports use"),
      p("All reports use only the responses in the date range you pick. Survey previews and tests are always left out. ",
        "Each response is its own entry, identified by its Qualtrics response ID."),
      p("'Date submitted' is the date Qualtrics recorded the response. For a form that was started but never submitted, ",
        "that is the date Qualtrics closed it, usually a week after the last activity.")
    )
  )
)

ui <- main_ui

# ---- Server -------------------------------------------------------------------

server <- function(input, output, session) {
  # Password gate: nothing is loaded or sent to the browser until the right
  # password is entered.
  authed <- reactiveVal(is.null(cfg$password))
  if (!is.null(cfg$password)) {
    showModal(modalDialog(
      title = "FCECPR training application reports",
      passwordInput("pw", "Password"),
      uiOutput("login_msg"),
      footer = actionButton("login", "Open", class = "btn-primary"),
      easyClose = FALSE
    ))
    observeEvent(input$login, {
      if (identical(input$pw, cfg$password)) {
        authed(TRUE)
        removeModal()
      } else {
        output$login_msg <- renderUI(div(class = "text-danger", "That password is not right."))
      }
    })
  }

  exports <- reactiveVal(NULL)
  load_data <- function(force) {
    withProgress(message = "Getting responses ...", value = 0.3, {
      res <- tryCatch(get_exports(force), error = function(e) e)
    })
    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = NULL)
    } else {
      exports(list(num = res$num, lab = res$lab, time = res$time, source = res$source))
    }
  }
  observeEvent(authed(), if (authed() && data_source != "none") load_data(FALSE))
  observeEvent(input$refresh, { req(authed()); load_data(TRUE) })
  # The date boxes start out covering every response; "Reset" puts them back.
  full_range <- reactive({
    ex <- exports(); req(ex)
    d <- ex$num$data
    tz_in <- ex$num$columns$tz[ex$num$columns$col == "StartDate"] %||% "UTC"
    as_day <- function(x) as.Date(with_tz(ymd_hms(x, tz = tz_in, quiet = TRUE), default_settings()$display_tz))
    days <- c(as_day(d$StartDate), as_day(d$RecordedDate))
    range(days, na.rm = TRUE)
  })
  reset_dates <- function() {
    r <- full_range()
    updateDateRangeInput(session, "dates", start = r[1], end = r[2])
  }
  observeEvent(full_range(), reset_dates())
  observeEvent(input$all_dates, reset_dates())

  output$data_status <- renderUI({
    ex <- exports()
    if (data_source == "none") return(div(class = "text-danger small mt-2",
      "Not connected to Qualtrics yet: the administrator needs to add the API settings."))
    if (is.null(ex)) return(div(class = "text-muted small mt-2", "No responses loaded yet."))
    div(class = "text-muted small mt-2",
        sprintf("%s: %d responses, updated %s", ex$source, nrow(ex$num$data),
                format(ex$time, "%b %d %I:%M %p")))
  })

  settings_raw <- reactive({
    ids <- trimws(strsplit(input$exclude_ids %||% "", "\n")[[1]])
    d <- as.Date(as.character(input$dates %||% c(NA, NA)), optional = TRUE)
    d[!is.na(d) & as.integer(format(d, "%Y")) < 2000] <- NA   # half-typed dates
    if (length(d) == 2 && !anyNA(d) && d[1] > d[2]) d <- rev(d)
    list(
      date_from = if (length(d) >= 1 && !is.na(d[1])) as.character(d[1]) else NULL,
      date_to   = if (length(d) >= 2 && !is.na(d[2])) as.character(d[2]) else NULL,
      date_field = input$date_field %||% "recorded",
      exclude_response_ids = ids[nzchar(ids)],
      quit_progress_cutoff = input$quit_cutoff %||% 80,
      duration_trim_hours = input$trim_hours %||% 4,
      low_response_threshold = (input$low_rate %||% 80) / 100
    )
  })
  settings <- debounce(settings_raw, 800)   # wait until typing stops

  dat <- reactive({
    req(authed())
    ex <- exports()
    req(ex)
    tryCatch(
      build_survey_data(ex$num, ex$lab, survey_docx, settings(), source = ex$source),
      error = function(e) {
        validate(need(FALSE, conditionMessage(e)))
      }
    )
  })

  output$filter_note <- renderUI({
    d <- dat()
    p(class = "text-muted", sprintf("%s. %d response(s) outside the dates and %d preview/test response(s) left out.",
                                    d$date_filter$text, d$date_filter$n_outside, nrow(d$excluded)))
  })
  output$n_all  <- renderText(nrow(dat()$responses))
  output$n_done <- renderText(sum(dat()$responses$finished))
  output$n_open <- renderText(sum(!dat()$responses$finished))

  response_table <- reactive({
    dat()$responses %>%
      mutate(last_date = if_else(finished, recorded, end)) %>%
      arrange(desc(last_date)) %>%
      transmute(`Response ID` = ResponseId,
                Provider = coalesce(provider, "(not entered)"),
                `Training title` = coalesce(training_title, "(not entered)"),
                Status = as.character(completion),
                Progress = paste0(progress, "%"),
                `Submitted / last activity` = format(last_date, "%m/%d/%Y"))
  })
  dt <- function(df, selection) {
    datatable(df, rownames = FALSE, selection = selection, filter = "top",
              options = list(pageLength = 15, dom = "tip", scrollX = TRUE))
  }
  output$overview_table <- renderDT(dt(response_table(), "none"))
  output$app_table <- renderDT(dt(response_table(), "multiple"))

  # ---- downloads -------------------------------------------------------------------
  render_with_progress <- function(which, response_ids = NULL) {
    out <- tempfile("reports_")
    d <- dat()
    withProgress(message = "Building reports ...", value = 0, {
      render_all_reports(
        d, out, project_dir, which = which,
        response_ids = response_ids %||% d$responses$ResponseId,
        progress = function(i, n, label) incProgress(1 / n, detail = label)
      )
    })
  }
  single_report <- function(which, prefix) {
    downloadHandler(
      filename = function() paste0(prefix, "_", format(Sys.Date(), "%Y-%m-%d"), ".docx"),
      content = function(file) {
        f <- render_with_progress(which)
        if (!length(f)) stop("The report could not be built.")
        file.copy(f[1], file)
      }
    )
  }
  output$dl_functioning <- single_report("functioning", "Survey_Functioning")
  output$dl_methods     <- single_report("methods", "Survey_Functioning_Methods")
  output$dl_feedback    <- single_report("feedback", "Open_Ended_Feedback")

  zip_or_single <- function(ids_fun) {
    downloadHandler(
      filename = function() {
        ids <- ids_fun()
        if (length(ids) == 1) {
          r <- dat()$responses %>% filter(ResponseId == ids)
          applicant_file_name(r$ResponseId, r$provider)
        } else paste0("Application_reviews_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        ids <- ids_fun()
        f <- render_with_progress("applicants", ids)
        if (!length(f)) stop("No reports could be built.")
        if (length(ids) == 1) file.copy(f[1], file)
        else zip::zip(file, files = f, mode = "cherry-pick")
      }
    )
  }
  selected_ids <- reactive({
    rows <- input$app_table_rows_selected
    if (!length(rows)) {
      showNotification("Select one or more rows in the table first.", type = "warning")
      req(FALSE)
    }
    response_table()$`Response ID`[rows]
  })
  output$dl_selected <- zip_or_single(function() selected_ids())
  output$dl_all_apps <- zip_or_single(function() dat()$responses$ResponseId)

  # Bind download links even while their tab is hidden, so a quick click
  # right after switching tabs downloads the file instead of the page.
  for (id in c("dl_functioning", "dl_methods", "dl_feedback", "dl_selected", "dl_all_apps")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}

shinyApp(ui, server)
