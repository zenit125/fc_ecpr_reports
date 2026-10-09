# =============================================================================
# FCECPR Training Registration Form: shared functions for the three reports
#
#   1. Reading the Qualtrics exports and the survey Word file
#   2. SURVEY-SPECIFIC RULES (display logic, required items, automated checks)
#      -> this is the section to edit when the form changes
#   3. Building the analysis data: prepare_survey_data()
#   4. Formatting helpers for Word output
#
# Everything is driven from run_reports.R; the .Rmd files only read the
# prepared data and call the helpers below.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(flextable)
  library(officer)
  library(knitr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a


# =============================================================================
# 1. READING THE INPUT FILES
# =============================================================================

#' Read a Qualtrics CSV export (3 header rows: names, question text, ImportIds).
#' Every column is read as text so nothing is silently converted.
read_qualtrics_csv <- function(path) {
  raw <- readr::read_csv(
    path, col_names = FALSE, na = "", trim_ws = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE, show_col_types = FALSE
  )
  if (nrow(raw) < 3 || !any(grepl("ImportId", unlist(raw[3, ])))) {
    stop("'", basename(path), "' does not look like a Qualtrics CSV export ",
         "(expected three header rows, the third holding ImportIds).")
  }
  hdr <- as.character(unlist(raw[1, ]))
  import <- as.character(unlist(raw[3, ]))
  columns <- tibble(
    col       = hdr,
    label     = as.character(unlist(raw[2, ])),
    import_id = str_match(import, '"ImportId":"([^"]+)"')[, 2],
    tz        = str_match(import, '"timeZone":"([^"]+)"')[, 2],
    position  = seq_along(hdr)
  )
  data <- raw[-(1:3), , drop = FALSE]
  names(data) <- hdr
  list(data = data, columns = columns, path = path)
}

#' Is this the "numeric values" or the "choice text" export?
#' Decided from the Finished column (1/0 vs True/False).
detect_export_type <- function(x) {
  fin <- x$data$Finished[!is.na(x$data$Finished)]
  if (length(fin) == 0) return(NA_character_)
  if (all(fin %in% c("0", "1"))) return("numeric")
  if (all(tolower(fin) %in% c("true", "false"))) return("labels")
  NA_character_
}

#' Work out which files to use. Explicit paths in settings win; otherwise the
#' newest numeric CSV, newest choice-text CSV and newest .docx in data_dir.
resolve_input_files <- function(settings) {
  out <- list(numeric = settings$numeric_csv, labels = settings$labels_csv,
              docx = settings$survey_docx)
  newest_first <- function(pattern) {
    f <- list.files(settings$data_dir, pattern = pattern, full.names = TRUE,
                    ignore.case = TRUE)
    f <- f[!startsWith(basename(f), "~$")]          # Word lock files
    f[order(file.mtime(f), decreasing = TRUE)]
  }
  if (is.null(out$numeric) || is.null(out$labels)) {
    csvs <- newest_first("\\.csv$")
    types <- vapply(csvs, function(f) detect_export_type(read_qualtrics_csv(f)),
                    character(1))
    if (is.null(out$numeric)) out$numeric <- csvs[which(types == "numeric")[1]]
    if (is.null(out$labels))  out$labels  <- csvs[which(types == "labels")[1]]
  }
  if (is.null(out$docx)) {
    # The survey Word file lives in survey/ (shared with the Shiny app);
    # a copy in data/ also works.
    docs <- unlist(lapply(c(settings$survey_dir, settings$data_dir), function(d) {
      f <- list.files(d, pattern = "\\.docx$", full.names = TRUE, ignore.case = TRUE)
      f <- f[!startsWith(basename(f), "~$")]
      f[order(file.mtime(f), decreasing = TRUE)]
    }))
    out$docx <- docs[1]
  }

  if (length(out$numeric) == 0 || is.na(out$numeric)) {
    stop("Could not find the numeric-values export in '", settings$data_dir,
         "'. Export with 'Use numeric values' or set settings$numeric_csv.")
  }
  if (length(out$labels) == 0 || is.na(out$labels)) {
    warning("No choice-text export found; answer labels will come from the survey ",
            "Word file only.")
    out$labels <- NULL
  }
  if (length(out$docx) == 0 || is.na(out$docx)) {
    stop("Could not find the survey Word file (.docx) in the survey/ or data/ folder.")
  }
  out
}

clean_block_name <- function(x) {
  str_trim(sub("\\s+-\\s+[A-Z][a-z]{2} \\d{1,2}, \\d{4}$", "", x))   # drop " - Sep 16, 2026"
}

#' Parse the Qualtrics "Export survey to Word" file for (a) which block each
#' question sits in and (b) the code -> label list for every choice question.
#' For matrix questions the first numbered list is the column (answer) scale.
read_survey_structure <- function(path, tags) {
  tmp <- tempfile("survey_docx_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(path, files = "word/document.xml", exdir = tmp)
  doc <- xml2::read_xml(file.path(tmp, "word", "document.xml"))
  paras <- xml2::xml_find_all(doc, ".//w:p", xml2::xml_ns(doc))
  lines <- str_squish(xml2::xml_text(paras))
  lines <- lines[lines != ""]

  choice_re <- "^(.*?)\\s*\\((\\d+)\\)\\s*(_*)\\s*$"
  block_of <- list()
  choices <- list()
  block_order <- character()
  cur_block <- NA_character_
  cur_q <- NA_character_
  last_code <- 0L

  for (l in lines) {
    if (startsWith(l, "Start of Block:")) {
      cur_block <- clean_block_name(sub("^Start of Block:\\s*", "", l))
      block_order <- c(block_order, cur_block)
      cur_q <- NA_character_
      next
    }
    if (startsWith(l, "End of Block:")) {
      cur_block <- NA_character_
      cur_q <- NA_character_
      next
    }
    m <- regmatches(l, regexec(choice_re, l, perl = TRUE))[[1]]
    is_choice <- length(m) > 0
    tok <- sub(" .*$", "", l)
    if (!is_choice && tok %in% tags && is.null(block_of[[tok]])) {
      block_of[[tok]] <- cur_block
      cur_q <- tok
      last_code <- 0L
      next
    }
    if (!is.na(cur_q) && is_choice) {
      if (nchar(m[4]) > 0) next                    # "Name (1) ____" = text field
      code <- as.integer(m[3])
      if (code <= last_code) {                     # numbering restarted:
        cur_q <- NA_character_                     # matrix rows begin; stop
        next
      }
      choices[[length(choices) + 1]] <- tibble(tag = cur_q, code = code,
                                               choice = str_trim(m[2]))
      last_code <- code
    }
  }
  list(
    blocks = tibble(tag = names(block_of), block = unlist(block_of, use.names = FALSE)),
    block_order = unique(block_order),
    choices = bind_rows(choices) %>%
      { if (nrow(.) == 0) tibble(tag = character(), code = integer(), choice = character()) else . }
  )
}


# =============================================================================
# 2. SURVEY-SPECIFIC RULES  (edit this section when the form changes)
# =============================================================================

# Helper used by the rules: did the respondent select any of `codes`?
# `x` is the numeric-export value, e.g. "1,3,5" for a multi-select.
selected <- function(x, codes) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(FALSE)
  any(suppressWarnings(as.integer(strsplit(x, ",")[[1]])) %in% codes)
}

# --- 2a. When does a question apply? ----------------------------------------
# Questions not listed here apply to everyone. Each rule gets `r`, a named
# list of one respondent's numeric-export values, and returns TRUE/FALSE.
# If a question does not apply, it is never counted as missing.
applicability_rules <- function() {
  rules <- list(
    PriorID         = function(r) selected(r$SubmissionType, 2:4),
    Audience_Other  = function(r) selected(r$Audience, 5),
    Hybrid_Split    = function(r) selected(r$Delivery, 4),
    Language_Other  = function(r) selected(r$Language, 5),
    ContactHours    = function(r) selected(r$HoursType, 1),
    CEUs            = function(r) selected(r$HoursType, 2),
    Enrollment      = function(r) selected(r$Enroll_Applies, 1),
    Access_Counties = function(r) selected(r$Access, 3),
    Location        = function(r) selected(r$Location_Type, 1),
    Cost_Fee        = function(r) selected(r$Cost, 2),
    CourseAccess    = function(r) selected(r$Delivery, 2:4),   # online or hybrid
    Upload_Handouts = function(r) selected(r$Checklist, 3),    # "required if used"
    Upload_Slides   = function(r) selected(r$Checklist, 4),
    FELDS_StdNums   = function(r) !is.na(r$FELDS_Domains %||% NA)
  )
  # Display logic: competency items appear only for selected standards,
  # FELDS components only for selected domains.
  for (s in 1:7) {
    local({
      code <- s
      rules[[paste0("PC_Items_Std", code)]] <<- function(r) selected(r$PC_Standards, code)
    })
  }
  roman <- as.character(as.roman(1:8))
  for (d in 1:8) {
    local({
      code <- d
      rules[[paste0("FELDS_", roman[code])]] <<- function(r) selected(r$FELDS_Domains, code)
    })
  }
  rules
}

# --- 2b. Optional questions (never flagged as missing) ------------------------
optional_questions <- c("Access_Other", "AddlDates", "Cost_Scholar",
                        "Upload_Guide", "Upload_Certificate")

# --- 2c. Which parts of a multi-part question are required? -------------------
# Default for multi-part questions: answered if ANY part is filled in.
# "all" = every part required; a vector = only those columns required.
required_parts <- list(
  Contact      = "all",
  Signer       = "all",
  CEUs         = "all",
  Enrollment   = "all",
  Trainers     = "Trainers_1",
  CourseAccess = "CourseAccess_1",
  Location     = c("Location_3", "Location_4")      # city and county
)

# Questions whose text is long enough that reviewers want a word count.
word_count_questions <- c(Overview = "200-500", Description = "50-150",
                          Agenda_Desc = NA, Assessment_Desc = NA)

# --- 2d. Automated reviewer checks -------------------------------------------
# Returns one row per check: Check / Result ("OK", "Review", "Not applicable",
# "Can't check") / Detail. `r` = named list of numeric-export values,
# `submitted` = date the response was recorded.
application_checks <- function(r, submitted) {
  v <- function(x) { y <- r[[x]]; if (is.null(y) || is.na(y)) NA_character_ else y }
  codes <- function(x) {
    y <- v(x); if (is.na(y)) integer() else suppressWarnings(as.integer(strsplit(y, ",")[[1]]))
  }
  num <- function(x) suppressWarnings(readr::parse_number(v(x)))
  rows <- list()
  add <- function(check, result, detail = "") {
    rows[[length(rows) + 1]] <<- tibble(Check = check, Result = result, Detail = detail)
  }
  length_check <- function(var, lo, hi, label) {
    n <- word_count(v(var))
    if (is.na(v(var))) add(label, "Can't check", "Blank")
    else add(label, if (n >= lo && n <= hi) "OK" else "Review", paste(n, "words"))
  }

  length_check("Overview", 200, 500, "Overview is 200-500 words")
  length_check("Description", 50, 150, "Description is 50-150 words")

  if (1 %in% codes("HoursType")) {
    h <- num("ContactHours")
    if (is.na(h)) add("Contact hours are at least 2.0", "Can't check",
                      paste0("Entered: ", v("ContactHours") %||% "blank"))
    else add("Contact hours are at least 2.0", if (h >= 2) "OK" else "Review", paste(h, "hours"))
  } else if (2 %in% codes("HoursType")) {
    ceu <- num("CEUs_2")
    if (is.na(ceu)) add("CEUs requested are at least 0.2", "Can't check",
                        paste0("Entered: ", v("CEUs_2") %||% "blank"))
    else add("CEUs requested are at least 0.2", if (ceu >= 0.2) "OK" else "Review", paste(ceu, "CEUs"))
  }

  fd_raw <- v("FirstDate")
  fd <- suppressWarnings(as.Date(parse_date_time(fd_raw, c("mdy", "ymd"), quiet = TRUE)))
  if (is.na(fd_raw)) {
    add("First delivery date is at least 45 business days after submission", "Can't check", "Blank")
  } else if (is.na(fd) || is.na(submitted)) {
    add("First delivery date is at least 45 business days after submission", "Review",
        paste0("Could not read the date '", fd_raw, "'"))
  } else {
    bd <- business_days_between(as.Date(submitted), fd)
    add("First delivery date is at least 45 business days after submission",
        if (bd >= 45) "OK" else "Review",
        sprintf("%s is %d business days after %s (holidays not excluded)",
                format(fd, "%m/%d/%Y"), bd, format(as.Date(submitted), "%m/%d/%Y")))
  }

  if (1 %in% codes("Enroll_Applies")) {
    lo <- num("Enrollment_1"); hi <- num("Enrollment_2")
    if (is.na(lo) || is.na(hi)) add("Enrollment minimum is not above maximum", "Can't check", "Min or max blank")
    else add("Enrollment minimum is not above maximum", if (lo <= hi) "OK" else "Review",
             sprintf("Min %s, max %s", lo, hi))
  }

  # Checklist vs. what was actually uploaded/provided
  material <- c(`1` = "Agenda_Upload_Id", `2` = "Assessment_Upload_Id",
                `3` = "Upload_Handouts_Id", `4` = "Upload_Slides_Id",
                `5` = "CourseAccess_1", `6` = "Upload_Guide_Id",
                `7` = "Upload_Certificate_Id")
  material_name <- c(`1` = "agenda", `2` = "assessment", `3` = "handouts",
                     `4` = "slides", `5` = "online course access",
                     `6` = "facilitator guide", `7` = "sample certificate")
  ticked <- codes("Checklist")
  provided <- names(material)[!is.na(vapply(material, v, character(1)))]
  missing_files <- setdiff(as.character(ticked), provided)
  unticked <- setdiff(provided, as.character(ticked))
  detail <- c(
    if (length(missing_files)) paste0("Checked but not provided: ",
                                      paste(material_name[missing_files], collapse = ", ")),
    if (length(unticked)) paste0("Provided but not checked: ",
                                 paste(material_name[unticked], collapse = ", "))
  )
  if (is.na(v("Checklist"))) {
    add("Materials checklist matches what was uploaded", "Can't check", "Checklist blank")
  } else {
    add("Materials checklist matches what was uploaded",
        if (length(detail)) "Review" else "OK",
        paste(detail, collapse = "; "))
  }

  std <- codes("PC_Standards")
  if (length(std)) {
    empty <- std[is.na(vapply(paste0("PC_Items_Std", std), v, character(1)))]
    add("Each selected competency standard has items selected",
        if (length(empty)) "Review" else "OK",
        if (length(empty)) paste("No items for Standard", paste(empty, collapse = ", ")) else "")
  }
  dom <- codes("FELDS_Domains")
  if (length(dom)) {
    roman <- as.character(as.roman(1:8))
    empty <- dom[vapply(dom, function(d) {
      cols <- grep(paste0("^FELDS_", roman[d], "_\\d+$"), names(r), value = TRUE)
      all(is.na(unlist(r[cols])))
    }, logical(1))]
    add("Each selected FELDS domain has components selected",
        if (length(empty)) "Review" else "OK",
        if (length(empty)) paste("No components for domain", paste(roman[empty], collapse = ", ")) else "")
  }
  if (8 %in% codes("AgeGroups") && length(codes("AgeGroups")) > 1) {
    add("Age groups: N/A not combined with specific ages", "Review", "N/A and age groups both selected")
  }
  if (5 %in% codes("Programs") && length(codes("Programs")) > 1) {
    add("Programs: N/A not combined with a program", "Review", "N/A and a program both selected")
  }
  if (is.na(v("Attest"))) add("Attestation checked", "Review", "Not checked (blank)")
  else add("Attestation checked", if (1 %in% codes("Attest")) "OK" else "Review", "")
  bind_rows(rows)
}

business_days_between <- function(from, to) {
  if (to <= from) return(0L)
  d <- seq(from + 1, to, by = "day")
  sum(!lubridate::wday(d) %in% c(1, 7))
}


# =============================================================================
# 3. BUILD THE ANALYSIS DATA
# =============================================================================

status_codes <- c(`0` = "IP Address", `1` = "Survey Preview", `2` = "Survey Test",
                  `4` = "Imported", `8` = "Spam", `9` = "Survey Preview Spam",
                  `12` = "Imported Spam", `16` = "Offline", `17` = "Offline Survey Preview")

#' Read the exported files and return one list used by all three reports.
prepare_survey_data <- function(settings) {
  files <- resolve_input_files(settings)
  num <- read_qualtrics_csv(files$numeric)
  lab <- if (!is.null(files$labels)) read_qualtrics_csv(files$labels) else NULL
  build_survey_data(num, lab, files$docx, settings, source = basename(unlist(files)))
}

#' Build the analysis data from an export already in memory.
#' `num` / `lab` are list(data, columns) as returned by read_qualtrics_csv()
#' or qualtrics_to_export(); `docx` is the survey Word file.
build_survey_data <- function(num, lab, docx, settings, source = character()) {
  settings <- modifyList(default_settings(), settings, keep.null = TRUE)
  if (!is.null(lab) && !identical(num$columns$col, lab$columns$col)) {
    stop("The numeric and choice-text exports have different columns; ",
         "export both from the same survey version.")
  }

  # ---- column / question metadata ------------------------------------------
  columns <- num$columns %>%
    mutate(
      is_meta  = !str_detect(coalesce(import_id, ""), "^QID"),
      qid      = str_extract(import_id, "^QID\\d+"),
      first_line = str_trim(str_split_i(label, "\r?\n", 1))
    ) %>%
    # A question's columns share a QID; its export tag is the column name
    # without the part suffix (Contact_1 -> Contact, Agenda_Upload_Id -> Agenda_Upload)
    group_by(qid) %>%
    mutate(tag = case_when(
      is_meta ~ NA_character_,
      n() == 1 ~ col,
      TRUE ~ str_remove(col, "_(\\d+|Id|Name|Size|Type)$")
    )) %>%
    group_by(tag) %>%
    mutate(n_cols = n()) %>%
    ungroup() %>%
    mutate(
      title    = if_else(n_cols > 1 & !is_meta, str_remove(first_line, " - .*$"), first_line),
      sublabel = if_else(n_cols > 1 & !is_meta, str_remove(label, "(?s)^.* - "), NA_character_)
    )

  tags <- unique(na.omit(columns$tag))
  structure <- read_survey_structure(docx, tags)
  feedback_blocks <- structure$block_order[
    str_detect(structure$block_order, regex(settings$feedback_block_pattern, ignore_case = TRUE))]

  questions <- columns %>%
    filter(!is_meta) %>%
    group_by(tag) %>%
    summarise(position = min(position), cols = list(col), n_cols = n(),
              title = first(title), .groups = "drop") %>%
    arrange(position) %>%
    left_join(structure$blocks, by = "tag") %>%
    mutate(
      block = coalesce(block, "Unassigned"),
      block = factor(block, levels = unique(c(structure$block_order, "Unassigned"))),
      is_upload = map_lgl(cols, ~ any(str_detect(.x, "_Id$")) && any(str_detect(.x, "_Name$"))),
      has_choices = tag %in% structure$choices$tag,
      type = case_when(
        is_upload ~ "upload",
        has_choices & n_cols > 1 ~ "matrix",
        has_choices ~ "choice",
        n_cols > 1 ~ "text_group",
        TRUE ~ "text"
      ),
      is_feedback = block %in% feedback_blocks,
      optional = tag %in% optional_questions | is_feedback,   # feedback is voluntary
      order = row_number()
    ) %>%
    select(-is_upload)

  # ---- response-level metadata -----------------------------------------------
  tz_in <- num$columns$tz[num$columns$col == "StartDate"] %||% "UTC"
  parse_dt <- function(x) with_tz(ymd_hms(x, tz = tz_in, quiet = TRUE), settings$display_tz)
  d <- num$data
  col_or_na <- function(df, name) if (name %in% names(df)) df[[name]] else rep(NA_character_, nrow(df))
  status_label <- if (!is.null(lab)) col_or_na(lab$data, "Status") else
    unname(status_codes[col_or_na(d, "Status")])

  # Each response is its own entry, identified by its Qualtrics ResponseId.
  responses <- tibble(
    ResponseId     = d$ResponseId,
    status         = coalesce(status_label, col_or_na(d, "Status")),
    start          = parse_dt(col_or_na(d, "StartDate")),
    end            = parse_dt(col_or_na(d, "EndDate")),
    recorded       = parse_dt(col_or_na(d, "RecordedDate")),
    progress       = as.numeric(col_or_na(d, "Progress")),
    duration_sec   = as.numeric(col_or_na(d, "Duration (in seconds)")),
    finished       = col_or_na(d, "Finished") %in% c("1", "True", "TRUE", "true"),
    last_seen_qids = col_or_na(d, "Last Seen Question IDs"),
    provider       = col_or_na(d, "Provider"),
    contact_name   = col_or_na(d, "Contact_1"),
    contact_email  = col_or_na(d, "Contact_4"),
    training_title = col_or_na(d, "Title")
  ) %>%
    mutate(has_contact = !(is.na(provider) & is.na(contact_name) & is.na(contact_email)))

  # ---- date range ------------------------------------------------------------------
  # Keep responses whose recorded (or start) date falls inside the range,
  # counting whole calendar days in the display time zone. Both ends inclusive.
  date_col <- if (identical(settings$date_field, "start")) "start" else "recorded"
  day <- as.Date(responses[[date_col]], tz = settings$display_tz)
  in_range <- !is.na(day)
  if (!is.null(settings$date_from)) in_range <- in_range & day >= as.Date(settings$date_from)
  if (!is.null(settings$date_to))   in_range <- in_range & day <= as.Date(settings$date_to)
  date_filter <- list(
    field = date_col, from = settings$date_from, to = settings$date_to,
    n_before = nrow(responses), n_outside = sum(!in_range),
    text = describe_date_filter(settings$date_from, settings$date_to, date_col)
  )
  responses <- responses[in_range, ]

  # ---- exclusions ----------------------------------------------------------------
  ids_file <- settings$exclude_ids_file
  file_ids <- if (!is.null(ids_file) && file.exists(ids_file)) {
    x <- str_trim(readLines(ids_file, warn = FALSE)); x[x != "" & !startsWith(x, "#")]
  } else character()
  drop_ids <- unique(c(settings$exclude_response_ids, file_ids))
  excluded <- responses %>%
    filter(status %in% settings$exclude_status | ResponseId %in% drop_ids) %>%
    mutate(reason = if_else(ResponseId %in% drop_ids, "Listed for exclusion",
                            paste("Status:", status)))
  responses <- responses %>% filter(!ResponseId %in% excluded$ResponseId)
  keep <- d$ResponseId %in% responses$ResponseId
  if (!any(keep)) {
    stop("No responses left after the date range and exclusions (", date_filter$text, ").")
  }

  # ---- answers (long: one row per respondent x column) ---------------------------
  qcols <- columns$col[!columns$is_meta]
  long <- d[keep, c("ResponseId", qcols)] %>%
    pivot_longer(-ResponseId, names_to = "col", values_to = "num")
  if (!is.null(lab)) {
    long <- long %>% left_join(
      lab$data[lab$data$ResponseId %in% responses$ResponseId, c("ResponseId", qcols)] %>%
        pivot_longer(-ResponseId, names_to = "col", values_to = "lab"),
      by = c("ResponseId", "col"))
  } else {
    long$lab <- NA_character_
  }
  choice_lookup <- split(structure$choices, structure$choices$tag)
  answers <- long %>%
    left_join(columns %>% select(col, tag, sublabel, position), by = "col") %>%
    mutate(value = pmap(list(num, lab, tag), function(n, l, t) {
      decode_answer(n, l, choice_lookup[[t]])
    }))

  # ---- how far did each respondent get? ---------------------------------------------
  last_pos <- answers %>%
    filter(!is.na(num)) %>%
    group_by(ResponseId) %>%
    slice_max(position, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(ResponseId, last_position = position, last_tag = tag)

  qid_to_tag <- columns %>%
    filter(!is_meta) %>%
    mutate(qid = str_extract(import_id, "^QID\\d+")) %>%
    distinct(qid, tag) %>%
    left_join(questions %>% select(tag, title), by = "tag")

  responses <- responses %>%
    left_join(last_pos, by = "ResponseId") %>%
    left_join(questions %>% select(last_tag = tag, last_block = block, last_title = title),
              by = "last_tag") %>%
    mutate(
      last_position = coalesce(last_position, 0L),
      last_seen = map_chr(last_seen_qids, function(x) {
        if (is.na(x)) return(NA_character_)
        q <- str_extract_all(x, "QID\\d+")[[1]]
        t <- qid_to_tag$title[match(q, qid_to_tag$qid)]
        paste(unique(coalesce(t, q)), collapse = ", ")
      }),
      completion = case_when(
        finished ~ "Completed",
        progress >= settings$quit_progress_cutoff ~ "Not submitted (near complete)",
        TRUE ~ "Quit"
      ),
      completion = factor(completion, levels = c("Completed", "Not submitted (near complete)", "Quit"))
    )

  # ---- question status per respondent -------------------------------------------------
  qstatus <- build_question_status(answers, questions, responses, d[keep, ])

  list(
    settings = settings,
    files = as.list(source),
    date_filter = date_filter,
    columns = columns,
    questions = questions,
    choices = structure$choices,
    block_order = structure$block_order,
    feedback_blocks = feedback_blocks,
    responses = responses,
    excluded = excluded,
    answers = answers,
    qstatus = qstatus,
    raw_numeric = d[keep, ],
    prepared_at = Sys.time()
  )
}

#' Turn one cell into display text. Multi-select codes ("1,3") are split and
#' mapped to labels via the survey Word file, because choice labels can
#' themselves contain commas. Anything else uses the choice-text export.
decode_answer <- function(num, lab, choices) {
  if (is.na(num) && is.na(lab)) return(character())
  if (!is.na(num) && !is.null(choices) && str_detect(num, "^\\d+(,\\d+)*$")) {
    codes <- as.integer(strsplit(num, ",")[[1]])
    labs <- choices$choice[match(codes, choices$code)]
    if (!anyNA(labs)) return(labs)
  }
  coalesce(lab, num)
}

build_question_status <- function(answers, questions, responses, raw) {
  rules <- applicability_rules()
  rows_as_lists <- split(raw, raw$ResponseId) %>% map(~ as.list(.x[1, ]))

  applicable <- map_dfr(names(rows_as_lists), function(id) {
    r <- rows_as_lists[[id]]
    tibble(ResponseId = id, tag = names(rules),
           applicable = map_lgl(rules, function(f) isTRUE(f(r))))
  })

  req_lookup <- questions %>%
    mutate(req = map2(tag, cols, function(t, cc) {
      spec <- required_parts[[t]]
      if (is.null(spec)) NULL else if (identical(spec, "all")) cc else spec
    })) %>%
    select(tag, req)
  req_list <- setNames(req_lookup$req, req_lookup$tag)

  answers %>%
    group_by(ResponseId, tag) %>%
    summarise(
      answered_any = any(!is.na(num)),
      complete = {
        req <- req_list[[first(tag)]]
        if (is.null(req)) any(!is.na(num)) else all(!is.na(num[col %in% req]))
      },
      position = min(position),
      .groups = "drop"
    ) %>%
    left_join(applicable, by = c("ResponseId", "tag")) %>%
    left_join(questions %>% select(tag, block, title, type, optional, is_feedback, order),
              by = "tag") %>%
    left_join(responses %>% select(ResponseId, finished, last_position), by = "ResponseId") %>%
    mutate(
      applicable = coalesce(applicable, TRUE),
      reached = finished | position <= last_position,
      status = case_when(
        !applicable ~ "Not applicable",
        complete | (optional & answered_any) ~ "Answered",
        answered_any ~ "Incomplete",
        optional ~ "Optional - blank",
        !reached ~ "Not reached",
        TRUE ~ "Missing"
      )
    ) %>%
    arrange(ResponseId, order)
}

describe_date_filter <- function(from, to, field) {
  what <- if (field == "start") "started" else "recorded (submitted)"
  fmt <- function(x) format(as.Date(x), "%m/%d/%Y")
  if (is.null(from) && is.null(to)) return("All dates")
  if (is.null(from)) return(sprintf("Responses %s on or before %s", what, fmt(to)))
  if (is.null(to)) return(sprintf("Responses %s on or after %s", what, fmt(from)))
  sprintf("Responses %s from %s to %s", what, fmt(from), fmt(to))
}

#' Defaults for every setting, so the script or the app only has to give the
#' ones it changes.
default_settings <- function() {
  list(
    exclude_status = c("Survey Preview", "Survey Test", "Spam",
                       "Survey Preview Spam", "Offline Survey Preview"),
    exclude_response_ids = character(),
    exclude_ids_file = NULL,
    date_from = NULL, date_to = NULL, date_field = "recorded",
    applicant_sections = c("Section 1 Training Overview", "Training Topics",
                           "Section 2 Training Design", "Section 3 Supporting Materials",
                           "Section 4 Attestation"),
    feedback_block_pattern = "feedback",
    quit_progress_cutoff = 80,
    duration_trim_method = "fixed",
    duration_trim_hours = 4,
    low_response_threshold = 0.80,
    display_tz = "America/New_York"
  )
}

#' File name for one applicant report: the ResponseId (the identifier), then
#' the provider name so the folder is easy to browse.
applicant_file_name <- function(response_id, provider) {
  who <- if (is.na(provider) || !nzchar(safe_filename(provider))) "no_provider" else safe_filename(provider)
  sprintf("Application_%s_%s.docx", response_id, who)
}


# =============================================================================
# 3b. QUALTRICS API (used by the Shiny app)
# =============================================================================

#' Convert a data frame from qualtRics::fetch_survey() or read_survey() into the
#' same shape as read_qualtrics_csv(), so everything downstream is shared.
qualtrics_to_export <- function(df) {
  cm <- qualtRics::extract_colmap(df)
  if (is.null(cm)) stop("The Qualtrics data has no column map; use add_column_map = TRUE.")
  cm <- cm[match(names(df), cm$qname), ]
  data <- lapply(df, function(x) {
    x <- if (inherits(x, "POSIXct")) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC") else as.character(x)
    x[!is.na(x) & x == ""] <- NA_character_
    x
  })
  data <- tibble::as_tibble(data, .name_repair = "minimal")
  columns <- tibble(
    col       = names(df),
    label     = coalesce(cm$description, names(df)),
    import_id = cm$ImportId,
    tz        = if ("timeZone" %in% names(cm)) cm$timeZone else NA_character_,
    position  = seq_along(df)
  )
  list(data = data, columns = columns, path = "Qualtrics API")
}

#' Download every response twice (numeric codes and choice text) through the
#' Qualtrics API. Credentials come from the QUALTRICS_API_KEY and
#' QUALTRICS_BASE_URL environment variables.
fetch_qualtrics_exports <- function(survey_id, verbose = FALSE) {
  get <- function(label) {
    df <- suppressWarnings(qualtRics::fetch_survey(
      surveyID = survey_id,
      label = label, convert = FALSE,
      breakout_sets = FALSE,          # keep multi-select answers in one "1,3" column
      col_types = readr::cols(.default = readr::col_character()),
      time_zone = "UTC",
      add_column_map = TRUE, add_var_labels = FALSE,
      include_display_order = FALSE,
      verbose = verbose
    ))
    out <- qualtrics_to_export(df)
    out$columns$tz[out$columns$col %in% c("StartDate", "EndDate", "RecordedDate")] <- "UTC"
    out
  }
  list(numeric = get(FALSE), labels = get(TRUE))
}


# =============================================================================
# 4. FORMATTING HELPERS
# =============================================================================

word_count <- function(x) {
  if (length(x) == 0 || is.na(x)) return(0L)
  length(strsplit(str_squish(x), " ")[[1]])
}

#' Escape respondent text so it is printed literally in the Word file
#' (pandoc treats any backslash-escaped ASCII punctuation as literal).
md_escape <- function(x) {
  x <- gsub("([!-/:-@\\[-`{-~])", "\\\\\\1", x, perl = TRUE)
  gsub(" ", " ", x)
}

#' Respondent text -> markdown, keeping their line breaks.
md_text <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  paras <- strsplit(gsub("\r\n?", "\n", x), "\n[ \t]*\n")[[1]]
  paras <- vapply(paras, function(p) {
    lines <- str_trim(strsplit(p, "\n")[[1]])
    paste(md_escape(lines[lines != ""]), collapse = "\\\n")
  }, character(1))
  paste(paras[paras != ""], collapse = "\n\n")
}

#' Text in a named Word character style (styles live in templates/reference.docx).
styled <- function(text, style) sprintf('[%s]{custom-style="%s"}', md_escape(text), style)

fmt_bytes <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("size unknown")
  if (x >= 1024^2) sprintf("%.1f MB", x / 1024^2) else sprintf("%.0f KB", x / 1024)
}

fmt_duration <- function(sec) {
  ifelse(is.na(sec), "-",
         ifelse(sec < 3600, sprintf("%d min", round(sec / 60)),
                sprintf("%d h %02d min", sec %/% 3600, round((sec %% 3600) / 60))))
}

fmt_dt <- function(x) ifelse(is.na(x), "-", format(x, "%m/%d/%Y %I:%M %p"))

safe_filename <- function(x, max = 40) {
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- str_replace_all(x, "[^A-Za-z0-9]+", "_")
  x <- str_remove_all(x, "^_+|_+$")
  substr(x, 1, max)
}

#' Standard flextable look for these reports.
ft_style <- function(ft, font_size = 9.5) {
  ft %>%
    flextable::font(fontname = "Calibri", part = "all") %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::bold(part = "header") %>%
    flextable::bg(bg = "#EEF3F8", part = "header") %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = officer::fp_border(color = "#8C8C8C", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "#8C8C8C", width = 1), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(color = "#8C8C8C", width = 1), part = "body") %>%
    flextable::hline(border = officer::fp_border(color = "#E3E3E3", width = 0.5), part = "body") %>%
    flextable::padding(padding = 3, part = "all") %>%
    flextable::valign(valign = "top", part = "all") %>%
    flextable::set_table_properties(layout = "autofit", width = 1)
}

#' Colour a status column: red = needs attention, green = fine, grey = n/a.
ft_status_colors <- function(ft, j, bad = c("Missing", "Review", "Incomplete", "Not reached", "Quit"),
                             good = c("OK", "Complete", "Completed", "Answered")) {
  vals <- ft$body$dataset[[j]]
  bad_rows  <- which(str_detect(vals, paste0("^(", paste(bad, collapse = "|"), ")")))
  good_rows <- which(vals %in% good)
  if (length(bad_rows))  ft <- flextable::color(ft, i = bad_rows, j = j, color = "#B3261E") %>%
      flextable::bold(i = bad_rows, j = j)
  if (length(good_rows)) ft <- flextable::color(ft, i = good_rows, j = j, color = "#1E6B32")
  ft
}

#' Clean ggplot theme for Word figures.
theme_report <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#E6E6E3", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      axis.text = element_text(color = "#52514E"),
      axis.title = element_text(color = "#52514E"),
      plot.title = element_text(face = "bold", color = "#0B0B0B"),
      plot.title.position = "plot",
      legend.position = "top",
      legend.justification = "left"
    )
}
report_blue <- "#2A78D6"
likert_colors <- c("1" = "#C9403F", "2" = "#EFA3A2", "3" = "#D4D3CE", "4" = "#86B6EF", "5" = "#2A78D6")

#' Remove identifying details from open-ended text: emails, phone numbers, URLs
#' and any provider / contact / trainer / signer names given in the survey.
anonymize_text <- function(x, names_to_redact) {
  x <- str_replace_all(x, "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[A-Za-z]{2,}", "[email]")
  x <- str_replace_all(x, "\\(?\\b\\d{3}\\)?[-. ]?\\d{3}[-. ]\\d{4}\\b", "[phone]")
  x <- str_replace_all(x, "https?://\\S+|www\\.\\S+", "[link]")
  nm <- unique(str_squish(names_to_redact))
  nm <- nm[!is.na(nm) & nchar(nm) >= 3]
  nm <- nm[order(nchar(nm), decreasing = TRUE)]
  for (n in nm) x <- str_replace_all(x, fixed(n, ignore_case = TRUE), "[name]")
  x
}

is_trivial_response <- function(x) {
  str_detect(str_squish(tolower(x)),
             "^(n/?a|na|none|no|nope|nothing|not applicable|no comments?|-+|\\.+)[.!]?$")
}


# =============================================================================
# 5. RENDERING (shared by run_reports.R and the Shiny app)
# =============================================================================

#' Render one report. The .Rmd and Word template are copied to a fresh
#' temporary folder first, so renders never collide and the project folder
#' can be read-only (as it is on a hosting server).
render_report <- function(rmd, output_file, out_dir, project_dir, data_rds, params = list()) {
  work <- tempfile("render_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  file.copy(file.path(project_dir, "reports", c(rmd, "reference.docx")), work)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  rmarkdown::render(
    input = file.path(work, rmd),
    output_file = output_file, output_dir = out_dir,
    intermediates_dir = work, knit_root_dir = work,
    params = c(list(project_dir = project_dir, data_rds = data_rds), params),
    envir = new.env(parent = globalenv()), quiet = TRUE
  )
}

#' Render the full set: functioning report, methods companion, feedback
#' report and one applicant file per response. `which` picks a subset;
#' `progress(i, n, label)` is called before each file (used by the app).
render_all_reports <- function(dat, out_dir, project_dir,
                               which = c("functioning", "methods", "feedback", "applicants"),
                               response_ids = dat$responses$ResponseId,
                               progress = function(i, n, label) message("  ", label)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  data_rds <- file.path(out_dir, "prepared_data.rds")
  saveRDS(dat, data_rds)
  stamp <- format(Sys.Date(), "%Y-%m-%d")
  jobs <- list()
  if ("functioning" %in% which) jobs[[length(jobs) + 1]] <- list(
    rmd = "survey_functioning.Rmd", file = paste0("Survey_Functioning_", stamp, ".docx"),
    dir = out_dir, params = list(), label = "Survey functioning report")
  if ("methods" %in% which) jobs[[length(jobs) + 1]] <- list(
    rmd = "survey_functioning_methods.Rmd", file = paste0("Survey_Functioning_Methods_", stamp, ".docx"),
    dir = out_dir, params = list(), label = "Methods and code report")
  if ("feedback" %in% which) jobs[[length(jobs) + 1]] <- list(
    rmd = "feedback_report.Rmd", file = paste0("Open_Ended_Feedback_", stamp, ".docx"),
    dir = out_dir, params = list(), label = "Open-ended feedback report")
  if ("applicants" %in% which) {
    r <- dat$responses %>% filter(ResponseId %in% response_ids)
    for (i in seq_len(nrow(r))) jobs[[length(jobs) + 1]] <- list(
      rmd = "applicant_report.Rmd", file = applicant_file_name(r$ResponseId[i], r$provider[i]),
      dir = file.path(out_dir, "applicants"), params = list(response_id = r$ResponseId[i]),
      label = paste("Applicant", r$ResponseId[i]))
  }
  done <- character()
  for (k in seq_along(jobs)) {
    j <- jobs[[k]]
    progress(k, length(jobs), j$label)
    tryCatch({
      render_report(j$rmd, j$file, j$dir, project_dir, data_rds, j$params)
      done <- c(done, file.path(j$dir, j$file))
    }, error = function(e) message("  ! ", j$label, ": ", conditionMessage(e)))
  }
  invisible(done)
}
