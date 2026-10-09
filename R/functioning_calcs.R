# =============================================================================
# Survey functioning report: the code behind every table and figure.
#
# Each block below starts with a "## ---- name ----" line. Both
# reports/survey_functioning.Rmd and reports/survey_functioning_methods.Rmd
# load this file with knitr::read_chunk() and run the blocks by name, so the
# methods report always shows exactly the code that produced the numbers.
#
# Objects available when these run (created in the Rmd setup chunk):
#   dat  - the prepared data from build_survey_data() (see R/functions.R)
# =============================================================================

## ---- fx-setup ----
s     <- dat$settings
resp  <- dat$responses                      # one row per response kept for analysis
n_all <- nrow(resp)
pct   <- function(x, d) ifelse(d == 0, "-", sprintf("%.0f%%", 100 * x / d))

# Time to complete: completed responses only
comp <- resp %>% filter(finished, !is.na(duration_sec))
use_iqr <- identical(s$duration_trim_method, "iqr") && nrow(comp) >= 4
trim_limit <- if (use_iqr) {
  q <- quantile(comp$duration_sec, c(.25, .75))     # R default (type 7) quantiles
  q[[2]] + 3 * diff(q)                              # upper quartile + 3 x IQR
} else {
  s$duration_trim_hours * 3600                      # fixed cutoff, in seconds
}
trim_rule <- if (use_iqr) {
  sprintf("longer than %s (upper quartile + 3 x IQR)", fmt_duration(trim_limit))
} else {
  sprintf("longer than %s hours", s$duration_trim_hours)
}
comp <- comp %>% mutate(trimmed = duration_sec > trim_limit)
kept <- comp %>% filter(!trimmed)                   # the set used for averages
med_min <- if (nrow(kept)) median(kept$duration_sec) / 60 else NA

# Response rates: one row per question, among respondents who reached it and
# to whom it applied (see build_question_status() in R/functions.R)
rates <- dat$qstatus %>%
  filter(applicable, reached) %>%
  group_by(block, order, tag, title, optional) %>%
  summarise(shown = n(), answered = sum(status == "Answered"), .groups = "drop") %>%
  mutate(rate = answered / shown) %>%
  arrange(order)
never_shown <- dat$questions %>% filter(!tag %in% rates$tag)
low <- rates %>% filter(rate < s$low_response_threshold, !optional)

## ---- fx-summary ----
counts <- resp %>% count(completion, .drop = FALSE)
get_n <- function(lbl) counts$n[counts$completion == lbl]
tibble::tribble(
  ~Measure, ~Value,
  "Date range", dat$date_filter$text,
  "Responses in the date range", as.character(dat$date_filter$n_before - dat$date_filter$n_outside),
  "Excluded (previews, tests, listed IDs)", as.character(nrow(dat$excluded)),
  "Responses analysed", as.character(n_all),
  "Completed (submitted)", sprintf("%d (%s)", get_n("Completed"), pct(get_n("Completed"), n_all)),
  "Not submitted, near complete", sprintf("%d (%s)", get_n("Not submitted (near complete)"),
                                           pct(get_n("Not submitted (near complete)"), n_all)),
  "Quit", sprintf("%d (%s)", get_n("Quit"), pct(get_n("Quit"), n_all)),
  "Median time to complete (completed responses)",
    if (is.na(med_min)) "-" else fmt_duration(med_min * 60),
  "Required questions answered by fewer than threshold",
    sprintf("%d (threshold %.0f%%)", nrow(low), 100 * s$low_response_threshold)
) %>%
  flextable() %>% ft_style() %>% delete_part("header") %>% bold(j = 1) %>%
  width(j = 1, width = 3.3) %>% width(j = 2, width = 3.2) %>% set_table_properties(layout = "fixed")

## ---- fx-definitions ----
cat("**Definitions.** *Completed* = Qualtrics Finished = TRUE. *Not submitted, near complete* = Finished = FALSE with Progress of at least ",
    s$quit_progress_cutoff, "%. *Quit* = Finished = FALSE with Progress below ", s$quit_progress_cutoff,
    "%. Qualtrics only exports an unfinished response after it is closed (by default one week after the last activity), so recent unfinished responses may not appear yet.\n\n", sep = "")
if (nrow(dat$excluded) > 0) {
  cat("Excluded responses: ",
      md_escape(paste(sprintf("%s (%s)", dat$excluded$ResponseId, dat$excluded$reason), collapse = "; ")),
      ".\n\n", sep = "")
}

## ---- fx-time-intro ----
cat(sprintf(paste0("Based on the %d completed responses. Qualtrics measures duration from first opening the form to submitting it, ",
                   "so it includes time the form sat open. %d response(s) %s are left out of the figures below and listed separately."),
            nrow(comp), sum(comp$trimmed), trim_rule), "\n\n")

## ---- fx-time-table ----
if (nrow(kept) > 0) {
  m <- kept$duration_sec
  tibble(
    `Responses` = length(m),
    `Median` = fmt_duration(median(m)),
    `Middle 50% (IQR)` = paste(fmt_duration(quantile(m, .25)), "to", fmt_duration(quantile(m, .75))),
    `Mean` = fmt_duration(mean(m)),
    `Shortest` = fmt_duration(min(m)),
    `Longest` = fmt_duration(max(m))
  ) %>% flextable() %>% ft_style() %>% align(align = "center", part = "all")
}

## ---- fx-time-hist ----
if (nrow(kept) > 1) {
  # bin width: about 15 bins across the range, rounded up to a multiple of 5 minutes
  bw <- max(5, ceiling(diff(range(kept$duration_sec / 60)) / 15 / 5) * 5)
  ggplot(kept, aes(duration_sec / 60)) +
    geom_histogram(binwidth = bw, boundary = 0, fill = report_blue, color = "white", linewidth = 0.6) +
    geom_vline(xintercept = med_min, linetype = "dashed", color = "#52514E") +
    annotate("text", x = med_min, y = Inf, label = paste("median", fmt_duration(med_min * 60)),
             hjust = -0.05, vjust = 1.5, size = 3, color = "#52514E") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Time to complete (completed responses)", x = "Minutes", y = "Responses") +
    theme_report()
}

## ---- fx-time-outliers-head ----
if (any(comp$trimmed)) cat("## Long completion times (left out of the figures above)\n\n")

## ---- fx-time-outliers ----
if (any(comp$trimmed)) {
  comp %>% filter(trimmed) %>% arrange(desc(duration_sec)) %>%
    transmute(`Response ID` = ResponseId, Duration = fmt_duration(duration_sec),
              Started = fmt_dt(start), Submitted = fmt_dt(recorded)) %>%
    flextable() %>% ft_style()
}

## ---- fx-time-unfinished ----
unf <- resp %>% filter(!finished, !is.na(duration_sec))
if (nrow(unf) > 0) {
  cat(sprintf("Unfinished responses (%d) spent a median of %s in the form before stopping (range %s to %s).\n\n",
              nrow(unf), fmt_duration(median(unf$duration_sec)),
              fmt_duration(min(unf$duration_sec)), fmt_duration(max(unf$duration_sec))))
}

## ---- fx-survival ----
# For every question position p: share of ALL analysed responses that either
# submitted or answered something at position p or later.
remaining <- dat$questions %>%
  select(tag, title, block, position, order) %>%
  mutate(still_answering = map_dbl(position, function(p)
    mean(resp$finished | resp$last_position >= p)))
block_starts <- remaining %>% group_by(block) %>% summarise(start = min(order), .groups = "drop")
ggplot(remaining, aes(order, still_answering)) +
  geom_vline(data = block_starts, aes(xintercept = start - 0.5), color = "#D4D3CE", linewidth = 0.4) +
  geom_step(color = report_blue, linewidth = 0.9, direction = "hv") +
  geom_text(data = block_starts, aes(x = start - 0.2, y = 0.03, label = block),
            angle = 90, hjust = 0, vjust = 1.2, size = 2.3, color = "#52514E") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1.02)) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  labs(title = "Share of respondents who got at least this far",
       x = "Question, in survey order", y = NULL) +
  theme_report() + theme(panel.grid.major.y = element_line(color = "#E6E6E3"))

## ---- fx-stop-intro ----
stops <- resp %>% filter(!finished)
cat(sprintf("%d response(s) were not submitted. The tables show the last question each one answered.%s\n\n",
            nrow(stops),
            if (nrow(stops)) " 'Last seen' is Qualtrics' own record of the last page shown, which can be later than the last answer." else ""))

## ---- fx-stop-section ----
if (nrow(stops) > 0) {
  stops %>%
    mutate(Section = coalesce(as.character(last_block), "(nothing answered)")) %>%
    count(Section, completion) %>%
    pivot_wider(names_from = completion, values_from = n, values_fill = 0) %>%
    mutate(Section = factor(Section, levels = c(dat$block_order, "(nothing answered)"))) %>%
    arrange(Section) %>%
    flextable() %>% ft_style() %>% set_header_labels(Section = "Last section answered") %>%
    align(j = -1, align = "center", part = "all")
}

## ---- fx-stop-question ----
if (nrow(stops) > 0) {
  stops %>%
    count(Section = as.character(last_block), `Last question answered` = last_title, name = "Responses") %>%
    arrange(factor(Section, levels = dat$block_order), desc(Responses)) %>%
    flextable() %>% ft_style() %>% align(j = 3, align = "center", part = "all") %>%
    width(j = 1, width = 1.8) %>% width(j = 2, width = 3.9) %>% width(j = 3, width = 0.8) %>%
    set_table_properties(layout = "fixed")
}

## ---- fx-stop-list-head ----
if (nrow(stops) > 0) cat("## Unfinished responses\n\n")

## ---- fx-stop-list ----
if (nrow(stops) > 0) {
  stops %>% arrange(progress) %>%
    transmute(`Response ID` = ResponseId, Status = as.character(completion),
              `Progress` = paste0(progress, "%"),
              `Last answered` = paste0(coalesce(as.character(last_block), "-"), ": ", coalesce(last_title, "-")),
              `Last seen (Qualtrics)` = coalesce(last_seen, "-"),
              `Time in form` = fmt_duration(duration_sec), Started = format(start, "%m/%d/%Y")) %>%
    flextable() %>% ft_style(font_size = 8.5) %>% ft_status_colors(j = "Status")
}

## ---- fx-progress-ref-head ----
cat("## Progress values by section\n\n",
    "Use this to choose the Progress cutoff for 'Quit' (currently ", s$quit_progress_cutoff,
    "%). It shows the Progress recorded for respondents whose last answer fell in each section.\n\n", sep = "")

## ---- fx-progress-ref ----
prog_ref <- resp %>% filter(!finished, !is.na(last_block))
if (nrow(prog_ref) > 0) {
  prog_ref %>%
    group_by(Section = last_block) %>%
    summarise(Responses = n(), `Lowest progress` = paste0(min(progress), "%"),
              `Highest progress` = paste0(max(progress), "%"), .groups = "drop") %>%
    arrange(Section) %>% mutate(Section = as.character(Section)) %>%
    flextable() %>% ft_style() %>% align(j = 2:4, align = "center", part = "all")
}

## ---- fx-rates-intro ----
cat("For each question, the response rate counts only respondents who **reached** it (answered it or something later, or submitted) and to whom it **applied** under the form's display logic (for example, *Fee per participant* applies only when *Fee* was selected). ",
    sprintf("Required questions below %.0f%% are flagged. Optional questions, including everything in the feedback section, are shown for information and are never flagged.\n\n",
            100 * s$low_response_threshold), sep = "")

## ---- fx-rates-low-head ----
cat("## Required questions below the threshold\n\n")
if (nrow(low) == 0) cat(styled("None.", "OK Flag"), "\n\n")

## ---- fx-rates-low ----
if (nrow(low) > 0) {
  low %>% arrange(rate) %>%
    transmute(Section = as.character(block), Question = title, `Shown to` = shown,
              Answered = answered, Rate = sprintf("%.0f%%", 100 * rate)) %>%
    flextable() %>% ft_style() %>% align(j = 3:5, align = "center", part = "all") %>%
    width(j = 1, width = 1.5) %>% width(j = 2, width = 2.9) %>% width(j = 3:5, width = 0.72) %>%
    set_table_properties(layout = "fixed") %>%
    color(j = 5, color = "#B3261E") %>% bold(j = 5)
}

## ---- fx-rates-plot ----
plot_rates <- rates %>%
  mutate(block = factor(str_wrap(as.character(block), 22),
                        levels = rev(unique(str_wrap(as.character(dat$questions$block), 22)))),
         kind = if_else(optional, "Optional", "Required"))
ggplot(plot_rates, aes(rate, block)) +
  geom_vline(xintercept = s$low_response_threshold, linetype = "dashed", color = "#B3261E", linewidth = 0.4) +
  # points are spread vertically only (seeded), so the rate itself is not altered
  geom_point(aes(shape = kind), position = position_jitter(height = 0.15, width = 0, seed = 1),
             size = 2.4, color = report_blue, alpha = 0.75, stroke = 0.8) +
  scale_shape_manual(values = c(Required = 16, Optional = 1), name = NULL) +
  scale_x_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1.02)) +
  labs(title = "Each point is one question", x = "Response rate among respondents shown the question", y = NULL) +
  theme_report() +
  theme(panel.grid.major.x = element_line(color = "#E6E6E3", linewidth = 0.3),
        panel.grid.major.y = element_blank())

## ---- fx-rates-all ----
all_tbl <- rates %>%
  transmute(Section = as.character(block),
            Question = if_else(optional, paste0(title, " (optional)"), title),
            `Shown to` = shown, Answered = answered,
            Rate = sprintf("%.0f%%", 100 * rate),
            flag = rate < s$low_response_threshold & !optional)
ft <- all_tbl %>% select(-flag) %>%
  flextable() %>% ft_style(font_size = 8.5) %>% merge_v(j = "Section") %>%
  align(j = 3:5, align = "center", part = "all") %>%
  width(j = 1, width = 1.4) %>% width(j = 2, width = 3.4) %>% width(j = 3:5, width = 0.6) %>%
  set_table_properties(layout = "fixed")
if (any(all_tbl$flag)) ft <- ft %>% color(i = which(all_tbl$flag), j = 5, color = "#B3261E") %>%
  bold(i = which(all_tbl$flag), j = 5)
ft

## ---- fx-never-shown ----
if (nrow(never_shown) > 0) {
  cat("Not shown to anyone in this period (display logic never triggered): ",
      md_escape(paste(never_shown$title, collapse = "; ")), ".\n\n", sep = "")
}

## ---- fx-over-time ----
resp %>%
  mutate(week = floor_date(start, "week", week_start = 1)) %>%   # weeks start on Monday
  count(week, completion) %>%
  ggplot(aes(as.Date(week), n, fill = completion)) +
  geom_col(width = 5, color = "white", linewidth = 0.4) +
  scale_fill_manual(values = c("Completed" = "#2A78D6", "Not submitted (near complete)" = "#EDA100",
                               "Quit" = "#E34948"), name = NULL, drop = FALSE) +
  scale_x_date(date_labels = "%b %d") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Responses started per week", x = "Week starting", y = "Responses") +
  theme_report()

## ---- fx-footer ----
cat(styled(sprintf("Data: %s. %s. Times shown in %s.",
                   paste(unlist(dat$files), collapse = ", "), dat$date_filter$text, s$display_tz),
           "Muted"), "\n")
