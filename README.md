# FCECPR Training Registration Form: reports

There are two ways to produce the same Word reports:

- **`run_reports.R`**: an R script that works from CSV files you export from Qualtrics.
- **`app.R`**: a Shiny app for people who don't code. It pulls responses from the Qualtrics API.

| Report | What it is for |
|---|---|
| Application review (one Word file per response) | Shows the answers, the required questions that are missing or not reached, upload details and automated checks. Each file is named `Application_<ResponseId>_<Provider>.docx`. |
| Survey functioning | Time to complete, completion and quit rates, where people stopped, and the response rate for each question. |
| Survey functioning: methods and code | Explains each table and chart in the functioning report in plain words, shows the exact code that made it, and shows its output. |
| Open-ended feedback | Verbatim comments from the Feedback section, grouped by question and anonymized. |

Each response is its own entry, identified by its Qualtrics **ResponseId**.

---

## Option 1: the R script (`run_reports.R`)

1. Export from Qualtrics twice (Data & Analysis > Export & Import > Export Data > CSV): once with **Use numeric values**, and once with **Use choice text**. Put both CSVs in `data/`.
2. Keep the survey Word file in `survey/`. Re-export it whenever the form changes.
3. **Date range:** at the top of `run_reports.R`, set `date_from` and `date_to` (as `"YYYY-MM-DD"`, both days included). Leave them `NULL` for all dates. `date_field = "recorded"` filters on the date the response was recorded (submitted). Use `"start"` to filter on the date it was started instead.
4. Optional: to leave out test entries, list their ResponseIds in `data/exclude_response_ids.txt`, one per line.
5. Run `source("run_reports.R")`. The reports are saved in `output/<date>/`.

## Option 2: the Shiny app (`app.R`)

The people using it pick a date range, click a button and download the Word files.

### Try it on your computer first

1. Copy `.Renviron.example` to `.Renviron` and fill in your API token, data-center URL, survey ID and a password.
   - The data-center URL is the part of your Qualtrics address before `/`, for example `yourorg.ca1.qualtrics.com`. In Qualtrics it is under Account Settings > Qualtrics IDs.
   - The survey ID (it starts with `SV_`) is on the same page.
2. In RStudio, open `app.R` and click **Run App**.
3. To try the app without the API, set `DEMO_DATA_DIR=data` in `.Renviron` instead. The app then reads the CSVs in `data/`.

### Publish it on Posit Connect Cloud (free plan)

> **Important: on the free plan, published apps are public.** Anyone who has the link can reach the app's front page. Set `APP_PASSWORD` so that nothing loads until the password is entered. A shared password is basic protection. These reports contain applicants' names, emails, phone numbers and course log-ins. Before you use the app with real applications, either move to a plan with private sharing (sign-in by email), or confirm that a shared password meets your organization's data rules.

1. Put the project in a GitHub repository, **without** `data/`, `output/` or `.Renviron`. The `.rscignore` file lists the files to keep out.
2. In R, run `source("write_manifest.R")`. This creates `manifest.json`. Commit it.
3. In Connect Cloud, click **Publish**, choose **Shiny**, then choose your repository and `app.R`.
   - You can also use RStudio's **Publish** button, after adding your Connect Cloud account in RStudio.
4. In the content's settings, add the secret variables `QUALTRICS_API_KEY`, `QUALTRICS_BASE_URL`, `QUALTRICS_SURVEY_ID` and `APP_PASSWORD`. Then restart the app.

When the form changes, re-export the survey Word file into `survey/`, then commit and republish.

### How the app behaves

- **Get latest responses from Qualtrics** downloads every response twice through the API, once as codes and once as labels. The download is shared by everyone using the app and refreshed when someone clicks the button or after 30 minutes.
- The date boxes start out covering all responses. Every report uses only the dates selected.
- **More settings** has the same options as the script: response IDs to leave out, the quit cutoff, the long-duration cutoff and the response-rate flag.

---

## Settings (both versions)

- `quit_progress_cutoff`: a response counts as *Quit* when Finished = FALSE and Progress is below this value. When Finished = FALSE and Progress is at or above it, the response counts as *Not submitted (near complete)*.
- `duration_trim_method` and `duration_trim_hours`: completion times longer than the cutoff are left out of the averages, but are still listed.
- `low_response_threshold`: the rate below which a required question is flagged.
- `applicant_sections`: the order of the sections in the application files.

## When the form changes

Edit section 2 of `R/functions.R`:

- `applicability_rules()`: which questions are shown only after certain answers.
- `optional_questions`: questions that are never flagged as missing.
- `required_parts`: which parts of a multi-part question are required.
- `application_checks()`: the automated checks in the application files.

New questions, blocks and answer choices are picked up automatically from the export and the survey Word file.

## Files

```
run_reports.R                  script version: settings + run
app.R                          Shiny app
R/functions.R                  reading data (CSV or API), rules, checks, rendering
R/functioning_calcs.R          every table and figure in the functioning report
R/_disable_autoload.R          stops Shiny from auto-running the files in R/
reports/*.Rmd                  the four report templates
reports/reference.docx         Word styles
survey/                        survey Word file (structure, answer labels)
data/                          your CSV exports (script version only; never publish)
output/                        reports made by the script
write_manifest.R, .rscignore   publishing helpers
.Renviron.example              template for API settings
```
