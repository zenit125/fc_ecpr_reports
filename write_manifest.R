# Creates manifest.json, which Posit Connect Cloud needs when you publish the
# app from a GitHub repository. Run it again whenever you update packages.
# Only the files the app needs are listed: no data, no output, no secrets.
if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
for (p in c("shiny", "bslib", "DT", "zip", "qualtRics", "rmarkdown", "knitr", "dplyr",
            "tidyr", "purrr", "stringr", "readr", "lubridate", "ggplot2", "flextable",
            "officer", "xml2")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
rsconnect::writeManifest(
  appDir = ".",
  appFiles = c(
    "app.R",
    "R/functions.R", "R/functioning_calcs.R", "R/_disable_autoload.R",
    list.files("reports", full.names = TRUE),
    list.files("survey", full.names = TRUE)
  )
)
message("manifest.json written. Commit it to GitHub together with the files it lists.")
