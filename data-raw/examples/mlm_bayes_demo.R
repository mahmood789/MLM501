suppressWarnings(suppressMessages({
  library(MLM501)
}))

# Attempt to load brms; if unavailable, write a note and exit gracefully
if (!requireNamespace("brms", quietly = TRUE)) {
  dir.create(file.path("inst","extdata"), showWarnings = FALSE, recursive = TRUE)
  note_file <- file.path("inst","extdata","mlm_bayes_demo_NOTE.txt")
  writeLines(c(
    "brms not installed; skipping Bayesian demo.",
    "Install with: install.packages('brms') (requires rstan toolchain)") , note_file)
  message("Wrote ", note_file)
  quit(save = "no")
}

# Prepare cohort
coh <- read_mlm_effects()
coh <- get_cohort(coh, outcome = "DICH", measure = "logOR")
# Cap extreme uncertainties for stability in demo
q99 <- stats::quantile(coh, 0.99, na.rm = TRUE, names = FALSE)
cap <- max(10, q99)
coh <- coh[coh <= cap & is.finite(coh) & is.finite(coh) & coh > 0, , drop = FALSE]

# Bayesian multilevel model
form <- brms::bf(TE | se(seTE, sigma = TRUE) ~ 1 + (1|review_id/analysis_id) + (1|study_id))
fit <- brms::brm(
  formula = form,
  data = coh,
  family = brms::gaussian(),
  cores = max(1, parallel::detectCores(logical = FALSE)),
  chains = 2,
  iter = 2000,
  control = list(adapt_delta = 0.95)
)

# Save summary artifacts
out_dir <- file.path("inst","extdata")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

sum_txt <- capture.output(print(summary(fit)))
writeLines(sum_txt, file.path(out_dir, "mlm_bayes_demo_summary.txt"))

fe <- as.data.frame(brms::fixef(fit))
utils::write.csv(fe, file.path(out_dir, "mlm_bayes_demo_fixef.csv"), row.names = FALSE)

message("Wrote Bayesian demo summaries to inst/extdata/")

