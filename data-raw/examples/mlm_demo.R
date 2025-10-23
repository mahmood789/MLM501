suppressWarnings(suppressMessages({
  library(MLM501)
  library(metafor)
  library(clubSandwich)
}))

df <- read_mlm_effects()
coh <- get_cohort(df, outcome = "DICH", measure = "logOR")
q99 <- stats::quantile(coh, 0.99, na.rm = TRUE, names = FALSE)
cap <- max(10, q99)
coh <- coh[coh <= cap, , drop = FALSE]

yi <- coh; vi <- coh^2
fit <- metafor::rma.mv(yi = yi, V = vi,
  random = list(~1 | review_id/analysis_id, ~1 | study_id),
  data = coh, method = "REML")
rob <- clubSandwich::coef_test(fit, vcov = "CR2", cluster = coh)

out_dir <- file.path("inst","extdata"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
sum_file <- file.path(out_dir, "mlm_demo_results.csv")
utils::write.csv(data.frame(term = rownames(rob), estimate = rob, SE = rob, t = rob, p = rob), sum_file, row.names = FALSE)
message("Wrote ", sum_file)

