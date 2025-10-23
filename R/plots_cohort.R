#' Plot a cohort quickly (base R)
#' @param df effects table (from read_mlm_effects())
#' @param outcome one of c("DICH","CONT","GENSUM")
#' @param measure e.g., "logOR","MD","SMD","GIV","GEN_CI"
#' @param main plot title
#' @export
plot_cohort_quick <- function(df, outcome, measure, main = NULL) {
  coh <- get_cohort(df, outcome = outcome, measure = measure)
  coh <- subset(coh, is.finite(TE) & is.finite(seTE) & seTE > 0)
  if (is.null(main)) main <- sprintf("Cohort: %s / %s", outcome, measure)
  plot_effects_quick(coh, main = main)
}

#' Plot a cohort with ggplot2 (forest-style)
#' @param df effects table (from read_mlm_effects())
#' @param outcome one of c("DICH","CONT","GENSUM")
#' @param measure e.g., "logOR","MD","SMD","GIV","GEN_CI"
#' @param top_n integer: only show the top-N most precise (smallest seTE)
#' @param title plot title
#' @export
plot_cohort_gg <- function(df, outcome, measure, top_n = 50, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 not installed. Install with install.packages('ggplot2').")
  }
  coh <- get_cohort(df, outcome = outcome, measure = measure)
  coh <- subset(coh, is.finite(TE) & is.finite(seTE) & seTE > 0)
  if (is.null(title)) title <- sprintf("Cohort: %s / %s", outcome, measure)
  plot_effects_gg(coh, top_n = top_n, title = title)
}
