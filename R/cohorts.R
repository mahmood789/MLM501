#' Select a coherent cohort for analysis
#' @param df effects table from read_mlm_effects()
#' @param outcome one of c("DICH","CONT","GENSUM")
#' @param measure character (e.g., "logOR","MD","GIV","GEN_CI")
#' @param drop_na logical: drop rows with NA TE or seTE
#' @return filtered data.frame
#' @export
get_cohort <- function(df, outcome, measure, drop_na = TRUE) {
  stopifnot(is.data.frame(df))
  coh <- subset(df, outcome_type %in% outcome & measure %in% measure)
  if (drop_na) {
    coh <- subset(coh, is.finite(TE) & is.finite(seTE) & seTE > 0)
  }
  coh
}
