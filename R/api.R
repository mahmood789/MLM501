#' Read the multilevel effects table
#' @return data.frame with columns review_id, analysis_id, study_id, TE, seTE, etc.
#' @export
read_mlm_effects <- function() {
  p <- system.file("extdata", "mlm_effects.csv", package = utils::packageName())
  if (!nzchar(p)) p <- file.path("inst", "extdata", "mlm_effects.csv")
  if (!file.exists(p)) stop("mlm_effects.csv not found. Run data-raw/import_cochrane501.R.")
  utils::read.csv(p, stringsAsFactors = FALSE)
}
