#' Quick effects plot (base R)
#' @param df data.frame with columns TE, seTE
#' @param main plot title
#' @export
plot_effects_quick <- function(df, main = "Effects (TE \u00b1 1.96*SE)") {
  stopifnot(is.data.frame(df), all(c("TE","seTE") %in% names(df)))
  y <- df$TE; se <- df$seTE; n <- length(y)
  if (!n) stop("No rows to plot")
  x <- seq_len(n)
  op <- par(mar = c(4,4,2,1)); on.exit(par(op), add = TRUE)
  plot(x, y, pch = 19, xlab = "Effect index", ylab = "TE", main = main)
  arrows(x0 = x, y0 = y - 1.96*se, x1 = x, y1 = y + 1.96*se, angle = 90, length = 0.02, code = 3)
  abline(h = 0, col = "gray", lty = 2)
}
