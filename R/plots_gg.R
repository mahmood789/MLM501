#' Forest-style plot with ggplot2
#' @param df data.frame with columns TE, seTE and optional id
#' @param top_n integer: show top N by precision (default: 50)
#' @param title character: plot title
#' @export
plot_effects_gg <- function(df, top_n = 50, title = "Effects (TE \u00b1 1.96*SE)") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 not installed. Install with install.packages('ggplot2').")
  }
  stopifnot(is.data.frame(df), all(c("TE","seTE") %in% names(df)))
  d <- df
  d$lower <- d$TE - 1.96 * d$seTE
  d$upper <- d$TE + 1.96 * d$seTE
  d <- d[order(d$seTE, decreasing = FALSE), , drop = FALSE]
  if (nrow(d) > top_n) d <- d[seq_len(top_n), , drop = FALSE]
  d$idx <- factor(seq_len(nrow(d)), levels = rev(seq_len(nrow(d))))
  lbl <- if ("id" %in% names(d)) d$id else d$idx

  ggplot2::ggplot(d, ggplot2::aes(x = TE, y = idx)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), height = 0) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
    ggplot2::scale_y_discrete(labels = rev(lbl)) +
    ggplot2::labs(x = "TE", y = "Effect index", title = title) +
    ggplot2::theme_minimal()
}
