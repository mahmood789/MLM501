# Read-only importer: builds a single multilevel effects table from Cochrane Pairwise .rda files
in_path <- "C:/Users/user/OneDrive - NHS/Documents/Pairwise70/data"
out_dir <- file.path("inst","extdata")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

derive_smd <- TRUE  # set FALSE to prefer MD over SMD for continuous outcomes

normalize_names <- function(x) {
  nm <- tolower(gsub("[^a-zA-Z0-9]+","_",names(x)))
  nm <- gsub("_+","_", nm); nm <- sub("^_","", nm)
  names(x) <- nm; x
}
pick <- function(df, cands) { h <- intersect(cands, names(df)); if (length(h)) df[[h[1]]] else NULL }

rows <- list()
rda_files <- list.files(in_path, pattern="\\.rda$", full.names=TRUE)
for (f in rda_files) {
  e <- new.env(parent=emptyenv()); try(load(f, envir=e), silent=TRUE)
  nms <- ls(e); if (!length(nms)) next
  obj <- get(nms[1], envir=e)
  if (!is.data.frame(obj)) next
  df <- normalize_names(obj)

  review_doi <- pick(df, "review_doi")
  review_url <- pick(df, "review_url")
  analysis_group <- pick(df, "analysis_group")
  analysis_number <- pick(df, "analysis_number")
  analysis_name <- pick(df, "analysis_name")
  subgroup <- pick(df, "subgroup")
  subgroup_number <- pick(df, "subgroup_number")
  study <- pick(df, c("study","study_label"))
  study_year <- pick(df, "study_year")

  giv_mean <- pick(df, "giv_mean")
  giv_se   <- pick(df, "giv_se")
  mean_    <- pick(df, "mean")
  ci_lo    <- pick(df, c("ci_start","ci_low","lcl"))
  ci_hi    <- pick(df, c("ci_end","ci_high","ucl"))

  exp_cases <- pick(df, "experimental_cases")
  exp_n     <- pick(df, "experimental_n")
  con_cases <- pick(df, "control_cases")
  con_n     <- pick(df, "control_n")

  exp_mean  <- pick(df, "experimental_mean")
  exp_sd    <- pick(df, c("experimental_sd","experimental_se"))
  con_mean  <- pick(df, "control_mean")
  con_sd    <- pick(df, c("control_sd","control_se"))

  rev_id <- sub("_data$", "", sub("_pub[0-9]+_data$", "", tools::file_path_sans_ext(basename(f))))

  for (i in seq_len(nrow(df))) {
    TE <- seTE <- NA_real_; outcome_type <- measure <- NA_character_
    # Generic summaries: GIV.Mean/SE
    if (!is.null(giv_mean) && !is.na(giv_mean[i]) && !is.null(giv_se) && !is.na(giv_se[i])) {
      TE <- as.numeric(giv_mean[i]); seTE <- as.numeric(giv_se[i]); outcome_type <- "GENSUM"; measure <- "GIV"
    } else if (!is.null(mean_) && !is.na(mean_[i]) && !is.null(ci_lo) && !is.null(ci_hi) &&
               is.finite(as.numeric(ci_lo[i])) && is.finite(as.numeric(ci_hi[i]))) {
      TE <- as.numeric(mean_[i]); seTE <- (as.numeric(ci_hi[i]) - as.numeric(ci_lo[i]))/(2*1.96)
      outcome_type <- "GENSUM"; measure <- "GEN_CI"
    } else if (!is.null(exp_cases) && !is.na(exp_cases[i]) && !is.null(exp_n) && !is.na(exp_n[i]) &&
               !is.null(con_cases) && !is.na(con_cases[i]) && !is.null(con_n) && !is.na(con_n[i])) {
      # Dichotomous: logOR with continuity correction if any zero cells
      a <- as.numeric(exp_cases[i]); b <- as.numeric(exp_n[i]) - a
      c <- as.numeric(con_cases[i]); d <- as.numeric(con_n[i]) - c
      if (!all(is.finite(c(a,b,c,d)))) next
      if (a==0 || b==0 || c==0 || d==0) { a <- a+0.5; b <- b+0.5; c <- c+0.5; d <- d+0.5 }
      TE <- log((a/b)/(c/d)); seTE <- sqrt(1/a + 1/b + 1/c + 1/d)
      outcome_type <- "DICH"; measure <- "logOR"
    } else if (!is.null(exp_mean) && !is.na(exp_mean[i]) && !is.null(exp_sd) && !is.na(exp_sd[i]) &&
               !is.null(exp_n) && !is.na(exp_n[i]) && !is.null(con_mean) && !is.na(con_mean[i]) &&
               !is.null(con_sd) && !is.na(con_sd[i]) && !is.null(con_n) && !is.na(con_n[i])) {
      # Continuous: SMD (Hedges g) or MD
      me <- as.numeric(exp_mean[i]); mc <- as.numeric(con_mean[i])
      sde <- as.numeric(exp_sd[i]);   sdc <- as.numeric(con_sd[i])
      ne <- as.numeric(exp_n[i]);     nc <- as.numeric(con_n[i])
      if (!all(is.finite(c(me, mc, sde, sdc, ne, nc))) || ne <= 1 || nc <= 1) next
      if (isTRUE(derive_smd)) {
        sd_pooled <- sqrt( ((ne - 1) * sde^2 + (nc - 1) * sdc^2) / (ne + nc - 2) )
        if (!is.finite(sd_pooled) || sd_pooled <= 0) next
        d <- (me - mc) / sd_pooled
        J <- 1 - 3 / (4 * (ne + nc) - 9)
        g <- J * d
        var_g <- ((ne + nc) / (ne * nc)) + (g^2 / (2 * (ne + nc - 2)))
        TE <- g; seTE <- sqrt(var_g)
        outcome_type <- "CONT"; measure <- "SMD"
      } else {
        TE <- me - mc
        seTE <- sqrt( (sde^2 / ne) + (sdc^2 / nc) )
        outcome_type <- "CONT"; measure <- "MD"
      }
    } else { next }

    if (!is.finite(TE) || !is.finite(seTE) || seTE <= 0) next

    rows[[length(rows)+1]] <- data.frame(
      review_id = rev_id,
      analysis_id = paste0(rev_id,"_G", suppressWarnings(as.integer(analysis_group[i])), "_N", suppressWarnings(as.integer(analysis_number[i]))),
      analysis_group = if (!is.null(analysis_group)) as.integer(analysis_group[i]) else NA_integer_,
      analysis_number = if (!is.null(analysis_number)) as.integer(analysis_number[i]) else NA_integer_,
      analysis_name = if (!is.null(analysis_name)) as.character(analysis_name[i]) else NA,
      subgroup = if (!is.null(subgroup)) as.character(subgroup[i]) else NA,
      subgroup_number = if (!is.null(subgroup_number)) as.integer(subgroup_number[i]) else NA_integer_,
      study_id = if (!is.null(study)) as.character(study[i]) else NA,
      study_year = if (!is.null(study_year)) as.integer(study_year[i]) else NA_integer_,
      outcome_type = outcome_type,
      measure = measure,
      TE = TE, seTE = seTE,
      review_doi = if (!is.null(review_doi)) as.character(review_doi[i]) else NA,
      review_url = if (!is.null(review_url)) as.character(review_url[i]) else NA,
      stringsAsFactors = FALSE
    )
  }
}
if (!length(rows)) stop("No effects derived.")
mlm <- do.call(rbind, rows)
utils::write.csv(mlm, file.path(out_dir, "mlm_effects.csv"), row.names = FALSE)
cat("Wrote", file.path(out_dir, "mlm_effects.csv"), "\n")
# simple catalogue by review/analysis
catg <- aggregate(list(n_effects = mlm$TE), by = list(review_id = mlm$review_id, analysis_id = mlm$analysis_id), FUN = length)
utils::write.csv(catg, file.path(out_dir, "catalogue.csv"), row.names = FALSE)
cat("Wrote", file.path(out_dir, "catalogue.csv"), "\n")
