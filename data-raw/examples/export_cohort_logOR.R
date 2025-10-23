## Export a coherent logOR cohort for quick demos
## Writes inst/extdata/cohort_logOR.csv

suppressWarnings(suppressMessages({ library(MLM501) }))

out_dir <- file.path('inst','extdata')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

df <- read_mlm_effects()
coh <- get_cohort(df, outcome = 'DICH', measure = 'logOR')
coh <- coh[is.finite(coh$TE) & is.finite(coh$seTE) & coh$seTE > 0, , drop = FALSE]
utils::write.csv(coh, file.path(out_dir, 'cohort_logOR.csv'), row.names = FALSE)
message('Wrote ', file.path(out_dir, 'cohort_logOR.csv'))

