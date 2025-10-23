options(repos = "https://cloud.r-project.org")
if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')
if (!requireNamespace('roxygen2', quietly = TRUE)) install.packages('roxygen2')

message('Running devtools::document() ...')
devtools::document()

message('Running devtools::check() ...')
res <- devtools::check(args = c('--no-manual'), cran = FALSE)
print(res)

if (length(res$errors)) stop('R CMD check reported errors')
if (length(res$warnings)) stop('R CMD check reported warnings (aiming zero).')
if (length(res$notes)) message('NOTE(s): ', paste(res$notes, collapse='; '))
message('devtools::check completed.')
