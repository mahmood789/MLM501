
## Downsample and Plot (demo)

```r
# Downsample a large logOR cohort and plot quick effects
setwd("C:/Users/user/OneDrive - NHS/Documents/501MLM")
source("data-raw/examples/downsample_cohort.R")
coh_ds <- read.csv("inst/extdata/cohort_downsampled.csv")

# Base plotting
plot_effects_quick(coh_ds)

# ggplot2 plotting (if available)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- plot_effects_gg(coh_ds, top_n = 50, title = "Downsampled effects")
  print(p)
}
```
