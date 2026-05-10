# Getting started with mach4ml

This guide walks through installing `mach4ml`, loading MACH-1 text exports, and running the recommended multi-file modulus workflow.

## Installation

```r
# install.packages("remotes")
remotes::install_github("elkadi/mach4ml")
library(mach4ml)
```

`mach4ml` requires R >= 3.6.0 and imports `pracma`.

## Prepare files

Place MACH-1 `.txt` exports in one folder. The high-level workflows use all files matching `*.txt`; file names become sample names.

## Recommended workflow

```r
folder <- "path/to/mach1_txt_files"
thicknesses <- c(2.0, 1.9, 1.8)
strains <- c(0.05, 0.10, 0.15)
indenter_radius <- 1.5

result <- calculate_moduli_multifiles2(
  folder = folder,
  Thicknesses = thicknesses,
  Strain = strains,
  indenterradius = indenter_radius,
  smfl = 19,
  EqDataPoints = 201
)

moduli <- result[[1]]
forces <- result[[2]]
forces_sm <- result[[3]]
```

`result[[1]]` contains the modulus summary; `result[[2]]` contains cleaned force data; `result[[3]]` contains smoothed cleaned force data.
