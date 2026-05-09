# Getting started with mach4ml

This guide walks through installing `mach4ml`, loading MACH-1 text exports, and running the recommended multi-file modulus workflow.

## 1. Install the package

Install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("elkadi/mach4ml")
```

Load the package:

```r
library(mach4ml)
```

`mach4ml` depends on R >= 3.6.0 and imports `pracma` for Savitzky-Golay smoothing.

## 2. Prepare your MACH-1 files

Create a folder containing the MACH-1 `.txt` exports you want to analyze. The high-level functions use all files matching:

```r
Sys.glob(paste0(folder_path, "\\*.txt"))
```

Because the current implementation uses a Windows-style path separator in this pattern, Windows paths are the most directly supported for multi-file workflows. On macOS or Linux, use paths carefully and test the file list first.

```r
folder <- "C:/Users/you/project/mach1_exports"
Sys.glob(paste0(folder, "\\*.txt"))
```

Each file name becomes the sample name after `.txt` is removed.

## 3. Run the recommended multi-file workflow

`calculate_moduli_multifiles2()` is the recommended high-level function because it includes baseline normalization and lets you control how many points are averaged for equilibrium-force extraction.

```r
folder <- "C:/Users/you/project/mach1_exports"

thicknesses <- c(2.0, 1.9, 1.8)   # sample thicknesses in mm
strains <- c(0.05, 0.10, 0.15)    # applied strain values for steps 1-3
indenter_radius <- 1.5            # indenter radius in mm

result <- calculate_moduli_multifiles2(
  folder = folder,
  Thicknesses = thicknesses,
  Strain = strains,
  indenterradius = indenter_radius,
  smfl = 19,
  EqDataPoints = 201
)
```

The result is a list with three elements:

```r
moduli <- result[[1]]      # calculated modulus summary table
forces <- result[[2]]      # cleaned force data in newtons
forces_sm <- result[[3]]   # smoothed cleaned force data in newtons
```

The summary table has these columns:

| Column | Meaning |
| --- | --- |
| `Folder` | Name of the input folder. |
| `File` | Sample/file identifier. |
| `EqModulus` | Equilibrium modulus calculated from unsmoothed cleaned force data. |
| `EqModulus_HC` | Hayes-corrected equilibrium modulus from unsmoothed cleaned force data. |
| `EqModulus_sm` | Equilibrium modulus calculated from smoothed cleaned force data. |
| `EqModulus_HC_Sm` | Hayes-corrected equilibrium modulus from smoothed cleaned force data. |

## 4. Inspect intermediate data

To inspect extraction and preprocessing manually:

```r
sr <- Extract_Stress_Relaxation_MultiFiles(folder)

forces_raw <- sr[[1]]
displacement <- sr[[2]]
step_indices <- sr[[3]]

str(forces_raw)
str(displacement)
step_indices
```

`step_indices` contains the column indices where the stress-relaxation steps begin. For a three-step protocol:

```r
S1i <- step_indices[1]
S2i <- step_indices[2]
S3i <- step_indices[3]
```

## 5. Plot a sample force curve

After extraction, rows are samples and columns are time/data points. You can plot one sample:

```r
plot(
  as.numeric(forces_sm[1, ]),
  type = "l",
  xlab = "Data point",
  ylab = "Force (N)",
  main = rownames(forces_sm)[1]
)
abline(v = step_indices, lty = 2)
```

## 6. Save results

```r
write.csv(moduli, file = "mach4ml_moduli.csv", row.names = FALSE)
write.csv(forces, file = "mach4ml_forces_clean.csv")
write.csv(forces_sm, file = "mach4ml_forces_smoothed.csv")
```

## Recommended next steps

- Read [Data format](data-format.md) to confirm the expected MACH-1 text structure.
- Read [Workflows](workflows.md) for manual processing options.
- Read [API reference](api-reference.md) for all exported functions.
- Read [Troubleshooting](troubleshooting.md) if extraction returns empty data or errors.
