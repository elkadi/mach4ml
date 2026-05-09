# Workflows

This page documents common `mach4ml` workflows, from single-section extraction to complete modulus calculation.

## Workflow 1: Extract one section from one file

Use these helpers when you want to inspect a specific MACH-1 section.

### Find Contact

```r
fc <- Extract_Find_Contact("sample01.txt", y = 11)
head(fc)
```

### Wait

```r
wait <- Extract_Wait("sample01.txt", y = 4)
head(wait)
```

### Stress relaxation

```r
sr <- Extract_Stress_Relaxation("sample01.txt", y = 12)
head(sr)
```

`sr` contains the extracted stress-relaxation rows from the three stress-relaxation cycles, with columns:

```text
Time, z, x, y, Fz, Step
```

## Workflow 2: Extract and merge stress-relaxation data across files

```r
folder <- "C:/Users/you/project/mach1_exports"

sr_multi <- Extract_Stress_Relaxation_MultiFiles(folder)
forces_raw <- sr_multi[[1]]
displacement <- sr_multi[[2]]
step_indices <- sr_multi[[3]]
```

The multi-file extraction returns rows as samples and columns as time/data points.

```r
dim(forces_raw)
rownames(forces_raw)
step_indices
```

## Workflow 3: Manual preprocessing

The high-level functions perform preprocessing automatically, but manual preprocessing is useful for quality control.

```r
# Convert to newtons if raw force units match the package assumption.
forces_n <- forces_raw * 9.81 * 0.001

# Normalize row-wise baseline using the first 10 data points.
forces_norm <- NormalizeBaseline(forces_n, dpi = 10)

# Smooth force curves with a Savitzky-Golay filter.
forces_sm <- smooth_SR(forces_norm, smfl = 19)

# Remove negative peaks around the stress-relaxation steps.
forces_clean <- Rmv_negative_peaks(
  forces_sm,
  S1i = step_indices[1],
  S2i = step_indices[2],
  S3i = step_indices[3]
)
```

Optional abnormal-peak cleaning:

```r
forces_clean <- Rmv_abnormal_peaks(
  forces_clean,
  S1i = step_indices[1],
  S2i = step_indices[2],
  S3i = step_indices[3],
  dp = 200,
  threshold = 1.4
)
```

## Workflow 4: Extract stress-relaxation parameters

For a three-step protocol with the default equilibrium window:

```r
params <- Extract_SR_Parameters(
  SRmergedfile = forces_clean,
  Step2Index = step_indices[2],
  Step3Index = step_indices[3]
)
```

Returned values are:

```r
Se1 <- params[[1]]  # equilibrium force, step 1
Se2 <- params[[2]]  # equilibrium force, step 2
Se3 <- params[[3]]  # equilibrium force, step 3
Sp1 <- params[[4]]  # peak force, step 1
Sp2 <- params[[5]]  # peak force, step 2
Sp3 <- params[[6]]  # peak force, step 3
Sm1 <- params[[7]]  # pre-peak force, step 1
Sm2 <- params[[8]]  # pre-peak force, step 2
Sm3 <- params[[9]]  # pre-peak force, step 3
```

For a configurable equilibrium window, use `Extract_SR_Parameters2()`:

```r
params <- Extract_SR_Parameters2(
  SRmergedfile = forces_clean,
  Step2Index = step_indices[2],
  Step3Index = step_indices[3],
  EqDataPoints = 201
)
```

For four-step data, pass `Step4Index`:

```r
params4 <- Extract_SR_Parameters2(
  SRmergedfile = forces_clean,
  Step2Index = step_indices[2],
  Step3Index = step_indices[3],
  Step4Index = step_indices[4],
  EqDataPoints = 201
)
```

## Workflow 5: Calculate equilibrium modulus manually

```r
indenterradius <- 1.5
indentation_area <- pi * indenterradius^2
strains <- c(0.05, 0.10, 0.15)

eq_modulus <- calculate_Eq_Modulus(
  Se1 = params[[1]],
  Se2 = params[[2]],
  Se3 = params[[3]],
  Indentation_area = indentation_area,
  Strains = strains
)
```

Apply Hayes correction:

```r
thicknesses <- c(2.0, 1.9, 1.8)

eq_modulus_hc <- Hayes_correction(
  Modulus = eq_modulus,
  indenterradius = indenterradius,
  Thicknesses = thicknesses,
  v = 0.5
)
```

## Workflow 6: Use the high-level modulus pipeline

For most users, the high-level function is simpler:

```r
result <- calculate_moduli_multifiles2(
  folder = folder,
  Thicknesses = c(2.0, 1.9, 1.8),
  Strain = c(0.05, 0.10, 0.15),
  indenterradius = 1.5,
  smfl = 19,
  EqDataPoints = 201
)

moduli <- result[[1]]
forces_clean <- result[[2]]
forces_smoothed <- result[[3]]
```

## Workflow 7: Quality-control plots

Plot raw, normalized, and smoothed data for one sample:

```r
sample_i <- 1

plot(as.numeric(forces_raw[sample_i, ]), type = "l",
     xlab = "Data point", ylab = "Raw force",
     main = rownames(forces_raw)[sample_i])
abline(v = step_indices, lty = 2)

plot(as.numeric(forces_clean[sample_i, ]), type = "l",
     xlab = "Data point", ylab = "Cleaned force (N)",
     main = rownames(forces_clean)[sample_i])
abline(v = step_indices, lty = 2)
```

## Choosing important parameters

| Parameter | Used by | Guidance |
| --- | --- | --- |
| `y` | extraction functions | Number of header lines to skip after a section tag. Adjust if your MACH-1 export layout differs. |
| `dpi` | `NormalizeBaseline()` | Number of initial data points used to estimate baseline. |
| `smfl` | `smooth_SR()` and high-level pipelines | Savitzky-Golay filter length. Larger values smooth more but may flatten sharp features. |
| `EqDataPoints` | `Extract_SR_Parameters2()` and `calculate_moduli_multifiles2()` | Number of points used to average equilibrium force near the end of each step. |
| `threshold` | `Rmv_abnormal_peaks()` | Peak is replaced when the largest value is more than `threshold` times the second-largest value in the peak window. |
| `Thicknesses` | `Hayes_correction()` and high-level pipelines | Sample thickness per step. Use values from your experiment. |
| `Strain` | modulus calculations | Strain values corresponding to the protocol steps. |
| `indenterradius` | modulus calculations | Radius of the indenter, used to compute indentation area and Hayes correction. |

## Reproducible outputs

After running a high-level pipeline, save both the summary and intermediate data:

```r
write.csv(moduli, "moduli_summary.csv", row.names = FALSE)
write.csv(forces_clean, "forces_clean.csv")
write.csv(forces_smoothed, "forces_smoothed.csv")
```

Keeping intermediate force data makes it easier to re-check peak cleaning, smoothing, and step detection later.
