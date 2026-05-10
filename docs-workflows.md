# Workflows

This guide shows common ways to use `mach4ml`.

## Recommended high-level workflow

Use `calculate_moduli_multifiles2()` when you have a folder of MACH-1 `.txt` files from the same stress-relaxation protocol.

```r
library(mach4ml)

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
forces_clean <- result[[2]]
forces_smooth <- result[[3]]
```

## Manual extraction and preprocessing

Use the lower-level functions when you want to inspect or modify each step.

```r
sr <- Extract_Stress_Relaxation_MultiFiles(folder)
forces_raw <- sr[[1]]
displacement <- sr[[2]]
step_indices <- sr[[3]]

forces_n <- forces_raw * 9.81 * 0.001
forces_n <- NormalizeBaseline(forces_n, dpi = 10)
forces_sm <- smooth_SR(forces_n, smfl = 19)
forces_sm <- Rmv_negative_peaks(
  forces_sm,
  S1i = step_indices[1],
  S2i = step_indices[2],
  S3i = step_indices[3]
)
```

## Extract stress-relaxation parameters

```r
params <- Extract_SR_Parameters2(
  SRmergedfile = forces_sm,
  Step2Index = step_indices[2],
  Step3Index = step_indices[3],
  EqDataPoints = 201
)

Se1 <- params[[1]]
Se2 <- params[[2]]
Se3 <- params[[3]]
Sp1 <- params[[4]]
Sp2 <- params[[5]]
Sp3 <- params[[6]]
Sm1 <- params[[7]]
Sm2 <- params[[8]]
Sm3 <- params[[9]]
```

## Calculate equilibrium modulus manually

```r
indenter_radius <- 1.5
indentation_area <- pi * indenter_radius^2
strains <- c(0.05, 0.10, 0.15)

eq_modulus <- calculate_Eq_Modulus(
  Se1 = Se1,
  Se2 = Se2,
  Se3 = Se3,
  Indentation_area = indentation_area,
  Strains = strains
)

eq_modulus_hayes <- Hayes_correction(
  Modulus = eq_modulus,
  indenterradius = indenter_radius,
  Thicknesses = c(2.0, 1.9, 1.8),
  v = 0.5
)
```

## Extract a single section from one file

```r
file <- "path/to/sample.txt"
find_contact <- Extract_Find_Contact(file)
wait_data <- Extract_Wait(file)
stress_relaxation <- Extract_Stress_Relaxation(file)
```

## Plot processed curves

```r
plot(
  as.numeric(forces_smooth[1, ]),
  type = "l",
  xlab = "Data point",
  ylab = "Force (N)",
  main = rownames(forces_smooth)[1]
)
abline(v = step_indices, lty = 2)
```

## Save outputs

```r
write.csv(moduli, "mach4ml_moduli.csv", row.names = FALSE)
write.csv(forces_clean, "mach4ml_forces_clean.csv")
write.csv(forces_smooth, "mach4ml_forces_smooth.csv")
```
