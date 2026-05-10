# mach4ml

`mach4ml` is an R package for extracting, cleaning, organizing, and analyzing Biomomentum MACH-1 text exports, with a focus on stress-relaxation experiments and high-throughput workflows that produce large data sets for downstream analysis or machine-learning pipelines.

The package provides helpers to:

- extract `<Find Contact>`, `<Wait>`, and `<Stress Relaxation>` sections from MACH-1 text files;
- merge stress-relaxation force and displacement data across all `.txt` files in a folder;
- clean common stress-relaxation artifacts such as baseline offsets, negative peaks, abnormal high peaks, and noise;
- extract equilibrium, peak, and pre-peak forces from 3-step or optional 4-step stress-relaxation protocols;
- calculate equilibrium and instantaneous elastic moduli;
- apply the Hayes et al. correction for disk-shaped specimens when Poisson's ratio is 0.5.

This package was inspired by MATLAB code from Mohammad Hossein Ebrahimi for similar MACH-1 analysis workflows: <https://github.com/MohammadHosseinEbrahimi/Equilibrium-and-instantaneous-moduli>.

## Installation

Install the package from GitHub:

```r
# install.packages("remotes")
remotes::install_github("elkadi/mach4ml")
```

Load the package:

```r
library(mach4ml)
```

## Requirements

`mach4ml` requires:

- R >= 3.6.0
- `pracma`

The package uses base R utilities plus selected functions from `stats`, `utils`, and `pracma`.

## Expected input data

The package is designed for text files exported from Biomomentum MACH-1. The extraction functions search for section tags such as:

- `<Find Contact>`
- `<Wait>`
- `<Stress Relaxation>`
- `<divider>`
- `<END DATA>`

Most functions assume tab-separated data with five columns corresponding to time, displacement/position axes, and force. Stress-relaxation data are typically returned with columns named:

```text
Time, z, x, y, Fz, Step
```

For multi-file workflows, place one or more MACH-1 `.txt` files in a single folder. File names are used as sample names.

See [Data format](docs-data-format.md) for details.

## Quick start: calculate moduli from a folder of MACH-1 files

```r
library(mach4ml)

folder <- "path/to/mach1_txt_files"

# Example values only. Use specimen-specific values from your experiment.
thicknesses <- c(2.0, 1.9, 1.8)   # mm, one value per stress-relaxation step
strains <- c(0.05, 0.10, 0.15)    # strain values used in the protocol
indenter_radius <- 1.5            # mm

result <- calculate_moduli_multifiles2(
  folder = folder,
  Thicknesses = thicknesses,
  Strain = strains,
  indenterradius = indenter_radius,
  poisson_eq = 0.5,
  poisson_inst = 0.5,
  smfl = 19,
  EqDataPoints = 201
)

moduli <- result[[1]]      # summary table of calculated moduli
forces <- result[[2]]      # cleaned force data, converted to N
forces_sm <- result[[3]]   # smoothed cleaned force data, converted to N

head(moduli)
```

`calculate_moduli_multifiles2()` is the recommended high-level function for most current workflows because it includes baseline normalization and allows the equilibrium averaging window to be adjusted.

## Typical workflow

For more control, run the processing steps manually:

```r
library(mach4ml)

folder <- "path/to/mach1_txt_files"

# 1. Extract and merge stress-relaxation data from all .txt files.
sr <- Extract_Stress_Relaxation_MultiFiles(folder)
forces_raw <- sr[[1]]
displacement <- sr[[2]]
step_indices <- sr[[3]]

# 2. Convert force values from gram-force-like units to newtons, if appropriate.
forces_n <- forces_raw * 9.81 * 0.001

# 3. Normalize baseline and smooth.
forces_n <- NormalizeBaseline(forces_n, dpi = 10)
forces_sm <- smooth_SR(forces_n, smfl = 19)

# 4. Clean peaks around each stress-relaxation step.
forces_sm <- Rmv_negative_peaks(
  forces_sm,
  S1i = step_indices[1],
  S2i = step_indices[2],
  S3i = step_indices[3]
)

# 5. Extract equilibrium and peak force parameters.
params <- Extract_SR_Parameters2(
  SRmergedfile = forces_sm,
  Step2Index = step_indices[2],
  Step3Index = step_indices[3],
  EqDataPoints = 201
)
```

See [Workflows](docs-workflows.md) for complete examples.

## Main functions

### Extraction

| Function | Purpose |
| --- | --- |
| `Extract_Find_Contact()` | Extract the `<Find Contact>` section from one MACH-1 file. |
| `Extract_Wait()` | Extract a `<Wait>` section before stress-relaxation cycles. |
| `Extract_Single_Stress_Relaxation()` | Extract one stress-relaxation cycle from already-read file lines. |
| `Extract_Stress_Relaxation()` | Extract three stress-relaxation cycles from one MACH-1 file. |
| `Extract_Stress_Relaxation_MultiFiles()` | Extract and merge stress-relaxation data from all `.txt` files in a folder. |

### Cleaning and preprocessing

| Function | Purpose |
| --- | --- |
| `NormalizeBaseline()` | Subtract the row-wise baseline calculated from the first `dpi` points. |
| `smooth_SR()` | Smooth stress-relaxation force curves with a Savitzky-Golay filter. |
| `Rmv_negative_peaks()` | Remove negative peaks around three stress-relaxation steps. |
| `Rmv_negative_peaks2()` | Remove negative peaks around three or four stress-relaxation steps. |
| `Rmv_abnormal_peaks()` | Replace abnormally high isolated peaks around stress-relaxation steps. |
| `tnName()` | Transpose a data frame and promote the first row to column names. |

### Parameter extraction and mechanics

| Function | Purpose |
| --- | --- |
| `Extract_SR_Parameters()` | Extract equilibrium, peak, and pre-peak forces for a three-step protocol. |
| `Extract_SR_Parameters2()` | Extract the same values with configurable equilibrium points and optional fourth step. |
| `calculate_Eq_Modulus()` | Calculate equilibrium elastic modulus from three equilibrium-force values. |
| `calculate_Ins_Modulus()` | Calculate instantaneous elastic modulus from peak-minus-pre-peak force differences. |
| `Hayes_correction()` | Apply Hayes correction to modulus values for Poisson's ratio 0.5. |

### High-level pipelines

| Function | Purpose |
| --- | --- |
| `calculate_moduli_multifiles()` | Extract, clean, smooth, and calculate moduli for a three-step protocol. |
| `calculate_moduli_multifiles2()` | Updated multi-file pipeline with baseline normalization and adjustable equilibrium window. |

See [API reference](docs-api-reference.md) for arguments, return values, and examples.

## Units and assumptions

The high-level multi-file pipelines convert extracted force values using:

```r
force_newtons <- force_raw * 9.81 * 0.001
```

This assumes the raw force values are compatible with conversion from gram-force-like units to newtons. Confirm your MACH-1 export settings before interpreting modulus values.

`Hayes_correction()` is documented for materials with Poisson's ratio of 0.5, such as hydrogels. The function includes interpolation values used to calculate the correction factor.

## Documentation

- [Getting started](docs-getting-started.md)
- [Data format](docs-data-format.md)
- [Workflows](docs-workflows.md)
- [API reference](docs-api-reference.md)
- [Troubleshooting](docs-troubleshooting.md)

## Citation and attribution

If you use `mach4ml` in research, cite this repository and acknowledge the underlying MACH-1 instrument/export workflow. The package was authored by Omar Anwar Elkadi and is licensed under GPL >= 2.

## License

GPL >= 2.
