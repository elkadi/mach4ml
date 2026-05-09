# API reference

This page summarizes the exported functions in `mach4ml`.

## Extraction functions

### `Extract_Find_Contact(x, y = 11)`

Extracts the `<Find Contact>` section from a MACH-1 text file.

**Arguments**

| Argument | Description |
| --- | --- |
| `x` | Path to a MACH-1 text file. |
| `y` | Number of lines to skip after `<Find Contact>`. Default is `11`. |

**Returns**

A data frame containing the extracted Find Contact section.

**Example**

```r
find_contact <- Extract_Find_Contact("sample01.txt")
```

### `Extract_Wait(x, y = 4)`

Extracts the `<Wait>` section before stress-relaxation cycles, if the protocol includes one.

**Arguments**

| Argument | Description |
| --- | --- |
| `x` | Path to a MACH-1 text file. |
| `y` | Number of lines to skip after `<Wait>`. Default is `4`. |

**Returns**

A data frame containing the extracted Wait section.

**Example**

```r
wait <- Extract_Wait("sample01.txt")
```

### `Extract_Single_Stress_Relaxation(x, start_index, end_index, z = 1)`

Extracts one stress-relaxation cycle from an already-read character vector of MACH-1 file lines.

**Arguments**

| Argument | Description |
| --- | --- |
| `x` | Character vector, usually produced by `readLines()`. |
| `start_index` | Index of `<Stress Relaxation>` or `<divider>` where the cycle begins. |
| `end_index` | Index of the next `<divider>` where the cycle ends. |
| `z` | Number of lines to skip after `start_index`. Default is `1`. |

**Returns**

A data frame containing a single stress-relaxation cycle.

**Notes**

The function checks that every split data line has the same number of tab-separated fields. If not, it prints the inconsistent lines and stops.

### `Extract_Stress_Relaxation(file, y = 12)`

Extracts three stress-relaxation cycles from one MACH-1 text file.

**Arguments**

| Argument | Description |
| --- | --- |
| `file` | Path to a MACH-1 text file. |
| `y` | Number of lines to skip after `<Stress Relaxation>`. Default is `12`. |

**Returns**

A data frame with columns `Time`, `z`, `x`, `y`, `Fz`, and `Step`, where `Step` is `1`, `2`, or `3`.

**Example**

```r
sr <- Extract_Stress_Relaxation("sample01.txt")
```

### `Extract_Stress_Relaxation_MultiFiles(folder_path)`

Extracts stress-relaxation data from all `.txt` files in a folder and merges them into analysis-ready matrices.

**Arguments**

| Argument | Description |
| --- | --- |
| `folder_path` | Path to the folder containing MACH-1 `.txt` files. |

**Returns**

A list:

1. `tdfn`: merged force data with samples as rows and time/data points as columns.
2. `SRDisplacement`: merged displacement data with samples as rows and time/data points as columns.
3. `StepsIndex`: vector containing indices for steps 1, 2, and 3.

**Example**

```r
sr <- Extract_Stress_Relaxation_MultiFiles("C:/data/mach1")
forces <- sr[[1]]
displacement <- sr[[2]]
step_indices <- sr[[3]]
```

## Cleaning and preprocessing functions

### `NormalizeBaseline(SRmergedfile, dpi = 10)`

Normalizes baseline force values to zero by subtracting a row-wise median baseline.

**Arguments**

| Argument | Description |
| --- | --- |
| `SRmergedfile` | Merged force data, usually with samples as rows. |
| `dpi` | Number of initial data points used to estimate the baseline. Default is `10`. |

**Returns**

A data frame/matrix with baseline-subtracted values.

**Example**

```r
forces_norm <- NormalizeBaseline(forces_n, dpi = 10)
```

### `smooth_SR(SRmergedfile, smfl = 17)`

Smooths stress-relaxation force curves using a Savitzky-Golay filter from `pracma::savgol()`.

**Arguments**

| Argument | Description |
| --- | --- |
| `SRmergedfile` | Merged force data. |
| `smfl` | Filter length. Default is `17`. |

**Returns**

A smoothed data frame/matrix. The implementation drops the last 10 columns after smoothing.

**Example**

```r
forces_sm <- smooth_SR(forces_norm, smfl = 19)
```

### `Rmv_negative_peaks(SRmergedfile, S1i, S2i, S3i)`

Removes negative peaks around the three stress-relaxation step peaks.

**Arguments**

| Argument | Description |
| --- | --- |
| `SRmergedfile` | Merged force data. |
| `S1i` | Index of the first step. |
| `S2i` | Index of the second step. |
| `S3i` | Index of the third step. |

**Returns**

A cleaned data frame/matrix.

### `Rmv_negative_peaks2(SRmergedfile, S1i, S2i, S3i, S4i = 0)`

Removes negative peaks around three or four stress-relaxation steps.

**Arguments**

| Argument | Description |
| --- | --- |
| `SRmergedfile` | Merged force data. |
| `S1i` | Index of the first step. |
| `S2i` | Index of the second step. |
| `S3i` | Index of the third step. |
| `S4i` | Optional index of the fourth step. Use `0` to ignore. |

**Returns**

A cleaned data frame/matrix.

### `Rmv_abnormal_peaks(SRmergedfile, S1i, S2i, S3i, S4i = 0, dp = 200, threshold = 1.4)`

Replaces abnormally high isolated peaks near stress-relaxation steps.

**Arguments**

| Argument | Description |
| --- | --- |
| `SRmergedfile` | Merged force data. |
| `S1i`, `S2i`, `S3i` | Indices of the first three steps. |
| `S4i` | Optional index of the fourth step. Use `0` to ignore. |
| `dp` | Number of data points to inspect around each peak. Default is `200`. |
| `threshold` | If the largest peak is greater than `threshold` times the second largest value, it is replaced. Default is `1.4`. |

**Returns**

A modified data frame/matrix.

### `tnName(fx)`

Transposes a data frame or matrix and uses the first transposed row as column names.

**Arguments**

| Argument | Description |
| --- | --- |
| `fx` | Data frame or matrix. |

**Returns**

A transposed matrix/data frame-like object with updated column names.

## Parameter extraction functions

### `Extract_SR_Parameters(SRmergedfile, Step2Index, Step3Index)`

Extracts forces needed for modulus calculation from a three-step stress-relaxation data set.

**Arguments**

| Argument | Description |
| --- | --- |
| `SRmergedfile` | Merged force data. |
| `Step2Index` | Index where the second step begins. |
| `Step3Index` | Index where the third step begins. |

**Returns**

A list containing:

1. `Se1`: equilibrium force for step 1.
2. `Se2`: equilibrium force for step 2.
3. `Se3`: equilibrium force for step 3.
4. `Sp1`: peak force for step 1.
5. `Sp2`: peak force for step 2.
6. `Sp3`: peak force for step 3.
7. `Sm1`: force just before the step 1 peak.
8. `Sm2`: force just before the step 2 peak.
9. `Sm3`: force just before the step 3 peak.

### `Extract_SR_Parameters2(SRmergedfile, Step2Index, Step3Index, Step4Index = 0, EqDataPoints = 201)`

Updated parameter extractor with configurable equilibrium averaging and optional fourth step.

**Arguments**

| Argument | Description |
| --- | --- |
| `SRmergedfile` | Merged force data. |
| `Step2Index` | Index where the second step begins. |
| `Step3Index` | Index where the third step begins. |
| `Step4Index` | Optional index where the fourth step begins. Default is `0`. |
| `EqDataPoints` | Number of points used to average equilibrium force. Default is `201`. |

**Returns**

For three steps, returns the same nine values as `Extract_SR_Parameters()`. If `Step4Index > 0`, returns 12 values: four equilibrium forces, four peak forces, and four pre-peak forces.

## Modulus and correction functions

### `calculate_Eq_Modulus(Se1, Se2, Se3, Indentation_area, Strains)`

Calculates equilibrium elastic modulus from three equilibrium-force values by fitting stress versus strain.

**Arguments**

| Argument | Description |
| --- | --- |
| `Se1`, `Se2`, `Se3` | Equilibrium forces from steps 1, 2, and 3. |
| `Indentation_area` | Indentation area, typically `pi * indenterradius^2`. |
| `Strains` | Strain values for the three protocol steps. |

**Returns**

Calculated equilibrium elastic modulus values.

**Example**

```r
eq <- calculate_Eq_Modulus(Se1, Se2, Se3, pi * 1.5^2, c(0.05, 0.10, 0.15))
```

### `calculate_Ins_Modulus(Sd1, Sd2, Sd3, Indentation_area, Strains)`

Calculates instantaneous elastic modulus from peak-minus-pre-peak force differences.

**Arguments**

| Argument | Description |
| --- | --- |
| `Sd1`, `Sd2`, `Sd3` | Differences between peak force and pre-peak force for steps 1, 2, and 3. |
| `Indentation_area` | Indentation area. |
| `Strains` | Strain values for the protocol steps. |

**Returns**

Calculated instantaneous elastic modulus values.

**Note**

The current implementation uses `Strain[c(2,3)]` internally rather than the `Strains` argument name. Review this function before relying on it in production workflows.

### `Hayes_correction(Modulus, indenterradius, Thicknesses, v = 0.5)`

Applies a Hayes et al. correction to modulus values.

**Arguments**

| Argument | Description |
| --- | --- |
| `Modulus` | Modulus value or values to correct. |
| `indenterradius` | Indenter radius in mm. |
| `Thicknesses` | Sample thickness or vector of thicknesses for the stress-relaxation steps. |
| `v` | Poisson's ratio. Default is `0.5`. |

**Returns**

Hayes-corrected modulus values.

**Example**

```r
Hayes_correction(0.03, indenterradius = 1.5, Thicknesses = c(2, 1.9, 1.8), v = 0.5)
```

## High-level pipeline functions

### `calculate_moduli_multifiles(folder, Thicknesses, Strain, indenterradius, poisson_eq = 0.5, poisson_inst = 0.5, smfl = 19)`

Extracts, preprocesses, smooths, and calculates equilibrium moduli for all `.txt` files in a folder.

**Arguments**

| Argument | Description |
| --- | --- |
| `folder` | Folder containing MACH-1 `.txt` files. |
| `Thicknesses` | Sample thicknesses for the three steps. |
| `Strain` | Strain values for the three steps. |
| `indenterradius` | Indenter radius. |
| `poisson_eq` | Poisson's ratio for equilibrium modulus. Currently retained for interface compatibility. |
| `poisson_inst` | Poisson's ratio for instantaneous modulus. Currently retained for interface compatibility. |
| `smfl` | Savitzky-Golay filter length. Default is `19`. |

**Returns**

A list:

1. `folderoutput`: modulus summary table.
2. `tdfn`: cleaned force data in newtons.
3. `tdfn_sm`: smoothed cleaned force data in newtons.

### `calculate_moduli_multifiles2(folder, Thicknesses, Strain, indenterradius, poisson_eq = 0.5, poisson_inst = 0.5, smfl = 19, EqDataPoints = 201)`

Recommended high-level function. Compared with `calculate_moduli_multifiles()`, it includes baseline normalization and configurable equilibrium averaging.

**Arguments**

| Argument | Description |
| --- | --- |
| `folder` | Folder containing MACH-1 `.txt` files. |
| `Thicknesses` | Sample thicknesses for the three steps. |
| `Strain` | Strain values for the three steps. |
| `indenterradius` | Indenter radius. |
| `poisson_eq` | Poisson's ratio for equilibrium modulus. Currently retained for interface compatibility. |
| `poisson_inst` | Poisson's ratio for instantaneous modulus. Currently retained for interface compatibility. |
| `smfl` | Savitzky-Golay filter length. Default is `19`. |
| `EqDataPoints` | Number of points used for equilibrium-force averaging. Default is `201`. |

**Returns**

A list:

1. `folderoutput`: modulus summary table.
2. `tdfn`: baseline-normalized and cleaned force data in newtons.
3. `tdfn_sm`: smoothed baseline-normalized and cleaned force data in newtons.

**Example**

```r
result <- calculate_moduli_multifiles2(
  folder = "C:/data/mach1",
  Thicknesses = c(2.0, 1.9, 1.8),
  Strain = c(0.05, 0.10, 0.15),
  indenterradius = 1.5,
  smfl = 19,
  EqDataPoints = 201
)

result[[1]]
```
