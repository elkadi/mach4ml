# Troubleshooting

This page lists common issues when extracting and analyzing MACH-1 files with `mach4ml`.

## No files are found in a folder

The multi-file workflow uses:

```r
Sys.glob(paste0(folder_path, "\\*.txt"))
```

Check that:

- your folder path is correct;
- files end with `.txt`;
- you are using the path separator expected by your operating system;
- your path does not accidentally include a trailing separator that changes the pattern.

Test file discovery before running analysis:

```r
folder <- "C:/data/mach1"
files <- Sys.glob(paste0(folder, "\\*.txt"))
files
length(files)
```

## Extraction returns empty data or `NULL`

The extraction functions search for exact tags such as `<Stress Relaxation>`, `<Find Contact>`, `<Wait>`, `<divider>`, and `<END DATA>`.

Open the MACH-1 text file and confirm that the expected tags are present and spelled exactly as expected.

```r
file_content <- readLines("sample01.txt", warn = FALSE)
grep("<Stress Relaxation>", file_content, value = TRUE)
grep("<divider>", file_content, value = TRUE)
grep("<END DATA>", file_content, value = TRUE)
```

## Header offset is wrong

If the first extracted rows contain text headers instead of numeric data, adjust the offset argument.

Examples:

```r
Extract_Find_Contact("sample01.txt", y = 12)
Extract_Wait("sample01.txt", y = 5)
Extract_Stress_Relaxation("sample01.txt", y = 13)
```

The correct value depends on the MACH-1 export template.

## Inconsistent number of tab-separated fields

`Extract_Single_Stress_Relaxation()` checks that each data row splits into the same number of tab-separated values. If not, it prints the inconsistent lines and stops with:

```text
Please check the data for inconsistencies.
```

Inspect those lines in the original file. Common causes include:

- malformed rows;
- non-data footer text inside the extracted range;
- missing values;
- extra tabs;
- wrong header offset.

## Step indices do not match the plotted data

The step indices come from `<divider>` locations in the first extracted file. If files have different protocol lengths, step durations, sampling rates, or export templates, merged data may not align.

Check each file individually:

```r
sr1 <- Extract_Stress_Relaxation("sample01.txt")
table(sr1$Step)

sr2 <- Extract_Stress_Relaxation("sample02.txt")
table(sr2$Step)
```

Process incompatible files in separate batches.

## Smoothing removes too much detail

`smooth_SR()` uses `pracma::savgol()` with a filter length controlled by `smfl`. Larger values smooth more aggressively.

Try smaller values:

```r
forces_sm_11 <- smooth_SR(forces_norm, smfl = 11)
forces_sm_17 <- smooth_SR(forces_norm, smfl = 17)
forces_sm_19 <- smooth_SR(forces_norm, smfl = 19)
```

Plot and compare before choosing a final value.

## Baseline correction looks wrong

`NormalizeBaseline()` uses the median of the first `dpi` data points in each row. If those points are not true baseline, change `dpi` or normalize manually.

```r
forces_norm <- NormalizeBaseline(forces_n, dpi = 20)
```

## Negative peak removal changes valid data

`Rmv_negative_peaks()` and `Rmv_negative_peaks2()` replace values below a local median threshold near step transitions. If real features are being removed, inspect the force curve before and after cleaning.

```r
sample_i <- 1
plot(as.numeric(forces_n[sample_i, ]), type = "l")
lines(as.numeric(forces_clean[sample_i, ]), lty = 2)
abline(v = step_indices, lty = 3)
```

Consider skipping this step or using a custom cleaning strategy for unusual experiments.

## Hayes-corrected values look unexpected

`Hayes_correction()` is documented for materials with Poisson's ratio of 0.5, such as hydrogels. Confirm that:

- `indenterradius` is in the same length unit as `Thicknesses`;
- `Thicknesses` correspond to the specimen/step values used in the experiment;
- your material and geometry are appropriate for the correction.

## `calculate_Ins_Modulus()` error involving `Strain`

The current implementation accepts an argument named `Strains`, but internally references `Strain[c(2,3)]`. If `Strain` is not defined in your environment, the function may error.

Workaround:

```r
Strain <- c(0.05, 0.10, 0.15)
ins <- calculate_Ins_Modulus(Sd1, Sd2, Sd3, Indentation_area, Strains = Strain)
```

A future code fix should update the function to use its `Strains` argument consistently.

## Reproducibility checklist

Before reporting results, record:

- package version;
- MACH-1 export settings;
- folder/file list;
- `Thicknesses`;
- `Strain`;
- `indenterradius`;
- `dpi`;
- `smfl`;
- `EqDataPoints`;
- whether negative or abnormal peak cleaning was applied;
- whether Hayes correction was applied.
