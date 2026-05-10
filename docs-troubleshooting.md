# Troubleshooting

## No files are found

The multi-file extractor currently searches with:

```r
Sys.glob(paste0(folder_path, "\\*.txt"))
```

Check what R sees:

```r
folder <- "path/to/mach1_txt_files"
Sys.glob(paste0(folder, "\\*.txt"))
```

If this returns an empty vector, check the folder path, file extension, and operating-system path separators.

## Extraction returns empty data

Confirm that the MACH-1 file contains the expected tags:

```r
file_content <- readLines("sample.txt", warn = FALSE)
grep("<Stress Relaxation>", file_content, value = TRUE)
grep("<divider>", file_content, value = TRUE)
grep("<END DATA>", file_content, value = TRUE)
```

If a protocol does not include a tag expected by the extraction function, use another extraction function or adjust the line-offset argument.

## Inconsistent number of elements

`Extract_Single_Stress_Relaxation()` stops if tab-splitting produces inconsistent row lengths. Inspect the lines reported by the error. Common causes include headers inside the numeric section, missing values, or unexpected delimiters.

## Incorrect baseline correction

`NormalizeBaseline()` calculates the baseline as the row-wise median of the first `dpi` columns. Increase or decrease `dpi` depending on how long your pre-loading baseline is.

```r
forces_clean <- NormalizeBaseline(forces_n, dpi = 20)
```

## Excessive smoothing or weak smoothing

`smooth_SR()` uses a Savitzky-Golay filter. Larger `smfl` values produce stronger smoothing but can distort sharp peaks. Smaller values preserve peaks but remove less noise.

```r
forces_sm <- smooth_SR(forces_n, smfl = 11)
forces_sm <- smooth_SR(forces_n, smfl = 19)
forces_sm <- smooth_SR(forces_n, smfl = 31)
```

## Hayes-corrected values look wrong

Check that:

- `indenterradius` and `Thicknesses` use the same length units;
- thickness values correspond to the correct stress-relaxation steps;
- the sample is appropriate for Poisson's ratio 0.5, as documented for the current Hayes correction helper.

## Direct instantaneous modulus calculation errors

The current `calculate_Ins_Modulus()` implementation uses `Strain` internally rather than its formal argument `Strains`. When calling it directly, define `Strain` in the calling environment or patch the function body.

```r
Strain <- c(0.05, 0.10, 0.15)
calculate_Ins_Modulus(Sd1, Sd2, Sd3, Indentation_area, Strains = Strain)
```

## Results differ across files

For folder-level workflows, keep protocols consistent across files. Mixed protocols can shift step indices and make row-wise comparisons invalid.
