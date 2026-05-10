# Data format

`mach4ml` expects text exports from Biomomentum MACH-1.

## Required section tags

Extraction functions search for tags in the text file:

- `<Find Contact>`
- `<Wait>`
- `<Stress Relaxation>`
- `<divider>`
- `<END DATA>`

The exact sections required depend on the function. For example, `Extract_Find_Contact()` requires `<Find Contact>` followed by `<END DATA>`, while `Extract_Stress_Relaxation()` requires `<Stress Relaxation>` and three following `<divider>` markers.

## Tab-separated numeric rows

The extraction functions split data rows on tab characters and assume five fields per row. Stress-relaxation rows are assigned the following column names:

```text
Time, z, x, y, Fz
```

`Extract_Stress_Relaxation()` adds a `Step` column with values 1, 2, and 3.

## Multi-file folder structure

For multi-file analysis, place MACH-1 `.txt` files in one folder. File names become sample names.

```text
experiment-folder/
  sample_A.txt
  sample_B.txt
  sample_C.txt
```

`Extract_Stress_Relaxation_MultiFiles()` returns force and displacement matrices where rows are samples and columns are data points.

## Step indices

`Extract_Stress_Relaxation_MultiFiles()` returns `StepsIndex`, a vector of the beginning index for each stress-relaxation step. These indices are used by cleaning and parameter-extraction functions.

```r
sr <- Extract_Stress_Relaxation_MultiFiles(folder)
step_indices <- sr[[3]]
S1i <- step_indices[1]
S2i <- step_indices[2]
S3i <- step_indices[3]
```

## Units

The high-level functions convert extracted force data using:

```r
force_newtons <- force_raw * 9.81 * 0.001
```

Confirm that your MACH-1 export uses force units compatible with this conversion before interpreting modulus values.

Thickness and indenter radius should use matching length units. The examples use millimeters.

## Protocol assumptions

The current high-level functions are centered on three-step stress-relaxation workflows. Some lower-level helpers also support an optional fourth step.

Use consistent protocols within a folder: same number of steps, same sampling behavior, and comparable exported sections.
