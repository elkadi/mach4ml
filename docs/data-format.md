# MACH-1 data format

`mach4ml` is designed around text exports from Biomomentum MACH-1. The functions parse text files by locating specific section tags and reading tab-separated numeric data between those tags.

## Section tags

The extraction functions search for these tags:

| Tag | Used by | Meaning |
| --- | --- | --- |
| `<Find Contact>` | `Extract_Find_Contact()` | Beginning of the Find Contact section. |
| `<Wait>` | `Extract_Wait()` | Beginning of the Wait section. |
| `<Stress Relaxation>` | `Extract_Stress_Relaxation()` | Beginning of the stress-relaxation section. |
| `<divider>` | `Extract_Stress_Relaxation()` | Divider between stress-relaxation cycles/steps. |
| `<END DATA>` | extraction functions | End of the current data section. |

## Tab-separated data

The low-level extraction helpers split data lines with:

```r
strsplit(data_lines, "\t")
```

Most extracted sections are expected to contain five tab-separated fields per row. Stress-relaxation data are assigned these column names:

```text
Time, z, x, y, Fz
```

`Extract_Stress_Relaxation()` adds a sixth column:

```text
Step
```

where `Step` is 1, 2, or 3 for the three stress-relaxation cycles.

## Stress-relaxation structure

The current `Extract_Stress_Relaxation()` workflow assumes a three-step stress-relaxation protocol:

```text
<Stress Relaxation>
... header lines ...
step 1 data
<divider>
step 2 data
<divider>
step 3 data
<divider>
```

The first step begins at `<Stress Relaxation>` plus the `y` offset. The second and third steps begin after the subsequent `<divider>` tags.

Default offset values:

| Function | Default offset | Meaning |
| --- | ---: | --- |
| `Extract_Find_Contact(x, y = 11)` | 11 | Number of lines skipped after `<Find Contact>`. |
| `Extract_Wait(x, y = 4)` | 4 | Number of lines skipped after `<Wait>`. |
| `Extract_Stress_Relaxation(file, y = 12)` | 12 | Number of lines skipped after `<Stress Relaxation>`. |
| `Extract_Single_Stress_Relaxation(..., z = 1)` | 1 | Number of lines skipped after a cycle start index. |

Adjust these offsets if your MACH-1 export template has a different number of header lines.

## Multi-file input folder

`Extract_Stress_Relaxation_MultiFiles()` reads all files matching:

```r
Sys.glob(paste0(folder_path, "\\*.txt"))
```

It then:

1. extracts stress-relaxation data from every file;
2. uses the first file to define the shared time vector and step indices;
3. extracts force (`Fz`) data from every file;
4. extracts displacement (`z`) data from every file;
5. uses file names without `.txt` as sample names;
6. transposes the merged force and displacement tables so samples are rows and time/data points are columns.

## Returned multi-file objects

`Extract_Stress_Relaxation_MultiFiles(folder_path)` returns:

```r
list(tdfn, SRDisplacement, StepsIndex)
```

| Element | Meaning |
| --- | --- |
| `tdfn` | Merged force data from all files. Rows are samples; columns are time/data points. Values are converted to numeric. |
| `SRDisplacement` | Merged displacement data from all files. Rows are samples; columns are time/data points. |
| `StepsIndex` | Vector of indices marking the beginning of stress-relaxation steps 1, 2, and 3. |

## Units

The high-level modulus functions convert extracted force data with:

```r
tdfn <- SRmergedfile * 9.81 * 0.001
```

This is appropriate only if the exported force values are compatible with conversion from gram-force-like units to newtons. Confirm the units in your MACH-1 export before interpreting calculated moduli.

Thickness and indenter radius are treated as millimeter-scale values in the documented examples and Hayes correction function.

## Common format issues

### Missing tags

If a file does not contain the expected tag, extraction can return `NULL`, empty data, or an indexing error. Open the file in a text editor and verify the exact section label.

### Unexpected number of columns

`Extract_Single_Stress_Relaxation()` checks that all split rows have the same number of fields. If any line has fewer or more tab-separated fields, the function prints the inconsistent lines and stops.

### Different protocol lengths

The multi-file workflow assumes files have compatible time vectors and step locations. If some files have different numbers of data points, different step counts, or different export templates, process them separately or standardize the export settings before merging.
