# API reference

This page summarizes the exported functions in `mach4ml`.

## Extraction

- `Extract_Find_Contact(x, y = 11)`: extracts the `<Find Contact>` section from one MACH-1 text file. Returns a data frame parsed from tab-separated rows.
- `Extract_Wait(x, y = 4)`: extracts the `<Wait>` section before stress-relaxation cycles. Returns a data frame.
- `Extract_Single_Stress_Relaxation(x, start_index, end_index, z = 1)`: extracts one stress-relaxation cycle from a vector of file lines. It checks that rows have consistent tab-separated field counts.
- `Extract_Stress_Relaxation(file, y = 12)`: extracts three stress-relaxation cycles from one MACH-1 file and returns `Time`, `z`, `x`, `y`, `Fz`, and `Step` columns.
- `Extract_Stress_Relaxation_MultiFiles(folder_path)`: extracts stress-relaxation data from all `.txt` files in a folder. Returns `list(tdfn, SRDisplacement, StepsIndex)`.

## Cleaning and preprocessing

- `NormalizeBaseline(SRmergedfile, dpi = 10)`: subtracts each row's median baseline, calculated from the first `dpi` points.
- `smooth_SR(SRmergedfile, smfl = 17)`: smooths each row with `pracma::savgol()` and returns smoothed stress-relaxation curves.
- `Rmv_negative_peaks(SRmergedfile, S1i, S2i, S3i)`: removes negative artifacts around three step peaks.
- `Rmv_negative_peaks2(SRmergedfile, S1i, S2i, S3i, S4i = 0)`: handles three or optional four steps.
- `Rmv_abnormal_peaks(SRmergedfile, S1i, S2i, S3i, S4i = 0, dp = 200, threshold = 1.4)`: replaces isolated high peaks with the second-highest nearby peak when the highest peak exceeds `threshold` times the second.
- `tnName(fx)`: transposes a data frame or matrix and promotes the first row to column names.

## Stress-relaxation parameters

- `Extract_SR_Parameters(SRmergedfile, Step2Index, Step3Index)`: extracts equilibrium forces (`Se1`-`Se3`), peak forces (`Sp1`-`Sp3`), and pre-peak forces (`Sm1`-`Sm3`) for three-step data.
- `Extract_SR_Parameters2(SRmergedfile, Step2Index, Step3Index, Step4Index = 0, EqDataPoints = 201)`: updated extractor with adjustable equilibrium averaging window and optional fourth step.

## Modulus and correction

- `calculate_Eq_Modulus(Se1, Se2, Se3, Indentation_area, Strains)`: calculates equilibrium elastic modulus from three equilibrium-force values by fitting stress versus strain.
- `calculate_Ins_Modulus(Sd1, Sd2, Sd3, Indentation_area, Strains)`: calculates instantaneous elastic modulus from peak-minus-pre-peak force differences. Note: the current implementation references `Strain` internally instead of the formal argument `Strains`.
- `Hayes_correction(Modulus, indenterradius, Thicknesses, v = 0.5)`: applies Hayes correction. It is documented for materials with Poisson's ratio 0.5, such as hydrogels.

## High-level pipelines

- `calculate_moduli_multifiles(folder, Thicknesses, Strain, indenterradius, poisson_eq = 0.5, poisson_inst = 0.5, smfl = 19)`: extracts, converts forces to newtons, smooths, removes negative peaks, calculates equilibrium modulus, and applies Hayes correction.
- `calculate_moduli_multifiles2(folder, Thicknesses, Strain, indenterradius, poisson_eq = 0.5, poisson_inst = 0.5, smfl = 19, EqDataPoints = 201)`: recommended updated workflow. Adds baseline normalization and configurable equilibrium-window length.

Both high-level pipelines return `list(folderoutput, tdfn, tdfn_sm)`.
