# Data directory

This directory documents the input and output data structure
used by the Seoul5 Digital Twin Population pipeline.

## Sample data (included)

The `sample/` directory contains small illustrative subsets
of the generated datasets for demonstration and testing purposes:

- `core_population_sample100.csv`
- `person_file_sample100.csv`
- `timetable_panel_sample100.csv`

These files do **not** contain real individuals and are fully synthetic.

## Processed input data (not included)

The full pipeline expects processed input files to be placed
under `data/processed/` on the user's local machine.

These files are not included in this repository due to size
and data management considerations.

Required processed inputs include:

- `ind_pop_final_jumin.csv`
- `lp_final.csv`
- `purpose_final_kor.csv`
- `lp_in_out_korean.csv`
- `hh_type_2.csv`
- `hn_final.csv`
- `house_generation_level_2.xlsx`
- (optional) `shp_joined.rds`

Users should obtain the original datasets from the official
data providers described in the manuscript and preprocess
them according to the documented pipeline.
