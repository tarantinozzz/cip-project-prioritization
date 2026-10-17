# CIP spatial analysis

R code for Capital Improvement Plan indicators in Harris County Precinct 4.

- Model_28_CIP.R calculates grid-level indicators.
- Model_30_Projectscores.R calculates 17 indicator averages for projects intersecting the grid. It does not produce a combined priority ranking.

## Data and privacy

Only code is shared. Source data, generated results, credentials and the original framework document are not included. Personal absolute file paths were replaced with relative paths. Obtain permission before sharing any underlying datasets or organizational material.

## Local setup

Install R and optionally RStudio. Open both scripts and set the working directory to the folder containing them (in RStudio: Session > Set Working Directory > Choose Directory).

Install the required packages once:

```r
install.packages(c("sf", "dplyr", "readr", "scales", "units", "readxl",
                   "stringr", "tidycensus", "tidyverse", "terra"))
```

Spatial package installation may require system libraries depending on your platform. Package versions were not provided or pinned. The Census section needs internet access; if a Census API key is needed, keep it in your local environment, never in shared code.

Create a local data/raw folder and arrange your authorized datasets at the paths listed below. Shapefiles need their matching sidecar files, including .dbf, .shx and .prj. The scripts create a results folder for local CSV outputs.

## Execution order

Model 28 creates results/CIP.csv, reads that same file to add the leapfrog indicator, and writes results/CIP03.csv. Model 30 reads results/CIP03.csv and writes results/Projects_Scores03.csv. The final leapfrog input filename was corrected to CIP.csv at the author's request. Back up existing local results before running: the scripts overwrite their CSV outputs.

After validating inputs, run Model 28, then Model 30. Run from the folder containing both scripts.

Analytical formulas and joins are retained. Possible issues include duplicated counts from spatial joins, order-dependent drainage coverage, whole-segment lighting classification, missing values treated as zero, and project grouping by potentially nonunique names. Review these before relying on results. R execution was not tested during preparation because R and the source datasets were unavailable.

## Required local inputs

- `data/raw/ACS_GEOID/tl_2024_48_bg.shp` — Model_28_CIP.R
- `data/raw/All_Parks/final_parks_layer.shp` — Model_28_CIP.R
- `data/raw/BuildingPopulation/Bldgs_Pop_Clip.shp` — Model_28_CIP.R
- `data/raw/Crashes _20-25.csv` — Model_28_CIP.R
- `data/raw/Drainageprojects/DrainageProjects.shp` — Model_28_CIP.R
- `data/raw/FEMA/USA_Flood_Hazard_Reduce_Clip.shp` — Model_28_CIP.R
- `data/raw/HGAC_Parks/HGAC_Parks.shp` — Model_28_CIP.R
- `data/raw/InfrastructureProjects/Projects.shp` — Model_30_Projectscores.R
- `data/raw/NfipMultipleLossProperties.csv` — Model_28_CIP.R
- `data/raw/P4Boundary/Precinct_4.shp` — Model_28_CIP.R
- `data/raw/PCI-City/HPW_PCI_Streets_2024.shp` — Model_28_CIP.R
- `data/raw/PCI/PCI_Pct41.shp` — Model_28_CIP.R
- `data/raw/Parcels/HCAD_PDATA/BI2025/building_other.txt` — Model_28_CIP.R
- `data/raw/Parcels/HCAD_PDATA/BI2025/building_res.txt` — Model_28_CIP.R
- `data/raw/Parcels/HCAD_PDATA/Parcels/Parcels.shp` — Model_28_CIP.R
- `data/raw/Parcels/Parcels_2020_Oct/Parcels.shp` — Model_28_CIP.R
- `data/raw/Parks&Trails/Parks_and_Trails.shp` — Model_28_CIP.R
- `data/raw/Parks_Average_Condition.xlsx` — Model_28_CIP.R
- `data/raw/Pct4DevPermits/Pct4DevPermits.shp` — Model_28_CIP.R
- `data/raw/Reference_Grid/P4_Reference_Grid.shp` — Model_28_CIP.R, Model_30_Projectscores.R
- `data/raw/Roadlog/Roadlog_Export_20250224.shp` — Model_28_CIP.R
- `data/raw/SVI/SVI2022_TEXAS_tract.shp` — Model_28_CIP.R
- `data/raw/Sidewalk04/Fugro_Sidewalks.shp` — Model_28_CIP.R
- `data/raw/Streetlights/Street_Lights.shp` — Model_28_CIP.R
- `data/raw/Surface_temperature/LST_2025.tif` — Model_28_CIP.R
- `data/raw/Trails/Trails.shp` — Model_28_CIP.R
- `data/raw/Transportation/TxDOT_5_Year_Statewide_AADT_Traffic_Counts/TMB_RPT_SHORT_AADT_HIST.shp` — Model_28_CIP.R
- `data/raw/Transportation/TxDOT_Roadway_Inventory/Roadway_Inventory.shp` — Model_28_CIP.R
- `data/raw/TreeCanopy/TC.tif` — Model_28_CIP.R
- `data/raw/tab2010_tab2020_st48_tx.txt` — Model_28_CIP.R

## Public upload

Upload only these two R scripts and this README.md. This minimal package contains no .gitignore: do not upload local data/raw, results, environment files, keys or other private files. Review every file before committing. Confirm you have authority to release the code. No reuse license has been selected.
