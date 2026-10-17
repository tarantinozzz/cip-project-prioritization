# Capital Improvement Project Prioritization Indicators

A spatial analysis workflow that translates organizational goals and values into measurable indicators to support capital improvement project prioritization in Harris County Precinct 4.

## Purpose and decision context

Capital improvement programs must choose which projects to advance when available funding cannot cover every need. Comparing projects requires a consistent way to assess how they address the organization's goals and values, alongside other planning considerations.

This project develops the evidence needed for that comparison. For each organizational value, indicators were defined together with methods for measuring them using available data. The R workflow calculates those indicators across a common geographic grid and summarizes them for individual projects, supporting a more transparent and consistent assessment of investment needs.

The resulting project profiles help inform prioritization discussions. The supplied scripts calculate individual indicators and project-level averages; the final choice of projects, policy weights and budget allocation sits outside these two scripts.

## Project contributions

- Translated organizational goals and values into measurable indicators and defined how each indicator would be calculated.
- Integrated social, infrastructure, environmental, accessibility and growth-related datasets into a common spatial analysis framework.
- Developed indicator-specific methods, including spatial counts, area- and length-weighted summaries, service-access measures, raster summaries and normalization.
- Produced comparable grid-level measures and project-level indicator profiles to support evaluation under funding constraints.

## What is measured

The implementation includes indicators related to social vulnerability, traffic crashes, pavement condition, street lighting, sidewalk coverage, repetitive flood losses, flood exposure, tree canopy, park condition, surface temperature, drainage investment coverage, park and trail access, development permits, population growth, traffic utilization and development patterns.

These are measurement topics represented in the code, not a claim that each is a separately adopted organizational value. Values, indicator definitions and measurement choices form the broader prioritization framework.

## How the analysis works

1. **Establish a common geography.** Source layers are analyzed against a reference grid, using pixel_d as the shared identifier. Spatial calculations generally use EPSG:2278, with distances and areas interpreted in the appropriate units.
2. **Measure each indicator.** The method depends on the underlying phenomenon: for example, count crash points, summarize pavement condition by road length, extract raster values, or calculate access relative to population.
3. **Prepare comparable indicator measures.** Apply the specified proportions, transformations, rescaling or percentile ranks. Many measures are oriented so that higher values indicate greater need; interpretation remains specific to each indicator.
4. **Build project profiles.** Intersect project geometries with the scored grid and calculate the mean of each of the 17 indicator measures for each project name.
5. **Support prioritization.** Use the resulting profiles as evidence when comparing proposed investments against organizational objectives and funding constraints.

Project aggregation in the supplied code uses unweighted means over intersection records. The outputs are individual indicator profiles, not a combined priority score or an optimized project portfolio.

## Files and outputs

| File | Role |
| --- | --- |
| Model_28_CIP.R | Calculates and combines grid-level indicators. |
| Model_30_Projectscores.R | Summarizes the 17 indicator measures for intersecting projects. |

Model 28 builds results/CIP.csv, reads that file to add the leapfrog indicator, and writes results/CIP03.csv. Model 30 reads CIP03.csv and writes results/Projects_Scores03.csv. The project output includes the original non-geometric project attributes, indicator averages, intersecting cell identifiers and an intersection-record count.

## Public code version

This repository shares the analysis code and method description. Source datasets, generated results, credentials and the original framework document are not included. Personal absolute paths have been replaced with paths relative to the project folder.

Running the analysis requires access to compatible, authorized datasets with the expected fields and geometry. The absence of those datasets from the public repository does not represent the original operational workflow. The organization and geography named above are retained as project context.

## Local setup and execution

Install R and optionally RStudio. Set the working directory to the folder containing both scripts and this README. Install the required packages once:

```r
install.packages(c("sf", "dplyr", "readr", "scales", "units", "readxl",
                   "stringr", "tidycensus", "tidyverse", "terra"))
```

Spatial packages may require system libraries depending on the platform. Place compatible inputs at the relative paths listed below, keeping shapefile sidecars such as .dbf, .shx and .prj together. Confirm the expected field names, coordinate systems and grid identifiers before running.

The Census retrieval section needs internet access. If a Census API key is required, configure it in the local environment rather than shared code. The scripts create results/ automatically and overwrite their CSV outputs, so preserve any local results that need to be retained.

Run the grid analysis first, then the project summary:

```r
source("Model_28_CIP.R")
source("Model_30_Projectscores.R")
```

## Required local inputs

- `data/raw/ACS_GEOID/tl_2024_48_bg.shp`
- `data/raw/All_Parks/final_parks_layer.shp`
- `data/raw/BuildingPopulation/Bldgs_Pop_Clip.shp`
- `data/raw/Crashes _20-25.csv`
- `data/raw/Drainageprojects/DrainageProjects.shp`
- `data/raw/FEMA/USA_Flood_Hazard_Reduce_Clip.shp`
- `data/raw/HGAC_Parks/HGAC_Parks.shp`
- `data/raw/InfrastructureProjects/Projects.shp`
- `data/raw/NfipMultipleLossProperties.csv`
- `data/raw/P4Boundary/Precinct_4.shp`
- `data/raw/PCI-City/HPW_PCI_Streets_2024.shp`
- `data/raw/PCI/PCI_Pct41.shp`
- `data/raw/Parcels/HCAD_PDATA/BI2025/building_other.txt`
- `data/raw/Parcels/HCAD_PDATA/BI2025/building_res.txt`
- `data/raw/Parcels/HCAD_PDATA/Parcels/Parcels.shp`
- `data/raw/Parcels/Parcels_2020_Oct/Parcels.shp`
- `data/raw/Parks&Trails/Parks_and_Trails.shp`
- `data/raw/Parks_Average_Condition.xlsx`
- `data/raw/Pct4DevPermits/Pct4DevPermits.shp`
- `data/raw/Reference_Grid/P4_Reference_Grid.shp`
- `data/raw/Roadlog/Roadlog_Export_20250224.shp`
- `data/raw/SVI/SVI2022_TEXAS_tract.shp`
- `data/raw/Sidewalk04/Fugro_Sidewalks.shp`
- `data/raw/Streetlights/Street_Lights.shp`
- `data/raw/Surface_temperature/LST_2025.tif`
- `data/raw/Trails/Trails.shp`
- `data/raw/Transportation/TxDOT_5_Year_Statewide_AADT_Traffic_Counts/TMB_RPT_SHORT_AADT_HIST.shp`
- `data/raw/Transportation/TxDOT_Roadway_Inventory/Roadway_Inventory.shp`
- `data/raw/TreeCanopy/TC.tif`
- `data/raw/tab2010_tab2020_st48_tx.txt`
