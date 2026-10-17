# Run from the folder containing these two scripts and README.md.
if (!file.exists("Model_28_CIP.R") || !file.exists("Model_30_Projectscores.R")) stop("Set the working directory to the folder containing both R scripts.")
dir.create("results", showWarnings = FALSE, recursive = TRUE)


library(sf)
library(dplyr)
library(readr)
library(scales)
library(units)
library(readxl)
library(stringr)
library(tidycensus)
library(tidyverse)

library(terra)
# =============================================================================
# LOAD DATA
# =============================================================================

# Reference grid
reference_grid <- st_read("data/raw/Reference_Grid/P4_Reference_Grid.shp") %>%
  st_transform(2278)

# Precinct 4 boundary
precinct4 <- st_read("data/raw/P4Boundary/Precinct_4.shp") %>%
  st_transform(2278)

# SVI data - filter to Precinct 4
svi_data <- st_read("data/raw/SVI/SVI2022_TEXAS_tract.shp") %>%
  st_transform(2278)

svi_p4_intersect <- st_intersects(svi_data, precinct4, sparse = FALSE)
svi_p4 <- svi_data[rowSums(svi_p4_intersect) > 0, ]

# =============================================================================
# PREPARE SVI DATA
# =============================================================================

svi_columns <- c("RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4", "RPL_THEMES")

svi_clean <- svi_p4 %>%
  select(all_of(svi_columns)) %>%
  mutate(across(all_of(svi_columns), ~ifelse(.x < 0, NA, .x)))

# =============================================================================
# ASSIGN SVI TO GRID (AREA-WEIGHTED)
# =============================================================================

grid_svi_intersections <- st_intersection(
  reference_grid %>% select(pixel_d, geometry),
  svi_clean %>% select(all_of(svi_columns))
) %>%
  mutate(intersection_area = as.numeric(st_area(.)))

grid_svi_weighted <- grid_svi_intersections %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(
    across(all_of(svi_columns), 
           ~weighted.mean(.x, intersection_area, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(across(all_of(svi_columns), ~ifelse(is.nan(.x), 0, .x)))

svi_grid <- reference_grid %>%
  left_join(grid_svi_weighted, by = "pixel_d") %>%
  mutate(across(all_of(svi_columns), ~coalesce(.x, 0)))

# =============================================================================
# CALCULATE SVI SCORES
# =============================================================================

svi_final <- svi_grid %>%
  mutate(
    svi_socio = RPL_THEME1,
    svi_household = RPL_THEME2, 
    svi_minority = RPL_THEME3,
    svi_housing = RPL_THEME4,
    svi = RPL_THEMES
  )

# =============================================================================
# EXPORT FOR EXCEL (NO GEOMETRY)
# =============================================================================

svi_export <- svi_final %>%
  st_drop_geometry() %>%
  select(
    pixel_d,
    svi_socio,
    svi_household,
    svi_minority,
    svi_housing,
    svi
  )

write_csv(svi_export, 
          "results/CIP.csv")

# =============================================================================
# CRASH DATA
# =============================================================================

crash_csv <- read_csv("data/raw/Crashes _20-25.csv")

crash_data <- crash_csv %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(2278)

crash_intersects <- st_intersects(reference_grid, crash_data, sparse = FALSE)

crash_export <- reference_grid %>%
  st_drop_geometry() %>%
  mutate(
    crash_count = rowSums(crash_intersects),
    crash_score = rescale(crash_count, to = c(0, 1))
  ) %>%
  select(pixel_d, crash_count, crash_score)

# Read existing CIP.csv and add crash columns
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(crash_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")


# =============================================================================
# PAVEMENT CONDITION INDEX (PCI)
# =============================================================================

county_pci <- st_read("data/raw/PCI/PCI_Pct41.shp") %>%
  st_transform(2278) %>%
  select(DTS_PCI__, geometry) %>%
  rename(pci_value = DTS_PCI__) %>%
  mutate(pci_value = ifelse(pci_value == -1, 0, pci_value))

city_pci <- st_read("data/raw/PCI-City/HPW_PCI_Streets_2024.shp") %>%
  st_transform(2278) %>%
  select(PCI, geometry) %>%
  rename(pci_value = PCI)

# County intersections and weighted average
county_intersections <- st_intersection(
  reference_grid %>% select(pixel_d, geometry), 
  county_pci %>% select(pci_value, geometry)
) %>%
  mutate(length_mi = as.numeric(set_units(st_length(.), "mi")))

county_pci_summary <- county_intersections %>%
  st_set_geometry(NULL) %>%
  group_by(pixel_d) %>%
  summarise(
    county_weighted_pci = sum(pci_value * length_mi, na.rm = TRUE) / sum(length_mi, na.rm = TRUE),
    .groups = "drop"
  )

# City intersections and weighted average
city_intersections <- st_intersection(
  reference_grid %>% select(pixel_d, geometry),
  city_pci %>% select(pci_value, geometry)
) %>%
  mutate(length_mi = as.numeric(set_units(st_length(.), "mi")))

city_pci_summary <- city_intersections %>%
  st_set_geometry(NULL) %>%
  group_by(pixel_d) %>%
  summarise(
    city_weighted_pci = sum(pci_value * length_mi, na.rm = TRUE) / sum(length_mi, na.rm = TRUE),
    .groups = "drop"
  )

# Combine PCI data
pci_combined <- reference_grid %>%
  st_drop_geometry() %>%
  left_join(county_pci_summary, by = "pixel_d") %>%
  left_join(city_pci_summary, by = "pixel_d") %>%
  mutate(
    county_weighted_pci = coalesce(county_weighted_pci, 0),
    city_weighted_pci = coalesce(city_weighted_pci, 0),
    pci_raw = case_when(
      county_weighted_pci > 0 & city_weighted_pci > 0 ~ (county_weighted_pci + city_weighted_pci) / 2,
      county_weighted_pci > 0 & city_weighted_pci == 0 ~ county_weighted_pci,
      county_weighted_pci == 0 & city_weighted_pci > 0 ~ city_weighted_pci,
      TRUE ~ 0
    ),
    # Normalize and INVERT: higher score = worse pavement = needs more investment
    pci_score = case_when(
      pci_raw > 0 ~ 1 - rescale(pci_raw, to = c(0, 1)),
      TRUE ~ 0
    )
  )

pci_export <- pci_combined %>%
  select(pixel_d, pci_raw, pci_score)

# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(pci_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")

cat("✓ PCI data added to CIP.csv\n")

# =============================================================================
# STREET LIGHTS - % ROADS WITHOUT LIGHTING
# =============================================================================

# Load P4 boundary
precinct4 <- st_read("data/raw/P4Boundary/Precinct_4.shp") %>%
  st_transform(2278)

# Load and clip roads to P4 boundary
roads <- st_read("data/raw/Roadlog/Roadlog_Export_20250224.shp") %>%
  st_transform(2278) %>%
  st_intersection(precinct4)

# Load street lights
street_lamps <- st_read("data/raw/Streetlights/Street_Lights.shp") %>%
  st_transform(2278) %>%
  st_intersection(precinct4)

# Create buffer around lights (e.g., 100 feet lighting coverage)
light_buffer <- st_buffer(street_lamps, dist = 100)
lit_area <- st_union(light_buffer)

# For each pixel, calculate % of roads without lighting
pixel_roads <- st_intersection(reference_grid %>% select(pixel_d, geometry), roads)

road_lighting <- pixel_roads %>%
  mutate(
    road_length = as.numeric(st_length(geometry)),
    # Check if road segment is lit
    is_lit = as.numeric(st_intersects(geometry, lit_area, sparse = FALSE))
  ) %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(
    total_road_length = sum(road_length, na.rm = TRUE),
    lit_road_length = sum(road_length * is_lit, na.rm = TRUE),
    unlit_road_length = total_road_length - lit_road_length,
    road_unlit_pct = ifelse(total_road_length > 0, 
                            unlit_road_length / total_road_length, 
                            0),
    .groups = "drop"
  )

# Ensure all pixels are included
road_lighting_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(road_lighting, by = "pixel_d") %>%
  mutate(
    unlit_road_length = coalesce(unlit_road_length, 0),
    road_unlit_pct = coalesce(road_unlit_pct, 0)
  )

# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(road_lighting_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")

# =============================================================================
# SIDEWALKS - COVERAGE RATIO
# =============================================================================

sidewalks <- st_read("data/raw/Sidewalk04/Fugro_Sidewalks.shp") %>%
  st_transform(2278) %>%
  st_filter(precinct4)

roads <- st_read("data/raw/Roadlog/Roadlog_Export_20250224.shp") %>%
  st_transform(2278) %>%
  st_filter(precinct4)

# Calculate sidewalk length per grid cell
sidewalk_intersections <- st_intersection(
  reference_grid %>% select(pixel_d, geometry), 
  sidewalks
) %>%
  mutate(length_ft = as.numeric(st_length(.)))

sidewalk_summary <- sidewalk_intersections %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(sidewalk_length_ft = sum(length_ft, na.rm = TRUE), .groups = "drop")

# Calculate road length per grid cell  
road_intersections <- st_intersection(
  reference_grid %>% select(pixel_d, geometry), 
  roads
) %>%
  mutate(length_ft = as.numeric(st_length(.)))

road_summary <- road_intersections %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(road_length_ft = sum(length_ft, na.rm = TRUE), .groups = "drop")

sidewalk_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(sidewalk_summary, by = "pixel_d") %>%
  left_join(road_summary, by = "pixel_d") %>%
  mutate(
    sidewalk_length_ft = coalesce(sidewalk_length_ft, 0),
    road_length_ft = coalesce(road_length_ft, 0),
    sidewalk_to_road_ratio = case_when(
      road_length_ft > 0 ~ sidewalk_length_ft / road_length_ft,
      TRUE ~ 0
    ),
    # Simply invert the ratio (cap at 1 first)
    # Higher score = less sidewalk coverage = needs more investment
    sidewalk_score = 1 - pmin(sidewalk_to_road_ratio, 1)
  ) %>%
  select(pixel_d, sidewalk_to_road_ratio, sidewalk_score)

# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(sidewalk_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")

# =============================================================================
# REPETITIVE LOSS PROPERTIES
# =============================================================================

# Load repetitive loss CSV and aggregate by census block group
repetitive_loss_csv <- read_csv("data/raw/NfipMultipleLossProperties.csv")
if (interactive()) utils::View(repetitive_loss_csv)
colnames(repetitive_loss_csv)

rep_loss_by_bg <- repetitive_loss_csv %>%
  group_by(censusBlockGroup) %>%
  summarise(
    total_events = sum(totalLosses, na.rm = TRUE),
    property_count = n(),
    .groups = "drop"
  ) %>%
  filter(total_events > 0)

# Load census block group boundaries
census_bg <- st_read("data/raw/ACS_GEOID/tl_2024_48_bg.shp") %>%
  st_transform(2278)

# Join repetitive loss data with census boundaries
rep_loss_spatial <- census_bg %>%
  left_join(rep_loss_by_bg, by = c("GEOID" = "censusBlockGroup")) %>%
  mutate(
    total_events = coalesce(total_events, 0),
    property_count = coalesce(property_count, 0L)
  ) %>%
  st_filter(st_union(reference_grid))

# Assign repetitive loss to grid cells using area-weighted intersection
rep_loss_intersections <- st_intersection(reference_grid, rep_loss_spatial) %>%
  mutate(intersection_area = as.numeric(st_area(.)))

rep_loss_summary <- rep_loss_intersections %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(
    weighted_events = sum(total_events * intersection_area, na.rm = TRUE) / sum(intersection_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(weighted_events = ifelse(is.nan(weighted_events), 0, weighted_events))

# Create export with inverted score
rep_loss_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(rep_loss_summary, by = "pixel_d") %>%
  mutate(
    rep_loss_events = coalesce(weighted_events, 0),
    # Higher score = more loss events = needs more investment
    rep_loss_score = rescale(rep_loss_events, to = c(0, 1))
  ) %>%
  select(pixel_d, rep_loss_events, rep_loss_score)

# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(rep_loss_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")

# =============================================================================
# FLOODWAY EXPOSURE - PERCENT POPULATION IN FLOODPLAIN
# =============================================================================

# Load floodway data
floodway_data <- st_read("data/raw/FEMA/USA_Flood_Hazard_Reduce_Clip.shp") %>%
  st_transform(2278)

# Union all floodplain zones into one layer
floodplain_union <- st_union(floodway_data)

# Load building population data (UPDATE PATH)
buildings <- st_read("data/raw/BuildingPopulation/Bldgs_Pop_Clip.shp") %>%
  st_transform(2278)

# Identify buildings in floodplain
buildings <- buildings %>%
  mutate(
    in_floodplain = as.numeric(st_intersects(geometry, floodplain_union, sparse = FALSE))
  )

# Calculate population stats per pixel
building_intersections <- st_intersection(
  reference_grid %>% select(pixel_d, geometry),
  buildings
)

flood_pop_summary <- building_intersections %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(
    total_population = sum(Est_Night_, na.rm = TRUE),  # UPDATE column name if different
    population_in_floodplain = sum(Est_Night_ * in_floodplain, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    flood_pop_pct = ifelse(total_population > 0, 
                           population_in_floodplain / total_population, 
                           0)
  )

# Create export
flood_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(flood_pop_summary, by = "pixel_d") %>%
  mutate(
    total_population = coalesce(total_population, 0),
    population_in_floodplain = coalesce(population_in_floodplain, 0),
    flood_pop_pct = coalesce(flood_pop_pct, 0)
  ) %>%
  select(pixel_d, total_population, population_in_floodplain, flood_pop_pct)

# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(flood_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")

# =============================================================================
# FLOODWAY EXPOSURE - WEIGHTED POPULATION IN FLOODPLAIN
# =============================================================================

# Load floodway data with zones
floodway_data <- st_read("data/raw/FEMA/USA_Flood_Hazard_Reduce_Clip.shp") %>%
  st_transform(2278) %>%
  st_filter(precinct4)

# Define flood zone weights
flood_zone_weights <- data.frame(
  zone = c("Regulatory Floodway", "1% Annual Chance Flood Hazard", "0.2% Annual Chance Flood Hazard"),
  weight = c(1.0, 0.8, 0.6)
)

# Add weights to flood data
floodway_weighted <- floodway_data %>%
  left_join(flood_zone_weights, by = c("esri_symbo" = "zone")) %>%
  mutate(weight = coalesce(weight, 0.5))

# Load building population data
buildings <- st_read("data/raw/BuildingPopulation/Bldgs_Pop_Clip.shp") %>%
  st_transform(2278) %>%
  st_filter(precinct4)

# For each building, find which flood zone it's in (if any) and get the weight
building_flood_join <- st_join(buildings, floodway_weighted %>% select(esri_symbo, weight))

# Get maximum weight if building is in multiple zones
building_flood_weighted <- building_flood_join %>%
  group_by(across(-c(esri_symbo, weight))) %>%  # Group by all building columns
  summarise(
    flood_weight = max(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    flood_weight = ifelse(is.infinite(flood_weight), 0, flood_weight),  # Not in any flood zone
    weighted_population = Est_Night_ * flood_weight
  )

# Spatial join to grid
building_to_grid <- st_join(building_flood_weighted, reference_grid %>% select(pixel_d))

# Calculate weighted flood population per pixel
flood_pop_summary <- building_to_grid %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(
    total_population = sum(Est_Night_, na.rm = TRUE),
    weighted_population_in_flood = sum(weighted_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Weighted flood exposure as proportion
    flood_pop_weighted = ifelse(total_population > 0, 
                                weighted_population_in_flood / total_population, 
                                0)
  )

# Create export
flood_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(flood_pop_summary, by = "pixel_d") %>%
  mutate(
    total_population = coalesce(total_population, 0),
    weighted_population_in_flood = coalesce(weighted_population_in_flood, 0),
    flood_score = coalesce(flood_pop_weighted, 0)
  ) %>%
  select(pixel_d, total_population, weighted_population_in_flood, flood_score)

# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(flood_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")

cat("✓ Weighted floodway population data added to CIP.csv\n")

# =============================================================================
# TREE CANOPY COVERAGE
# =============================================================================
library(terra)
tree_canopy_raster <- rast("data/raw/TreeCanopy/TC.tif")

# Transform raster to match grid CRS if needed
if(crs(tree_canopy_raster) != "EPSG:2278") {
  tree_canopy_raster <- project(tree_canopy_raster, "EPSG:2278")
}

# Extract tree canopy values for each grid cell (mean)
tree_canopy_values <- extract(tree_canopy_raster, reference_grid, fun = mean, na.rm = TRUE)

# Create export with inverted score
tree_export <- reference_grid %>%
  st_drop_geometry() %>%
  mutate(
    tree_canopy_raw = coalesce(tree_canopy_values[[2]], 0),
    # INVERT: higher score = less canopy = needs more investment
    tree_canopy_score = 1 - scales::rescale(tree_canopy_raw, to = c(0, 1))  # Use scales:: prefix
  ) %>%
  select(pixel_d, tree_canopy_raw, tree_canopy_score)

# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(tree_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")

# =============================================================================
# PARK AMENITIES CONDITION
# =============================================================================

# Load parks spatial data
parks_trails <- st_read("data/raw/Parks&Trails/Parks_and_Trails.shp") %>%
  st_transform(2278) %>%
  st_intersection(precinct4)

# Load park condition scores from Excel
park_conditions <- read_excel("data/raw/Parks_Average_Condition.xlsx")

# Join condition scores to parks
parks_with_conditions <- parks_trails %>%
  rename(`Park Name` = Polygons_3) %>%  # Adjust column name if different
  left_join(park_conditions, by = "Park Name") %>%
  mutate(Average_Condition = as.numeric(Average_Condition)) %>%
  filter(!is.na(Average_Condition))

# Intersect parks with grid cells
park_intersections <- st_intersection(
  reference_grid %>% select(pixel_d, geometry),
  parks_with_conditions %>% select(`Park Name`, Average_Condition, geometry)
) %>%
  mutate(intersection_area = as.numeric(st_area(.)))

# Calculate area-weighted average condition per grid cell
park_summary <- park_intersections %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(
    park_area = sum(intersection_area, na.rm = TRUE),
    weighted_park_condition = sum(Average_Condition * intersection_area, na.rm = TRUE) / sum(intersection_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(weighted_park_condition = ifelse(is.nan(weighted_park_condition), NA, weighted_park_condition))

# Create export with score transformation
park_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(park_summary, by = "pixel_d") %>%
  mutate(
    park_area = coalesce(park_area, 0),
    park_condition_raw = coalesce(weighted_park_condition, NA),
    # Transform from 1-3 scale to 0-1 (3=best, 1=worst)
    # Higher score = worse condition = needs more investment
    park_condition_score = ifelse(!is.na(park_condition_raw),
                                  (3 - park_condition_raw) / 2,  # Maps 3→0, 1→1
                                  NA)
  ) %>%
  select(pixel_d, park_area, park_condition_raw, park_condition_score)

# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(park_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")

# =============================================================================
# LAND SURFACE TEMPERATURE
# =============================================================================

lst_raster <- rast("data/raw/Surface_temperature/LST_2025.tif")  # UPDATE PATH

# Transform raster to match grid CRS if needed
if(crs(lst_raster) != "EPSG:2278") {
  lst_raster <- project(lst_raster, "EPSG:2278")
}

# Extract LST values for each grid cell (mean)
lst_values <- extract(lst_raster, reference_grid, fun = mean, na.rm = TRUE)

# Create export - NO inversion needed (higher temp = worse = already correct direction)
lst_export <- reference_grid %>%
  st_drop_geometry() %>%
  mutate(
    lst_raw = coalesce(lst_values[[2]], 0),
    # Higher temp = worse = needs more investment (no inversion needed)
    lst_score = scales::rescale(lst_raw, to = c(0, 1))
  ) %>%
  select(pixel_d, lst_raw, lst_score)

# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(lst_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")


# =============================================================================
# DRAINAGE INVESTMENT COVERAGE
# =============================================================================

# Load completed drainage projects
drainage_projects <- st_read("data/raw/Drainageprojects/DrainageProjects.shp") %>%
  st_transform(2278) %>%
  st_filter(precinct4) %>%
  filter(PROJECT_ST == "Complete")

# Count completed projects per cell
drainage_intersects <- st_intersects(reference_grid, drainage_projects, sparse = FALSE)
completed_count <- rowSums(drainage_intersects)

# Function to get neighboring cells
get_neighbors <- function(grid, target_pixel_d, distance = 1) {
  target_cell <- grid[grid$pixel_d == target_pixel_d, ]
  buffer_size <- unique(grid$cell_sz)[1] * distance * 1.5
  target_buffer <- st_buffer(st_centroid(target_cell), buffer_size)
  neighbors <- st_intersects(target_buffer, st_centroid(grid), sparse = FALSE)[1,]
  neighbor_ids <- grid$pixel_d[neighbors & grid$pixel_d != target_pixel_d]
  return(neighbor_ids)
}

# Initialize coverage score
drainage_coverage <- rep(0, nrow(reference_grid))

# Apply scoring with ripple effect
cells_with_projects <- which(completed_count > 0)

for(i in cells_with_projects) {
  pixel_d <- reference_grid$pixel_d[i]
  project_count <- completed_count[i]
  
  # Center cell gets coverage based on project count
  center_coverage <- min(1.0, project_count / 4)
  drainage_coverage[i] <- center_coverage
  
  # Ripple effect for multiple projects
  if(project_count >= 2) {
    ring1_neighbors <- get_neighbors(reference_grid, pixel_d, distance = 1)
    ring1_coverage <- center_coverage * 0.8
    
    for(neighbor_d in ring1_neighbors) {
      neighbor_idx <- which(reference_grid$pixel_d == neighbor_d)
      drainage_coverage[neighbor_idx] <- max(drainage_coverage[neighbor_idx], ring1_coverage)
    }
  }
  
  if(project_count >= 3) {
    ring2_neighbors <- get_neighbors(reference_grid, pixel_d, distance = 2)
    ring2_coverage <- center_coverage * 0.6
    
    for(neighbor_d in ring2_neighbors) {
      neighbor_idx <- which(reference_grid$pixel_d == neighbor_d)
      drainage_coverage[neighbor_idx] <- max(drainage_coverage[neighbor_idx], ring2_coverage)
    }
  }
}

# Create export - INVERT so higher score = less coverage = needs investment
drainage_export <- reference_grid %>%
  st_drop_geometry() %>%
  mutate(
    drainage_projects = completed_count,
    drainage_coverage = drainage_coverage,
    # INVERT: higher score = less drainage coverage = needs more investment
    drainage_score = 1 - drainage_coverage
  ) %>%
  select(pixel_d, drainage_projects, drainage_coverage, drainage_score)

# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(drainage_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")

# =============================================================================
# PARK ACCESSIBILITY - ACRES PER CAPITA
# =============================================================================

# Buffer P4 by 5 miles to capture nearby parks
precinct4_buffered <- st_buffer(precinct4, 5280 * 5)

# Load P4 parks
parks_p4 <- st_read("data/raw/All_Parks/final_parks_layer.shp") %>%
  st_transform(2278)

# Load additional parks layer (REPLACE WITH YOUR ACTUAL PATH)
parks_outside <- st_read("data/raw/HGAC_Parks/HGAC_Parks.shp") %>%
  st_transform(2278) %>%
  st_filter(precinct4_buffered)  # Only keep parks within 5-mile buffer

# Combine parks, removing any duplicates
# Assuming both layers have similar structure - adjust column names as needed
parks_combined <- bind_rows(
  parks_p4 %>% mutate(source = "P4"),
  parks_outside %>% mutate(source = "outside")
) %>%
  distinct(geometry, .keep_all = TRUE)  # Remove duplicate geometries

# Now use the combined layer
parks <- parks_combined %>%
  mutate(
    area_acres = as.numeric(st_area(.)) / 43560,
    buffer_distance = case_when(
      area_acres >= 1000 ~ 5280 * 5,      # Regional: 5 miles
      area_acres >= 50 ~ 5280 * 3,        # Community: 3 miles
      TRUE ~ 5280 * 0.5                   # Neighborhood: 0.5 mile
    )
  )

# Load building population (ONLY within P4)
buildings <- st_read("data/raw/BuildingPopulation/Bldgs_Pop_Clip.shp") %>%
  st_transform(2278) %>%
  st_intersection(precinct4)

# Calculate population per pixel
building_to_grid <- st_join(buildings, reference_grid %>% select(pixel_d))

pixel_population <- building_to_grid %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(population = sum(Est_Night_, na.rm = TRUE), .groups = "drop")

# For each pixel, calculate accessible park acres
cat("Calculating park accessibility...\n")

park_access_list <- list()

for(i in 1:nrow(reference_grid)) {
  pixel_centroid <- st_centroid(reference_grid[i, ])
  pixel_id <- reference_grid$pixel_d[i]
  
  distances <- as.numeric(st_distance(pixel_centroid, parks))
  accessible <- distances <= parks$buffer_distance
  total_accessible_acres <- sum(parks$area_acres[accessible], na.rm = TRUE)
  
  park_access_list[[i]] <- data.frame(
    pixel_d = pixel_id,
    accessible_park_acres = total_accessible_acres
  )
  
  if(i %% 500 == 0) cat("  ", i, "/", nrow(reference_grid), "\n")
}

park_access_summary <- bind_rows(park_access_list)

# Calculate acres per capita
park_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(pixel_population, by = "pixel_d") %>%
  left_join(park_access_summary, by = "pixel_d") %>%
  mutate(
    population = coalesce(population, 0),
    accessible_park_acres = coalesce(accessible_park_acres, 0),
    park_acres_per_capita = ifelse(population > 0, 
                                   accessible_park_acres / population, 
                                   0),
    park_score = ifelse(population > 0,
                        1 - scales::rescale(park_acres_per_capita, to = c(0, 1)),
                        NA)
  ) %>%
  select(pixel_d, population, accessible_park_acres, park_acres_per_capita, park_score)

# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(park_export, by = "pixel_d")

write_csv(cip, "results/CIP.csv")

cat("✓ Park accessibility data added to CIP.csv\n")

# =============================================================================
# TRAIL ACCESSIBILITY - MILES PER CAPITA
# =============================================================================

# Load trails
trails <- st_read("data/raw/Trails/Trails.shp") %>%
  st_transform(2278) %>%
  mutate(length_miles = as.numeric(st_length(.)) / 5280)

# Buffer trails by 0.5 miles (10-minute walk)
trails_buffered <- st_buffer(trails, 5280 * 0.5)

# Load building population
buildings <- st_read("data/raw/BuildingPopulation/Bldgs_Pop_Clip.shp") %>%
  st_transform(2278)

# Calculate population per pixel
building_to_grid <- st_join(buildings, reference_grid %>% select(pixel_d))
pixel_population <- building_to_grid %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(population = sum(Est_Night_, na.rm = TRUE), .groups = "drop")

# Calculate accessible trail miles per pixel
trail_access_list <- list()

for(i in 1:nrow(reference_grid)) {
  pixel_id <- reference_grid$pixel_d[i]
  intersects <- st_intersects(reference_grid[i, ], trails_buffered, sparse = FALSE)[1,]
  total_accessible_miles <- sum(trails$length_miles[intersects], na.rm = TRUE)
  
  trail_access_list[[i]] <- data.frame(
    pixel_d = pixel_id,
    accessible_trail_miles = total_accessible_miles
  )
  
  if(i %% 500 == 0) cat("  ", i, "/", nrow(reference_grid), "\n")
}

trail_access_summary <- bind_rows(trail_access_list)

# Calculate score with clamping
trail_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(pixel_population, by = "pixel_d") %>%
  left_join(trail_access_summary, by = "pixel_d") %>%
  mutate(
    population = coalesce(population, 0),
    accessible_trail_miles = coalesce(accessible_trail_miles, 0),
    trail_miles_per_capita = ifelse(population > 0, 
                                    accessible_trail_miles / population, 
                                    0),
    # Clamp at 95th percentile, then rescale and invert
    trail_miles_capped = pmin(trail_miles_per_capita, 
                              quantile(trail_miles_per_capita[population > 0], 0.95, na.rm = TRUE)),
    trail_score = ifelse(population > 0,
                         1 - scales::rescale(trail_miles_capped, to = c(0, 1)),
                         NA)
  ) %>%
  select(pixel_d, population, accessible_trail_miles, trail_miles_per_capita, trail_score)

# Save to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(trail_export, by = "pixel_d")
write_csv(cip, "results/CIP.csv")

cat("✓ Trail accessibility data added to CIP.csv\n")

# =============================================================================
# BUILDING GROWTH ANALYSIS (2015-2025) - HCAD
# =============================================================================

# Load parcels
parcels <- st_read("data/raw/Parcels/HCAD_PDATA/Parcels/Parcels.shp")

# Load building files
building_res <- read_delim("data/raw/Parcels/HCAD_PDATA/BI2025/building_res.txt", 
                           delim = "\t", col_types = cols(.default = "c"))
building_other <- read_delim("data/raw/Parcels/HCAD_PDATA/BI2025/building_other.txt", 
                             delim = "\t", col_types = cols(.default = "c"))

# Clean and standardize account numbers
clean_acct <- function(x) {
  x %>% as.character() %>% str_trim() %>% str_pad(width = 13, side = "left", pad = "0")
}

parcels <- parcels %>% mutate(HCAD_NUM_clean = clean_acct(HCAD_NUM))
building_res <- building_res %>% mutate(acct_clean = clean_acct(acct))
building_other <- building_other %>% mutate(acct_clean = clean_acct(acct))

# Combine buildings and filter for 2015-2025
buildings_combined <- bind_rows(
  building_res %>% select(acct_clean, date_erected, im_sq_ft, dpr_val),
  building_other %>% select(acct_clean, date_erected, im_sq_ft, Depr_Val) %>% rename(dpr_val = Depr_Val)
) %>%
  filter(!is.na(date_erected), date_erected >= "2015", date_erected <= "2025")

# Aggregate by parcel
growth_by_parcel <- buildings_combined %>%
  group_by(acct_clean) %>%
  summarize(
    new_buildings_count = n(),
    total_new_area_sqft = sum(as.numeric(im_sq_ft), na.rm = TRUE),
    total_new_value = sum(as.numeric(dpr_val), na.rm = TRUE),
    .groups = "drop"
  )

# Join to parcels
parcels_with_growth <- parcels %>%
  left_join(growth_by_parcel, by = c("HCAD_NUM_clean" = "acct_clean")) %>%
  mutate(
    new_buildings_count = coalesce(new_buildings_count, 0),
    total_new_area_sqft = coalesce(total_new_area_sqft, 0),
    total_new_value = coalesce(total_new_value, 0)
  )

# Spatial join to grid (vectorized)
growth_by_grid <- st_join(reference_grid, parcels_with_growth) %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarize(
    total_new_buildings = sum(new_buildings_count, na.rm = TRUE),
    total_new_area = sum(total_new_area_sqft, na.rm = TRUE),
    total_new_value = sum(total_new_value, na.rm = TRUE),
    .groups = "drop"
  )

# Join to ALL grid cells first
growth_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(growth_by_grid, by = "pixel_d") %>%
  mutate(
    # Fill NAs with 0 FIRST
    total_new_buildings = coalesce(total_new_buildings, 0),
    total_new_area = coalesce(total_new_area, 0),
    total_new_value = coalesce(total_new_value, 0),
    # THEN calculate percentiles (now includes all the zeros)
    bldg_pct = percent_rank(total_new_buildings),
    area_pct = percent_rank(total_new_area),
    value_pct = percent_rank(total_new_value)
  )

# Save to CIP
cip <- read_csv("results/CIP.csv")
cip <- cip %>% left_join(growth_export, by = "pixel_d")
write_csv(cip, "results/CIP.csv")

# =============================================================================
# BUILDING GROWTH ANALYSIS (2016-2025)
# =============================================================================

# Load permits
permits <- st_read("data/raw/Pct4DevPermits/Pct4DevPermits.shp") %>%
  st_transform(2278)

# Spatial join: count permits per grid cell
permit_counts <- st_join(
  permits,  
  reference_grid %>% select(pixel_d),
  join = st_within
) %>%
  st_set_geometry(NULL) %>%
  group_by(pixel_d) %>%
  summarise(permit_count = n(), .groups = "drop")

# Join back to reference grid and normalize
permit_export <- reference_grid %>%
  st_set_geometry(NULL) %>%
  select(pixel_d) %>%
  left_join(permit_counts, by = "pixel_d") %>%
  mutate(
    permit_count = replace_na(permit_count, 0),
    permit_score = permit_count / max(permit_count, na.rm = TRUE)  # 0-1 scale, 1 = more permits
  )
if (interactive()) utils::View(permits)
if (interactive()) utils::View(permit_export)
# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(permit_export, by = "pixel_d")
write_csv(cip, "results/CIP.csv")

cat("✓ Permits data added to CIP.csv\n")

# =============================================================================
# POPULATION GROWTH (2010-2020)
# =============================================================================

# Read relationship file for block crosswalk
rel_file <- read_delim(
  "data/raw/tab2010_tab2020_st48_tx.txt",
  delim = "|", col_types = cols(.default = "c")
) %>%
  filter(COUNTY_2010 == "201" | COUNTY_2020 == "201") %>%
  mutate(
    GEOID_2010 = paste0(STATE_2010, COUNTY_2010, TRACT_2010, BLK_2010),
    GEOID_2020 = paste0(STATE_2020, COUNTY_2020, TRACT_2020, BLK_2020),
    AREALAND_INT_num = as.numeric(AREALAND_INT),
    AREALAND_2010_num = as.numeric(AREALAND_2010),
    weight = ifelse(AREALAND_2010_num > 0, AREALAND_INT_num / AREALAND_2010_num, 0)
  ) %>%
  select(GEOID_2010, GEOID_2020, weight)

# Get population data
pop_2020 <- get_decennial(
  geography = "block", variables = "P1_001N", year = 2020,
  state = "48", county = "201", sumfile = "pl", geometry = TRUE
) %>%
  select(GEOID_2020 = GEOID, geometry, pop_2020 = value)

pop_2010 <- get_decennial(
  geography = "block", variables = "P001001", year = 2010,
  state = "48", county = "201", sumfile = "sf1", geometry = FALSE
) %>%
  select(GEOID_2010 = GEOID, pop_2010 = value)

# Allocate 2010 to 2020 boundaries
pop_allocated <- rel_file %>%
  left_join(pop_2010, by = "GEOID_2010") %>%
  mutate(pop_2010_allocated = pop_2010 * weight) %>%
  group_by(GEOID_2020) %>%
  summarize(pop_2010_adjusted = sum(pop_2010_allocated, na.rm = TRUE), .groups = "drop")

# Calculate population change
pop_change <- pop_2020 %>%
  left_join(pop_allocated, by = "GEOID_2020") %>%
  mutate(
    pop_2010_adjusted = coalesce(pop_2010_adjusted, 0),
    pop_change = pop_2020 - pop_2010_adjusted
  ) %>%
  st_transform(st_crs(reference_grid))

# Aggregate to grid
grid_pop_change <- st_join(pop_change, reference_grid %>% select(pixel_d)) %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarize(
    pop_2010 = sum(pop_2010_adjusted, na.rm = TRUE),
    pop_2020 = sum(pop_2020, na.rm = TRUE),
    pop_change = sum(pop_change, na.rm = TRUE),
    .groups = "drop"
  )

# Join to all grid cells and calculate percentile
pop_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(grid_pop_change, by = "pixel_d") %>%
  mutate(
    pop_2010 = coalesce(pop_2010, 0),
    pop_2020 = coalesce(pop_2020, 0),
    pop_change = coalesce(pop_change, 0),
    pop_growth_score = percent_rank(pop_change)
  ) %>%
  select(pixel_d, pop_2010, pop_2020, pop_change, pop_growth_score)

# Save to CIP
cip <- read_csv("results/CIP.csv")
cip <- cip %>% left_join(pop_export, by = "pixel_d")
write_csv(cip, "results/CIP.csv")

# =============================================================================
# TRAFFIC UTILIZATION (VMT PER LANE)
# =============================================================================

# Clip roadway inventory to P4
txdot_roads <- st_read("data/raw/Transportation/TxDOT_Roadway_Inventory/Roadway_Inventory.shp") %>%
  st_transform(2278) %>%
  st_filter(precinct4)

# Clip AADT to P4
statewide_aadt <- st_read("data/raw/Transportation/TxDOT_5_Year_Statewide_AADT_Traffic_Counts/TMB_RPT_SHORT_AADT_HIST.shp") %>%
  st_transform(2278) %>%
  st_filter(precinct4) %>%
  filter(!is.na(AADT_RPT_2), AADT_RPT_2 > 0)

# Join AADT to roads
aadt_clean <- statewide_aadt %>%
  st_drop_geometry() %>%
  select(TRFC_STATN, AADT_RPT_2)

roads_with_aadt <- txdot_roads %>%
  left_join(aadt_clean, by = c("TRF_STA_ID" = "TRFC_STATN"))

# Intersect with grid
roads_grid <- st_intersection(roads_with_aadt, reference_grid %>% select(pixel_d)) %>%
  mutate(
    seg_length_mi = as.numeric(st_length(geometry)) / 5280,
    lane_miles = seg_length_mi * NUM_LANES,
    vmt = AADT_RPT_2 * seg_length_mi
  ) %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarize(
    lane_miles = sum(lane_miles, na.rm = TRUE),
    total_vmt = sum(vmt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(vmt_per_lane = ifelse(lane_miles > 0, total_vmt / lane_miles, NA))

# Join and scale
traffic_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(roads_grid, by = "pixel_d") %>%
  mutate(
    lane_miles = coalesce(lane_miles, 0),
    total_vmt = coalesce(total_vmt, 0),
    traffic_score = percent_rank(vmt_per_lane)  # Rank all, NAs stay NA
  ) %>%
  mutate(
    traffic_score = coalesce(traffic_score, 0)  # Replace NAs with 0
  ) %>%
  select(pixel_d, lane_miles, total_vmt, vmt_per_lane, traffic_score)

# Save to CIP
cip <- read_csv("results/CIP.csv")
cip <- cip %>% left_join(traffic_export, by = "pixel_d")
write_csv(cip, "results/CIP.csv")

# =============================================================================
# LEAPFROG DEVELOPMENT - LANE MILES PER HOUSING UNIT
# =============================================================================

# Helper function
clean_acct <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_pad(width = 13, side = "left", pad = "0")
}

# Load parcels
parcels <- st_read("data/raw/Parcels/Parcels_2020_Oct/Parcels.shp") %>%
  st_transform(2278) %>%
  mutate(HCAD_NUM_clean = clean_acct(HCAD_NUM))

# Load buildings and calculate housing units
buildings <- read_delim("data/raw/Parcels/HCAD_PDATA/BI2025/building_res.txt", 
                        delim = "\t",
                        col_types = cols(.default = "c"),
                        locale = locale(encoding = "latin1")) %>%
  mutate(
    acct_clean = clean_acct(acct),
    property_use_cd = str_trim(property_use_cd)
  ) %>%
  filter(property_use_cd %in% c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", 
                                "Z0", "Z1", "Z2", "Z3", "Z4", "Z5")) %>%
  mutate(units = case_when(
    property_use_cd == "B2" ~ 2,
    property_use_cd == "B3" ~ 3,
    property_use_cd == "B4" ~ 4,
    property_use_cd == "B1" ~ 4,
    TRUE ~ 1
  )) %>%
  group_by(acct_clean) %>%
  summarise(total_units = sum(units, na.rm = TRUE), .groups = "drop")

# Join to parcels and aggregate to grid
parcels_with_units <- parcels %>%
  left_join(buildings, by = c("HCAD_NUM_clean" = "acct_clean")) %>%
  mutate(total_units = replace_na(total_units, 0))

grid_housing <- st_join(parcels_with_units, reference_grid %>% select(pixel_d)) %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(housing_units = sum(total_units, na.rm = TRUE), .groups = "drop")

# Load roads and calculate lane miles
roads <- st_read("data/raw/Transportation/TxDOT_Roadway_Inventory/Roadway_Inventory.shp") %>%
  st_transform(2278)

roads_grid <- st_intersection(roads, reference_grid %>% select(pixel_d)) %>%
  mutate(
    segment_length_miles = as.numeric(st_length(geometry)) / 5280,
    num_lanes = as.numeric(NUM_LANES),
    segment_lane_miles = segment_length_miles * num_lanes
  ) %>%
  st_drop_geometry() %>%
  group_by(pixel_d) %>%
  summarise(total_lane_miles = sum(segment_lane_miles, na.rm = TRUE), .groups = "drop")

# Combine and calculate leapfrog metric
leapfrog_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(grid_housing, by = "pixel_d") %>%
  left_join(roads_grid, by = "pixel_d") %>%
  mutate(
    housing_units = coalesce(housing_units, 0),
    total_lane_miles = coalesce(total_lane_miles, 0),
    
    # Step 1: Normalize housing units (0-1)
    housing_normalized = rescale(housing_units, to = c(0, 1)),
    
    # Step 2: Normalize lane miles (0-1)
    lane_miles_normalized = rescale(total_lane_miles, to = c(0, 1)),
    
    # Step 3: Divide normalized values
    ratio_normalized = if_else(housing_normalized > 0,
                               lane_miles_normalized / housing_normalized,
                               NA_real_),
    
    # Step 4: Scale the ratio to 0-1 and INVERT
    # High ratio (low housing) → closer to 0
    leapfrog_score = case_when(
      housing_units == 0 ~ 0,
      TRUE ~ 1 - rescale(ratio_normalized, to = c(0, 1))
    )
  ) %>%
  mutate(leapfrog_score = coalesce(leapfrog_score, 0)) %>%
  select(pixel_d, housing_units, total_lane_miles, ratio_normalized, leapfrog_score)

cat("✓ Leapfrog development data added to CIP.csv\n")


# Quick check map
grid_viz <- reference_grid %>%
  left_join(leapfrog_export, by = "pixel_d")

ggplot(grid_viz) +
  geom_sf(aes(fill = leapfrog_score), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Score") +
  theme_minimal() +
  labs(title = "Leapfrog Development Score") +
  theme(panel.grid = element_blank(), axis.text = element_blank())

# Quick stats
summary(grid_viz$leapfrog_score)
if (interactive()) utils::View(leapfrog_export)
# Add to CIP.csv
cip <- read_csv("results/CIP.csv")
cip <- cip %>%
  left_join(leapfrog_export, by = "pixel_d")

write_csv(cip, "results/CIP03.csv")



leapfrog_export <- reference_grid %>%
  st_drop_geometry() %>%
  select(pixel_d) %>%
  left_join(grid_housing, by = "pixel_d") %>%
  left_join(roads_grid, by = "pixel_d") %>%
  mutate(
    housing_units = coalesce(housing_units, 0),
    total_lane_miles = coalesce(total_lane_miles, 0),
    
    # Step 1: Normalize housing units (0-1)
    housing_normalized = rescale(housing_units, to = c(0, 1)),
    
    # Step 2: Normalize lane miles (0-1)
    lane_miles_normalized = rescale(total_lane_miles, to = c(0, 1)),
    
    # Step 3: Divide normalized values
    ratio_normalized = if_else(housing_normalized > 0,
                               lane_miles_normalized / housing_normalized,
                               NA_real_),
    
    # Step 4: Scale the ratio to 0-1 and INVERT
    # High ratio (low housing) → closer to 0
    leapfrog_score = case_when(
      housing_units == 0 ~ 0,
      TRUE ~ 1 - rescale(ratio_normalized, to = c(0, 1))
    )
  ) %>%
  mutate(leapfrog_score = coalesce(leapfrog_score, 0)) %>%
  select(pixel_d, housing_units, total_lane_miles, ratio_normalized, leapfrog_score)

cat("✓ Leapfrog development data added to CIP.csv\n")



