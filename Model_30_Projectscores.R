# Run from the folder containing these two scripts and README.md.
if (!file.exists("Model_28_CIP.R") || !file.exists("Model_30_Projectscores.R")) stop("Set the working directory to the folder containing both R scripts.")
dir.create("results", showWarnings = FALSE, recursive = TRUE)

library(sf)
library(dplyr)
library(readr)

# Load grid data from CIP.csv
cip_data <- read_csv("results/CIP03.csv")

# Load reference grid
reference_grid <- st_read("data/raw/Reference_Grid/P4_Reference_Grid.shp") %>%
  st_transform(2278)

# Join CIP data to grid
grid_with_scores <- reference_grid %>%
  left_join(cip_data, by = "pixel_d")

# Load projects
projects <- st_read("data/raw/InfrastructureProjects/Projects.shp") %>%
  st_transform(2278)

# Define the 16 score variables
score_vars <- c(
  "crash_score", "pci_score", "road_unlit_pct", "sidewalk_score",
  "rep_loss_score", "flood_score", "tree_canopy_score", "park_condition_score",
  "lst_score", "svi", "drainage_score", "park_score", "trail_score",
  "permit_score", "pop_growth_score", "traffic_score", "leapfrog_score"
)

# Intersect projects with grid
projects_grid_intersect <- st_intersection(
  projects %>% select(projname, geometry),
  grid_with_scores %>% select(pixel_d, all_of(score_vars))
)

# Calculate averages per project - DROP GEOMETRY HERE!
project_scores <- projects_grid_intersect %>%
  st_drop_geometry() %>%  # KEY: Drop geometry BEFORE summarizing
  group_by(projname) %>%
  summarise(
    crash_score_avg = mean(crash_score, na.rm = TRUE),
    pci_score_avg = mean(pci_score, na.rm = TRUE),
    road_unlit_pct_avg = mean(road_unlit_pct, na.rm = TRUE),
    sidewalk_score_avg = mean(sidewalk_score, na.rm = TRUE),
    rep_loss_score_avg = mean(rep_loss_score, na.rm = TRUE),
    flood_score_avg = mean(flood_score, na.rm = TRUE),
    tree_canopy_score_avg = mean(tree_canopy_score, na.rm = TRUE),
    park_condition_score_avg = mean(park_condition_score, na.rm = TRUE),
    lst_score_avg = mean(lst_score, na.rm = TRUE),
    svi_avg = mean(svi, na.rm = TRUE),
    drainage_score_avg = mean(drainage_score, na.rm = TRUE),
    park_score_avg = mean(park_score, na.rm = TRUE),
    trail_score_avg = mean(trail_score, na.rm = TRUE),
    permit_score_avg = mean(permit_score, na.rm = TRUE),
    pop_growth_score_avg = mean(pop_growth_score, na.rm = TRUE),
    traffic_score_avg = mean(traffic_score, na.rm = TRUE),
    leapfrog_score_avg = mean(leapfrog_score, na.rm = TRUE),
    intersecting_cells = paste(pixel_d, collapse = ", "),
    n_cells = n(),
    .groups = "drop"
  )

# Join back to projects - DROP GEOMETRY AGAIN!
projects_final <- projects %>%
  st_drop_geometry() %>%  # Drop geometry from projects
  left_join(project_scores, by = "projname")

# Verify before saving
cat("Final row count:", nrow(projects_final), "\n")

# Save as CSV
write_csv(projects_final, "results/Projects_Scores03.csv")
