# ============================================================
# 06_create_spatial_network_outputs.R
# Create spatial network outputs for assigned SCD hub-and-spoke model
# ============================================================

# Purpose:
# Create spatial layers for the primary assigned NBS-to-SCD-care network.
#
# Inputs:
# - 02_processed_data/routes/assigned_spoke_hub_routes_primary.csv
# - 02_processed_data/routes/hub_workload_route_risk_primary.csv
#
# Outputs:
# - 02_processed_data/spatial/scd_hub_spoke_network_outputs.gpkg
# - 02_processed_data/spatial/assigned_spoke_hub_routes_primary_lines.gpkg
# - 02_processed_data/spatial/assigned_spokes_primary_points.gpkg
# - 02_processed_data/spatial/assigned_hubs_primary_points.gpkg
# - 04_outputs/tables/06_spatial_network_outputs_summary.xlsx

# ----------------------------
# 1. Load packages
# ----------------------------

required_packages <- c(
  "readr",
  "dplyr",
  "stringr",
  "janitor",
  "sf",
  "tibble",
  "writexl",
  "tidyr",
  "purrr"
)

installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(sf)
library(tibble)
library(writexl)
library(tidyr)
library(purrr)

# ----------------------------
# 2. Define paths
# ----------------------------

project_root <- "scd_hub_spoke_model"

processed_route_dir <- file.path(
  project_root,
  "02_processed_data",
  "routes"
)

processed_spatial_dir <- file.path(
  project_root,
  "02_processed_data",
  "spatial"
)

output_table_dir <- file.path(
  project_root,
  "04_outputs",
  "tables"
)

log_dir <- file.path(
  project_root,
  "04_outputs",
  "logs"
)

admin_dir <- file.path(
  project_root,
  "00_admin"
)

for (d in c(
  processed_spatial_dir,
  output_table_dir,
  log_dir,
  admin_dir
)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

assigned_primary_file <- file.path(
  processed_route_dir,
  "assigned_spoke_hub_routes_primary.csv"
)

hub_workload_file <- file.path(
  processed_route_dir,
  "hub_workload_route_risk_primary.csv"
)

required_files <- c(
  assigned_primary_file,
  hub_workload_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required file(s):\n",
      paste(missing_files, collapse = "\n"),
      "\n\nRun Scripts 04 and 05 first."
    )
  )
}

# ----------------------------
# 3. Helper functions
# ----------------------------

valid_uganda_coordinates <- function(lon, lat) {
  !is.na(lon) &
    !is.na(lat) &
    lon >= 29 &
    lon <= 36 &
    lat >= -2 &
    lat <= 5
}

safe_chr_col <- function(data, col_options) {
  existing <- intersect(col_options, names(data))

  if (length(existing) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  as.character(data[[existing[1]]])
}

safe_num_col <- function(data, col_options) {
  existing <- intersect(col_options, names(data))

  if (length(existing) == 0) {
    return(rep(NA_real_, nrow(data)))
  }

  suppressWarnings(as.numeric(data[[existing[1]]]))
}

safe_lgl_col <- function(data, col_options) {
  existing <- intersect(col_options, names(data))

  if (length(existing) == 0) {
    return(rep(FALSE, nrow(data)))
  }

  x <- data[[existing[1]]]

  if (is.logical(x)) {
    return(x)
  }

  x_chr <- stringr::str_to_lower(as.character(x))

  dplyr::case_when(
    x_chr %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ FALSE
  )
}

make_route_line <- function(spoke_lon, spoke_lat, hub_lon, hub_lat) {
  sf::st_linestring(
    matrix(
      c(
        spoke_lon, spoke_lat,
        hub_lon, hub_lat
      ),
      ncol = 2,
      byrow = TRUE
    )
  )
}

# ----------------------------
# 4. Read inputs
# ----------------------------

assigned_primary <- read_csv(
  assigned_primary_file,
  show_col_types = FALSE
) %>%
  clean_names()

hub_workload <- read_csv(
  hub_workload_file,
  show_col_types = FALSE
) %>%
  clean_names()

message("Rows read from primary assignment: ", nrow(assigned_primary))
message("Rows read from hub workload file: ", nrow(hub_workload))

message("Primary assignment columns:")
print(names(assigned_primary))

message("Hub workload columns:")
print(names(hub_workload))

# ----------------------------
# 5. Validate required columns
# ----------------------------

required_assignment_cols <- c(
  "spoke_id",
  "spoke_name",
  "spoke_type",
  "point_longitude",
  "point_latitude",
  "hub_id",
  "assigned_hub_name",
  "assigned_hub_level",
  "longitude",
  "latitude",
  "assigned_distance_km",
  "assigned_distance_band",
  "route_feasibility",
  "route_feasibility_group",
  "weak_or_long_route",
  "long_route",
  "very_long_route",
  "cl_binary",
  "hu_binary",
  "nbs_binary",
  "tr_binary",
  "hub_service_score",
  "hub_readiness"
)

missing_assignment_cols <- setdiff(
  required_assignment_cols,
  names(assigned_primary)
)

if (length(missing_assignment_cols) > 0) {
  stop(
    paste0(
      "Primary assignment file is missing required columns:\n",
      paste(missing_assignment_cols, collapse = "\n"),
      "\n\nColumns found:\n",
      paste(names(assigned_primary), collapse = ", ")
    )
  )
}

required_workload_cols <- c(
  "hub_id",
  "assigned_hub_name",
  "linked_spokes",
  "hub_service_score",
  "hub_readiness",
  "workload_category",
  "route_risk_category",
  "hub_priority_flag"
)

missing_workload_cols <- setdiff(
  required_workload_cols,
  names(hub_workload)
)

if (length(missing_workload_cols) > 0) {
  stop(
    paste0(
      "Hub workload file is missing required columns:\n",
      paste(missing_workload_cols, collapse = "\n"),
      "\n\nColumns found:\n",
      paste(names(hub_workload), collapse = ", ")
    )
  )
}

# ----------------------------
# 6. Standardise primary assignment fields
# ----------------------------

assigned_primary <- assigned_primary %>%
  mutate(
    point_longitude = suppressWarnings(as.numeric(point_longitude)),
    point_latitude = suppressWarnings(as.numeric(point_latitude)),
    hub_longitude = suppressWarnings(as.numeric(longitude)),
    hub_latitude = suppressWarnings(as.numeric(latitude)),
    assigned_distance_km = suppressWarnings(as.numeric(assigned_distance_km)),

    spoke_coordinate_valid = valid_uganda_coordinates(
      point_longitude,
      point_latitude
    ),

    hub_coordinate_valid = valid_uganda_coordinates(
      hub_longitude,
      hub_latitude
    ),

    route_coordinate_valid = spoke_coordinate_valid & hub_coordinate_valid,

    weak_or_long_route = safe_lgl_col(
      cur_data(),
      c("weak_or_long_route")
    ),

    long_route = safe_lgl_col(
      cur_data(),
      c("long_route")
    ),

    very_long_route = safe_lgl_col(
      cur_data(),
      c("very_long_route")
    ),

    assigned_hub_ownership = safe_chr_col(
      cur_data(),
      c("assigned_hub_ownership", "ownership")
    ),

    assigned_hub_subregion = safe_chr_col(
      cur_data(),
      c("assigned_hub_subregion", "subregion")
    ),

    existing_nbs = suppressWarnings(as.integer(existing_nbs)),
    collocated_candidate_nbs = suppressWarnings(as.integer(collocated_candidate_nbs)),

    cl_binary = suppressWarnings(as.integer(cl_binary)),
    hu_binary = suppressWarnings(as.integer(hu_binary)),
    nbs_binary = suppressWarnings(as.integer(nbs_binary)),
    tr_binary = suppressWarnings(as.integer(tr_binary)),
    hub_service_score = suppressWarnings(as.integer(hub_service_score))
  )

invalid_routes <- assigned_primary %>%
  filter(!route_coordinate_valid)

if (nrow(invalid_routes) > 0) {
  stop(
    paste0(
      "Some assigned routes have invalid spoke or hub coordinates: ",
      nrow(invalid_routes),
      ". Review assigned_spoke_hub_routes_primary.csv."
    )
  )
}

# ----------------------------
# 7. Create assigned spoke point layer
# ----------------------------

assigned_spokes <- assigned_primary %>%
  distinct(
    spoke_id,
    .keep_all = TRUE
  ) %>%
  select(
    spoke_id,
    spoke_name,
    spoke_type,
    existing_nbs,
    collocated_candidate_nbs,
    matched_facility_id,
    matched_facility_level,
    matched_district,
    point_longitude,
    point_latitude,
    hub_id,
    assigned_hub_name,
    assigned_distance_km,
    assigned_distance_band,
    route_feasibility,
    route_feasibility_group,
    weak_or_long_route,
    long_route,
    very_long_route
  ) %>%
  st_as_sf(
    coords = c("point_longitude", "point_latitude"),
    crs = 4326,
    remove = FALSE
  )

# ----------------------------
# 8. Create assigned hub point layer
# ----------------------------

assigned_hubs <- assigned_primary %>%
  distinct(
    hub_id,
    .keep_all = TRUE
  ) %>%
  select(
    hub_id,
    assigned_hub_name,
    assigned_hub_level,
    assigned_hub_ownership,
    assigned_hub_subregion,
    hub_longitude,
    hub_latitude,
    cl_binary,
    hu_binary,
    nbs_binary,
    tr_binary,
    hub_service_score,
    hub_service_deficit,
    hub_readiness
  ) %>%
  st_as_sf(
    coords = c("hub_longitude", "hub_latitude"),
    crs = 4326,
    remove = FALSE
  )

# ----------------------------
# 9. Attach coordinates to hub workload and create workload point layer
# ----------------------------

hub_coordinates <- assigned_primary %>%
  distinct(
    hub_id,
    .keep_all = TRUE
  ) %>%
  select(
    hub_id,
    assigned_hub_name,
    assigned_hub_level,
    assigned_hub_ownership,
    assigned_hub_subregion,
    hub_longitude,
    hub_latitude
  )

hub_workload_points <- hub_workload %>%
  left_join(
    hub_coordinates,
    by = "hub_id"
  ) %>%
  mutate(
    hub_longitude = suppressWarnings(as.numeric(hub_longitude)),
    hub_latitude = suppressWarnings(as.numeric(hub_latitude)),
    coordinate_valid = valid_uganda_coordinates(
      hub_longitude,
      hub_latitude
    )
  ) %>%
  filter(coordinate_valid) %>%
  st_as_sf(
    coords = c("hub_longitude", "hub_latitude"),
    crs = 4326,
    remove = FALSE
  )

# ----------------------------
# 10. Create assigned route line layer
# ----------------------------

route_geometries <- purrr::pmap(
  list(
    assigned_primary$point_longitude,
    assigned_primary$point_latitude,
    assigned_primary$hub_longitude,
    assigned_primary$hub_latitude
  ),
  make_route_line
)

assigned_route_lines <- assigned_primary %>%
  select(
    spoke_id,
    spoke_name,
    spoke_type,
    existing_nbs,
    collocated_candidate_nbs,
    point_longitude,
    point_latitude,
    hub_id,
    assigned_hub_name,
    assigned_hub_level,
    assigned_hub_ownership,
    assigned_hub_subregion,
    hub_longitude,
    hub_latitude,
    assigned_distance_km,
    assigned_distance_band,
    route_feasibility,
    route_feasibility_group,
    weak_or_long_route,
    long_route,
    very_long_route,
    cl_binary,
    hu_binary,
    nbs_binary,
    tr_binary,
    hub_service_score,
    hub_readiness
  ) %>%
  st_as_sf(
    geometry = st_sfc(
      route_geometries,
      crs = 4326
    )
  )

# ----------------------------
# 11. Project metric copies for analysis and map export
# ----------------------------

assigned_spokes_utm <- assigned_spokes %>%
  st_transform(32636)

assigned_hubs_utm <- assigned_hubs %>%
  st_transform(32636)

hub_workload_points_utm <- hub_workload_points %>%
  st_transform(32636)

assigned_route_lines_utm <- assigned_route_lines %>%
  st_transform(32636)

# ----------------------------
# 12. Create spatial output summaries
# ----------------------------

spatial_output_summary <- tibble(
  layer_name = c(
    "assigned_spokes_primary_points",
    "assigned_hubs_primary_points",
    "hub_workload_primary_points",
    "assigned_spoke_hub_routes_primary_lines"
  ),
  feature_count = c(
    nrow(assigned_spokes),
    nrow(assigned_hubs),
    nrow(hub_workload_points),
    nrow(assigned_route_lines)
  ),
  crs_epsg = c(
    4326,
    4326,
    4326,
    4326
  ),
  description = c(
    "Assigned NBS spoke point layer using accepted geocoded spokes",
    "Assigned eligible SCD care hub point layer under primary scenario",
    "Hub workload and route-risk point layer under primary scenario",
    "Straight-line assigned spoke-to-hub route layer under primary scenario"
  )
)

spatial_qc <- tibble(
  metric = c(
    "assigned_spoke_points",
    "assigned_hub_points",
    "hub_workload_points",
    "assigned_route_lines",
    "weak_or_long_route_lines",
    "long_route_lines",
    "very_long_route_lines",
    "unique_spokes_in_routes",
    "unique_hubs_in_routes"
  ),
  value = c(
    nrow(assigned_spokes),
    nrow(assigned_hubs),
    nrow(hub_workload_points),
    nrow(assigned_route_lines),
    sum(assigned_route_lines$weak_or_long_route, na.rm = TRUE),
    sum(assigned_route_lines$long_route, na.rm = TRUE),
    sum(assigned_route_lines$very_long_route, na.rm = TRUE),
    n_distinct(assigned_route_lines$spoke_id),
    n_distinct(assigned_route_lines$hub_id)
  )
)

print(spatial_output_summary)
print(spatial_qc)

# ----------------------------
# 13. Save GeoPackage outputs
# ----------------------------

network_gpkg <- file.path(
  processed_spatial_dir,
  "scd_hub_spoke_network_outputs.gpkg"
)

if (file.exists(network_gpkg)) {
  file.remove(network_gpkg)
}

st_write(
  assigned_spokes,
  network_gpkg,
  layer = "assigned_spokes_primary_points",
  delete_layer = TRUE,
  quiet = TRUE
)

st_write(
  assigned_hubs,
  network_gpkg,
  layer = "assigned_hubs_primary_points",
  append = TRUE,
  quiet = TRUE
)

st_write(
  hub_workload_points,
  network_gpkg,
  layer = "hub_workload_primary_points",
  append = TRUE,
  quiet = TRUE
)

st_write(
  assigned_route_lines,
  network_gpkg,
  layer = "assigned_routes_primary_lines",
  append = TRUE,
  quiet = TRUE
)

# Also save metric CRS layers inside the same GeoPackage.
st_write(
  assigned_spokes_utm,
  network_gpkg,
  layer = "assigned_spokes_primary_points_utm32636",
  append = TRUE,
  quiet = TRUE
)

st_write(
  assigned_hubs_utm,
  network_gpkg,
  layer = "assigned_hubs_primary_points_utm32636",
  append = TRUE,
  quiet = TRUE
)

st_write(
  hub_workload_points_utm,
  network_gpkg,
  layer = "hub_workload_primary_points_utm32636",
  append = TRUE,
  quiet = TRUE
)

st_write(
  assigned_route_lines_utm,
  network_gpkg,
  layer = "assigned_routes_primary_lines_utm32636",
  append = TRUE,
  quiet = TRUE
)

# Separate GeoPackages for easy GIS loading.
route_lines_gpkg <- file.path(
  processed_spatial_dir,
  "assigned_spoke_hub_routes_primary_lines.gpkg"
)

spoke_points_gpkg <- file.path(
  processed_spatial_dir,
  "assigned_spokes_primary_points.gpkg"
)

hub_points_gpkg <- file.path(
  processed_spatial_dir,
  "assigned_hubs_primary_points.gpkg"
)

if (file.exists(route_lines_gpkg)) file.remove(route_lines_gpkg)
if (file.exists(spoke_points_gpkg)) file.remove(spoke_points_gpkg)
if (file.exists(hub_points_gpkg)) file.remove(hub_points_gpkg)

st_write(
  assigned_route_lines,
  route_lines_gpkg,
  layer = "assigned_routes_primary_lines",
  delete_layer = TRUE,
  quiet = TRUE
)

st_write(
  assigned_spokes,
  spoke_points_gpkg,
  layer = "assigned_spokes_primary_points",
  delete_layer = TRUE,
  quiet = TRUE
)

st_write(
  assigned_hubs,
  hub_points_gpkg,
  layer = "assigned_hubs_primary_points",
  delete_layer = TRUE,
  quiet = TRUE
)

# ----------------------------
# 14. Save tabular summaries
# ----------------------------

write_csv(
  spatial_output_summary,
  file.path(
    processed_spatial_dir,
    "spatial_network_outputs_summary.csv"
  )
)

write_csv(
  spatial_qc,
  file.path(
    log_dir,
    "06_spatial_network_outputs_qc.csv"
  )
)

write_xlsx(
  list(
    spatial_output_summary = spatial_output_summary,
    spatial_qc = spatial_qc
  ),
  file.path(
    output_table_dir,
    "06_spatial_network_outputs_summary.xlsx"
  )
)

# ----------------------------
# 15. Log
# ----------------------------

log_path <- file.path(
  admin_dir,
  "analysis_log_updated_spoke_layer.md"
)

log_entry <- c(
  "",
  paste0("## ", Sys.Date(), " - Created spatial network outputs"),
  "",
  paste0("- Assigned spoke points: ", nrow(assigned_spokes), "."),
  paste0("- Assigned hub points: ", nrow(assigned_hubs), "."),
  paste0("- Hub workload points: ", nrow(hub_workload_points), "."),
  paste0("- Assigned route lines: ", nrow(assigned_route_lines), "."),
  paste0("- Weak or long route lines: ", sum(assigned_route_lines$weak_or_long_route, na.rm = TRUE), "."),
  paste0("- Network GeoPackage: ", network_gpkg)
)

cat(
  paste(log_entry, collapse = "\n"),
  file = log_path,
  append = TRUE
)

message("Spatial network outputs complete.")
message("Main network GeoPackage:")
message(network_gpkg)
message("Route lines:")
message(route_lines_gpkg)
message("Spoke points:")
message(spoke_points_gpkg)
message("Hub points:")
message(hub_points_gpkg)
message("Summary workbook:")
message(file.path(output_table_dir, "06_spatial_network_outputs_summary.xlsx"))