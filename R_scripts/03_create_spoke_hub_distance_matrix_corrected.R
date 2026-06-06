# ============================================================
# 03_create_spoke_hub_distance_matrix.R
# Create spoke-to-hub distance matrices
# ============================================================

# Purpose:
# Create all-pair straight-line distance matrices between:
# - updated accepted geocoded NBS spokes, n = 168
# - essential-care candidate hubs, n = 280
#
# This version is robust to the actual accepted spoke file structure:
# source_facility_name, matched_facility_name, spoke_type,
# point_longitude, point_latitude, coordinate_valid
#
# It creates spoke_id, spoke_name, and coordinate_valid_final internally.

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
  "units"
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
library(units)

# ----------------------------
# 2. Define paths
# ----------------------------

project_root <- "scd_hub_spoke_model"

processed_facility_dir <- file.path(
  project_root,
  "02_processed_data",
  "facilities"
)

processed_route_dir <- file.path(
  project_root,
  "02_processed_data",
  "routes"
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
  processed_route_dir,
  output_table_dir,
  log_dir,
  admin_dir
)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

# ----------------------------
# 3. Define input files
# ----------------------------

spoke_file_options <- c(
  file.path(
    processed_facility_dir,
    "nbs_spoke_layer_geocoded_accepted.csv"
  ),
  file.path(
    project_root,
    "01_raw_data",
    "nbs_prioritisation",
    "nbs_spokes_geocoded_accepted.csv"
  ),
  file.path(
    project_root,
    "01_raw_data",
    "essential_care_prioritisation",
    "nbs_spokes_geocoded_accepted.csv"
  )
)

hub_file_options <- c(
  file.path(
    processed_facility_dir,
    "essential_care_hub_layer.csv"
  )
)

first_existing_file <- function(file_options, file_label) {
  existing_files <- file_options[file.exists(file_options)]

  if (length(existing_files) == 0) {
    stop(
      paste0(
        "Missing required file for ",
        file_label,
        ". Checked:\n",
        paste(file_options, collapse = "\n")
      )
    )
  }

  existing_files[1]
}

spoke_file <- first_existing_file(
  spoke_file_options,
  "accepted geocoded NBS spoke layer"
)

hub_file <- first_existing_file(
  hub_file_options,
  "essential-care hub layer"
)

message("Using spoke file: ", spoke_file)
message("Using hub file: ", hub_file)

# ----------------------------
# 4. Helper functions
# ----------------------------

valid_uganda_coordinates <- function(lon, lat) {
  !is.na(lon) &
    !is.na(lat) &
    lon >= 29 &
    lon <= 36 &
    lat >= -2 &
    lat <= 5
}

distance_band <- function(distance_km) {
  case_when(
    is.na(distance_km) ~ "missing",
    distance_km <= 25 ~ "0_to_25_km",
    distance_km > 25 & distance_km <= 50 ~ "25_to_50_km",
    distance_km > 50 & distance_km <= 100 ~ "50_to_100_km",
    distance_km > 100 ~ "over_100_km",
    TRUE ~ "missing"
  )
}

safe_min <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  min(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  median(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}

as_logical_safe <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  x_chr <- stringr::str_to_lower(as.character(x))

  case_when(
    x_chr %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ FALSE
  )
}

# ----------------------------
# 5. Read inputs
# ----------------------------

spokes_raw <- read_csv(
  spoke_file,
  show_col_types = FALSE
) %>%
  clean_names()

hubs_raw <- read_csv(
  hub_file,
  show_col_types = FALSE
) %>%
  clean_names()

message("Rows read from spoke file: ", nrow(spokes_raw))
message("Rows read from hub file: ", nrow(hubs_raw))

message("Spoke columns found:")
print(names(spokes_raw))

message("Hub columns found:")
print(names(hubs_raw))

# ----------------------------
# 6. Validate minimum spoke and hub columns
# ----------------------------

required_spoke_min_cols <- c(
  "source_facility_name",
  "spoke_type",
  "existing_nbs",
  "collocated_candidate_nbs",
  "point_longitude",
  "point_latitude",
  "coordinate_valid"
)

missing_spoke_min_cols <- setdiff(
  required_spoke_min_cols,
  names(spokes_raw)
)

if (length(missing_spoke_min_cols) > 0) {
  stop(
    paste0(
      "Spoke file is missing required minimum columns:\n",
      paste(missing_spoke_min_cols, collapse = "\n"),
      "\n\nColumns found:\n",
      paste(names(spokes_raw), collapse = ", ")
    )
  )
}

required_hub_cols <- c(
  "hub_id",
  "hub_name",
  "hub_level",
  "longitude",
  "latitude",
  "coordinate_valid",
  "cl_binary",
  "hu_binary",
  "nbs_binary",
  "tr_binary",
  "hub_service_score",
  "hub_readiness",
  "eligible_primary_hub",
  "eligible_strong_hub",
  "eligible_full_hub",
  "eligible_treatment_any_hub",
  "eligible_transfusion_ready_hub"
)

missing_hub_cols <- setdiff(
  required_hub_cols,
  names(hubs_raw)
)

if (length(missing_hub_cols) > 0) {
  stop(
    paste0(
      "Hub file is missing required columns:\n",
      paste(missing_hub_cols, collapse = "\n"),
      "\n\nColumns found:\n",
      paste(names(hubs_raw), collapse = ", ")
    )
  )
}

# ----------------------------
# 7. Standardise spokes
# ----------------------------

spokes_valid <- spokes_raw %>%
  mutate(
    source_facility_name = as.character(source_facility_name),

    matched_facility_name_tmp = if ("matched_facility_name" %in% names(.)) {
      as.character(matched_facility_name)
    } else {
      NA_character_
    },

    spoke_name = case_when(
      !is.na(matched_facility_name_tmp) & matched_facility_name_tmp != "" ~ matched_facility_name_tmp,
      !is.na(source_facility_name) & source_facility_name != "" ~ source_facility_name,
      TRUE ~ paste0("Unnamed spoke ", row_number())
    ),

    spoke_id = if ("spoke_id" %in% names(.)) {
      as.character(spoke_id)
    } else if ("matched_facility_id" %in% names(.)) {
      paste0("SPOKE_", stringr::str_pad(row_number(), width = 4, pad = "0"))
    } else {
      paste0("SPOKE_", stringr::str_pad(row_number(), width = 4, pad = "0"))
    },

    spoke_type = as.character(spoke_type),
    existing_nbs = suppressWarnings(as.integer(existing_nbs)),
    collocated_candidate_nbs = suppressWarnings(as.integer(collocated_candidate_nbs)),

    point_longitude = suppressWarnings(as.numeric(point_longitude)),
    point_latitude = suppressWarnings(as.numeric(point_latitude)),

    coordinate_valid_input = if ("coordinate_valid_final" %in% names(.)) {
      as_logical_safe(coordinate_valid_final)
    } else {
      as_logical_safe(coordinate_valid)
    },

    coordinate_valid_rechecked = valid_uganda_coordinates(
      point_longitude,
      point_latitude
    ),

    coordinate_valid_final = coordinate_valid_input & coordinate_valid_rechecked,

    matched_facility_id = if ("matched_facility_id" %in% names(.)) {
      as.character(matched_facility_id)
    } else {
      NA_character_
    },

    matched_facility_level = if ("matched_facility_level" %in% names(.)) {
      as.character(matched_facility_level)
    } else {
      NA_character_
    },

    matched_district = if ("matched_district" %in% names(.)) {
      as.character(matched_district)
    } else {
      NA_character_
    }
  ) %>%
  filter(coordinate_valid_final) %>%
  arrange(
    desc(collocated_candidate_nbs),
    spoke_type,
    spoke_name
  ) %>%
  mutate(
    spoke_id = paste0(
      "SPOKE_",
      stringr::str_pad(row_number(), width = 4, pad = "0")
    )
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
    coordinate_valid_final,
    everything()
  )

# ----------------------------
# 8. Standardise hubs
# ----------------------------

hubs_valid <- hubs_raw %>%
  mutate(
    longitude = suppressWarnings(as.numeric(longitude)),
    latitude = suppressWarnings(as.numeric(latitude)),

    coordinate_valid = as_logical_safe(coordinate_valid),

    coordinate_valid_rechecked = valid_uganda_coordinates(
      longitude,
      latitude
    ),

    coordinate_valid_final = coordinate_valid & coordinate_valid_rechecked,

    eligible_primary_hub = as_logical_safe(eligible_primary_hub),
    eligible_strong_hub = as_logical_safe(eligible_strong_hub),
    eligible_full_hub = as_logical_safe(eligible_full_hub),
    eligible_treatment_any_hub = as_logical_safe(eligible_treatment_any_hub),
    eligible_transfusion_ready_hub = as_logical_safe(eligible_transfusion_ready_hub),

    cl_binary = suppressWarnings(as.integer(cl_binary)),
    hu_binary = suppressWarnings(as.integer(hu_binary)),
    nbs_binary = suppressWarnings(as.integer(nbs_binary)),
    tr_binary = suppressWarnings(as.integer(tr_binary)),
    hub_service_score = suppressWarnings(as.integer(hub_service_score))
  ) %>%
  filter(coordinate_valid_final)

if (nrow(spokes_valid) == 0) {
  stop("No valid geocoded spokes found after standardisation.")
}

if (nrow(hubs_valid) == 0) {
  stop("No valid geocoded hubs found after standardisation.")
}

message("Valid geocoded spokes used: ", nrow(spokes_valid))
message("Valid candidate hubs used: ", nrow(hubs_valid))

# Expected check
if (nrow(spokes_valid) != 168) {
  warning(
    paste0(
      "Expected 168 valid geocoded spokes, but found ",
      nrow(spokes_valid),
      ". Continue only if this is expected."
    )
  )
}

if (nrow(hubs_valid) != 280) {
  warning(
    paste0(
      "Expected 280 valid candidate hubs, but found ",
      nrow(hubs_valid),
      ". Continue only if this is expected."
    )
  )
}

# ----------------------------
# 9. Convert to sf and project to UTM Zone 36N
# ----------------------------

spokes_sf <- spokes_valid %>%
  st_as_sf(
    coords = c("point_longitude", "point_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(32636)

hubs_sf <- hubs_valid %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(32636)

# ----------------------------
# 10. Create all-candidate distance matrix
# ----------------------------

distance_matrix_m <- st_distance(
  spokes_sf,
  hubs_sf
)

distance_matrix_km <- as.numeric(distance_matrix_m) / 1000

distance_matrix_tbl <- as_tibble(
  matrix(
    distance_matrix_km,
    nrow = nrow(spokes_sf),
    ncol = nrow(hubs_sf),
    byrow = FALSE
  )
)

names(distance_matrix_tbl) <- hubs_sf$hub_id

all_candidate_routes <- distance_matrix_tbl %>%
  mutate(
    spoke_id = spokes_sf$spoke_id
  ) %>%
  pivot_longer(
    cols = -spoke_id,
    names_to = "hub_id",
    values_to = "distance_km"
  ) %>%
  left_join(
    spokes_sf %>%
      st_drop_geometry() %>%
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
        point_latitude
      ),
    by = "spoke_id"
  ) %>%
  left_join(
    hubs_sf %>%
      st_drop_geometry() %>%
      select(
        hub_id,
        hub_name,
        hub_level,
        ownership,
        subregion,
        district,
        longitude,
        latitude,
        cl_binary,
        hu_binary,
        nbs_binary,
        tr_binary,
        hub_service_score,
        hub_service_deficit,
        hub_readiness,
        eligible_primary_hub,
        eligible_strong_hub,
        eligible_full_hub,
        eligible_treatment_any_hub,
        eligible_transfusion_ready_hub,
        catchment_population,
        scd_admissions,
        scd_deaths,
        hbs_mean,
        pfpr_mean
      ),
    by = "hub_id"
  ) %>%
  mutate(
    distance_km = round(distance_km, 3),
    distance_band = distance_band(distance_km)
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
    hub_name,
    hub_level,
    ownership,
    subregion,
    district,
    longitude,
    latitude,
    distance_km,
    distance_band,
    cl_binary,
    hu_binary,
    nbs_binary,
    tr_binary,
    hub_service_score,
    hub_service_deficit,
    hub_readiness,
    eligible_primary_hub,
    eligible_strong_hub,
    eligible_full_hub,
    eligible_treatment_any_hub,
    eligible_transfusion_ready_hub,
    catchment_population,
    scd_admissions,
    scd_deaths,
    hbs_mean,
    pfpr_mean
  )

# ----------------------------
# 11. Create scenario-specific routes
# ----------------------------

routes_primary <- all_candidate_routes %>%
  filter(eligible_primary_hub) %>%
  mutate(
    scenario = "primary_score_ge_2",
    scenario_label = "Primary hubs, service score >=2"
  )

routes_strong <- all_candidate_routes %>%
  filter(eligible_strong_hub) %>%
  mutate(
    scenario = "strong_score_ge_3",
    scenario_label = "Strong hubs, service score >=3"
  )

routes_full <- all_candidate_routes %>%
  filter(eligible_full_hub) %>%
  mutate(
    scenario = "full_score_4",
    scenario_label = "Full hubs, service score =4"
  )

routes_treatment_any <- all_candidate_routes %>%
  filter(eligible_treatment_any_hub) %>%
  mutate(
    scenario = "treatment_any_hu_cl_tr",
    scenario_label = "Treatment-any hubs, HU or CL or TR"
  )

routes_transfusion_ready <- all_candidate_routes %>%
  filter(eligible_transfusion_ready_hub) %>%
  mutate(
    scenario = "transfusion_ready",
    scenario_label = "Transfusion-ready hubs"
  )

scenario_routes <- bind_rows(
  routes_primary,
  routes_strong,
  routes_full,
  routes_treatment_any,
  routes_transfusion_ready
) %>%
  select(
    scenario,
    scenario_label,
    everything()
  )

# ----------------------------
# 12. Quality checks
# ----------------------------

route_qc <- tibble(
  metric = c(
    "valid_spokes_used",
    "valid_candidate_hubs_used",
    "all_candidate_routes",
    "primary_routes",
    "strong_routes",
    "full_routes",
    "treatment_any_routes",
    "transfusion_ready_routes",
    "missing_distance_values",
    "missing_spoke_metadata",
    "missing_hub_metadata"
  ),
  value = c(
    n_distinct(all_candidate_routes$spoke_id),
    n_distinct(all_candidate_routes$hub_id),
    nrow(all_candidate_routes),
    nrow(routes_primary),
    nrow(routes_strong),
    nrow(routes_full),
    nrow(routes_treatment_any),
    nrow(routes_transfusion_ready),
    sum(is.na(all_candidate_routes$distance_km)),
    sum(is.na(all_candidate_routes$spoke_name)),
    sum(is.na(all_candidate_routes$hub_name))
  )
)

print(route_qc)

expected_all_candidate_routes <- nrow(spokes_valid) * nrow(hubs_valid)

if (nrow(all_candidate_routes) != expected_all_candidate_routes) {
  stop(
    paste0(
      "Route matrix size is wrong.\n",
      "Expected ",
      expected_all_candidate_routes,
      " routes, but got ",
      nrow(all_candidate_routes),
      "."
    )
  )
}

if (sum(is.na(all_candidate_routes$distance_km)) > 0) {
  stop("Some route distances are missing. Check spoke and hub geometries.")
}

if (nrow(routes_primary) == 0) {
  stop("No primary eligible hub routes found. Check eligible_primary_hub.")
}

# ----------------------------
# 13. Summarise route matrices
# ----------------------------

all_candidate_summary <- all_candidate_routes %>%
  summarise(
    route_set = "all_candidate_hubs_280",
    spokes = n_distinct(spoke_id),
    hubs = n_distinct(hub_id),
    candidate_routes = n(),
    min_distance_km = round(safe_min(distance_km), 1),
    median_distance_km = round(safe_median(distance_km), 1),
    mean_distance_km = round(safe_mean(distance_km), 1),
    max_distance_km = round(safe_max(distance_km), 1),
    routes_0_to_25_km = sum(distance_band == "0_to_25_km", na.rm = TRUE),
    routes_25_to_50_km = sum(distance_band == "25_to_50_km", na.rm = TRUE),
    routes_50_to_100_km = sum(distance_band == "50_to_100_km", na.rm = TRUE),
    routes_over_100_km = sum(distance_band == "over_100_km", na.rm = TRUE),
    pct_0_to_25_km = round(100 * routes_0_to_25_km / candidate_routes, 1),
    pct_25_to_50_km = round(100 * routes_25_to_50_km / candidate_routes, 1),
    pct_50_to_100_km = round(100 * routes_50_to_100_km / candidate_routes, 1),
    pct_over_100_km = round(100 * routes_over_100_km / candidate_routes, 1),
    .groups = "drop"
  )

scenario_order <- c(
  "primary_score_ge_2",
  "strong_score_ge_3",
  "full_score_4",
  "treatment_any_hu_cl_tr",
  "transfusion_ready"
)

scenario_summary <- scenario_routes %>%
  group_by(
    scenario,
    scenario_label
  ) %>%
  summarise(
    spokes = n_distinct(spoke_id),
    hubs = n_distinct(hub_id),
    candidate_routes = n(),
    min_distance_km = round(safe_min(distance_km), 1),
    median_distance_km = round(safe_median(distance_km), 1),
    mean_distance_km = round(safe_mean(distance_km), 1),
    max_distance_km = round(safe_max(distance_km), 1),
    routes_0_to_25_km = sum(distance_band == "0_to_25_km", na.rm = TRUE),
    routes_25_to_50_km = sum(distance_band == "25_to_50_km", na.rm = TRUE),
    routes_50_to_100_km = sum(distance_band == "50_to_100_km", na.rm = TRUE),
    routes_over_100_km = sum(distance_band == "over_100_km", na.rm = TRUE),
    pct_0_to_25_km = round(100 * routes_0_to_25_km / candidate_routes, 1),
    pct_25_to_50_km = round(100 * routes_25_to_50_km / candidate_routes, 1),
    pct_50_to_100_km = round(100 * routes_50_to_100_km / candidate_routes, 1),
    pct_over_100_km = round(100 * routes_over_100_km / candidate_routes, 1),
    .groups = "drop"
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_order
    )
  ) %>%
  arrange(scenario) %>%
  mutate(
    scenario = as.character(scenario)
  )

distance_band_summary_all <- all_candidate_routes %>%
  count(
    distance_band,
    name = "n_routes"
  ) %>%
  mutate(
    pct_routes = round(100 * n_routes / sum(n_routes), 1),
    route_set = "all_candidate_hubs_280"
  ) %>%
  select(
    route_set,
    distance_band,
    n_routes,
    pct_routes
  )

distance_band_summary_by_scenario <- scenario_routes %>%
  count(
    scenario,
    scenario_label,
    distance_band,
    name = "n_routes"
  ) %>%
  group_by(
    scenario,
    scenario_label
  ) %>%
  mutate(
    pct_routes = round(100 * n_routes / sum(n_routes), 1)
  ) %>%
  ungroup() %>%
  arrange(
    factor(scenario, levels = scenario_order),
    distance_band
  )

spoke_nearest_candidate_summary <- all_candidate_routes %>%
  group_by(
    spoke_id,
    spoke_name,
    spoke_type
  ) %>%
  slice_min(
    order_by = distance_km,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    spoke_id,
    spoke_name,
    spoke_type,
    nearest_candidate_hub_id = hub_id,
    nearest_candidate_hub_name = hub_name,
    nearest_candidate_hub_level = hub_level,
    nearest_candidate_hub_ownership = ownership,
    nearest_candidate_hub_subregion = subregion,
    nearest_candidate_distance_km = distance_km,
    nearest_candidate_distance_band = distance_band,
    nearest_candidate_service_score = hub_service_score,
    nearest_candidate_readiness = hub_readiness
  ) %>%
  arrange(
    nearest_candidate_distance_km,
    spoke_name
  )

print(all_candidate_summary)
print(scenario_summary)
print(distance_band_summary_all)
print(distance_band_summary_by_scenario)

# ----------------------------
# 14. Save route outputs
# ----------------------------

write_csv(
  all_candidate_routes,
  file.path(
    processed_route_dir,
    "spoke_hub_distance_matrix_all_candidate_hubs.csv"
  )
)

write_csv(
  scenario_routes,
  file.path(
    processed_route_dir,
    "spoke_hub_distance_matrix_by_scenario.csv"
  )
)

write_csv(
  all_candidate_summary,
  file.path(
    processed_route_dir,
    "spoke_hub_distance_matrix_all_candidate_hubs_summary.csv"
  )
)

write_csv(
  scenario_summary,
  file.path(
    processed_route_dir,
    "spoke_hub_distance_matrix_by_scenario_summary.csv"
  )
)

write_csv(
  distance_band_summary_all,
  file.path(
    processed_route_dir,
    "spoke_hub_distance_band_summary_all_candidate_hubs.csv"
  )
)

write_csv(
  distance_band_summary_by_scenario,
  file.path(
    processed_route_dir,
    "spoke_hub_distance_band_summary_by_scenario.csv"
  )
)

write_csv(
  spoke_nearest_candidate_summary,
  file.path(
    processed_route_dir,
    "spoke_nearest_candidate_hub_summary.csv"
  )
)

write_csv(
  route_qc,
  file.path(
    log_dir,
    "03_spoke_hub_distance_matrix_qc.csv"
  )
)

write_xlsx(
  list(
    route_qc = route_qc,
    all_candidate_summary = all_candidate_summary,
    scenario_summary = scenario_summary,
    distance_band_summary_all = distance_band_summary_all,
    distance_band_summary_by_scenario = distance_band_summary_by_scenario,
    spoke_nearest_candidate_summary = spoke_nearest_candidate_summary
  ),
  file.path(
    output_table_dir,
    "03_spoke_hub_distance_matrix_summary.xlsx"
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
  paste0("## ", Sys.Date(), " - Created spoke-to-hub distance matrices"),
  "",
  paste0("- Valid geocoded spokes used: ", nrow(spokes_valid), "."),
  paste0("- Valid candidate hubs used: ", nrow(hubs_valid), "."),
  paste0("- All-candidate routes generated: ", nrow(all_candidate_routes), "."),
  paste0("- Scenario-specific routes generated: ", nrow(scenario_routes), "."),
  paste0("- Primary eligible hub routes: ", nrow(routes_primary), "."),
  paste0("- Strong hub routes: ", nrow(routes_strong), "."),
  paste0("- Full hub routes: ", nrow(routes_full), "."),
  paste0("- Treatment-any routes: ", nrow(routes_treatment_any), "."),
  paste0("- Transfusion-ready routes: ", nrow(routes_transfusion_ready), "."),
  paste0("- Output all-candidate matrix: ",
         file.path(processed_route_dir, "spoke_hub_distance_matrix_all_candidate_hubs.csv")),
  paste0("- Output scenario matrix: ",
         file.path(processed_route_dir, "spoke_hub_distance_matrix_by_scenario.csv"))
)

cat(
  paste(log_entry, collapse = "\n"),
  file = log_path,
  append = TRUE
)

message("Spoke-to-hub distance matrix complete.")
message("All-candidate route matrix:")
message(file.path(processed_route_dir, "spoke_hub_distance_matrix_all_candidate_hubs.csv"))
message("Scenario route matrix:")
message(file.path(processed_route_dir, "spoke_hub_distance_matrix_by_scenario.csv"))
message("Summary workbook:")
message(file.path(output_table_dir, "03_spoke_hub_distance_matrix_summary.xlsx"))
