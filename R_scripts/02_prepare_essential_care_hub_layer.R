# ============================================================
# 02_prepare_essential_care_hub_layer.R
# Prepare essential-care SCD candidate and eligible hub layer
# using facilities_master.csv for coordinates
# ============================================================

# Purpose:
# The 280-row essential-care hub attribute file contains service-readiness
# variables but does not contain longitude and latitude.
#
# Therefore, this script:
# 1. Reads the 280-row essential-care hub attribute file.
# 2. Reads facilities_master.csv.
# 3. Joins hub attributes to master coordinates by facility_id.
# 4. Builds candidate and eligible SCD care hub layers.
# 5. Saves CSV, Excel, and GeoPackage outputs.

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

raw_root_dir <- file.path(
  project_root,
  "01_raw_data"
)

raw_essential_dir <- file.path(
  raw_root_dir,
  "essential_care_prioritisation"
)

processed_facility_dir <- file.path(
  project_root,
  "02_processed_data",
  "facilities"
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
  processed_facility_dir,
  processed_spatial_dir,
  output_table_dir,
  log_dir,
  admin_dir
)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
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

clean_text_field <- function(x) {
  x %>%
    as.character() %>%
    str_squish()
}

clean_facility_name <- function(x) {
  x %>%
    as.character() %>%
    str_to_upper() %>%
    str_replace_all("&", " AND ") %>%
    str_replace_all("\\bH/C\\b", " HC ") %>%
    str_replace_all("\\bHEALTH\\s*CENTRE\\b", " HC ") %>%
    str_replace_all("\\bHEALTH\\s*CENTER\\b", " HC ") %>%
    str_replace_all("\\bH\\s*C\\b", " HC ") %>%
    str_replace_all("\\bHC\\s*2\\b", " HC II ") %>%
    str_replace_all("\\bHC\\s*II\\b", " HC II ") %>%
    str_replace_all("\\bHC\\s*3\\b", " HC III ") %>%
    str_replace_all("\\bHC\\s*III\\b", " HC III ") %>%
    str_replace_all("\\bHC\\s*4\\b", " HC IV ") %>%
    str_replace_all("\\bHC\\s*IV\\b", " HC IV ") %>%
    str_replace_all("\\bHOSP\\b", " HOSPITAL ") %>%
    str_replace_all("\\bST\\.?\\b", " SAINT ") %>%
    str_replace_all("[^A-Z0-9]+", " ") %>%
    str_squish()
}

to_binary <- function(x) {
  case_when(
    is.na(x) ~ 0L,
    x %in% c(TRUE, "TRUE", "True", "true", "YES", "Yes", "yes", "Y", "y") ~ 1L,
    suppressWarnings(as.numeric(x)) > 0 ~ 1L,
    TRUE ~ 0L
  )
}

# ----------------------------
# 4. Locate input files
# ----------------------------

raw_dirs_to_check <- c(
  raw_root_dir,
  raw_essential_dir
)

existing_raw_dirs <- raw_dirs_to_check[dir.exists(raw_dirs_to_check)]

if (length(existing_raw_dirs) == 0) {
  stop(
    paste0(
      "None of the expected raw-data directories exist:\n",
      paste(raw_dirs_to_check, collapse = "\n")
    )
  )
}

csv_files <- unlist(
  lapply(
    existing_raw_dirs,
    function(d) {
      list.files(
        d,
        pattern = "\\.csv$",
        full.names = TRUE,
        recursive = FALSE
      )
    }
  )
)

if (length(csv_files) == 0) {
  stop(
    paste0(
      "No CSV files found in:\n",
      paste(existing_raw_dirs, collapse = "\n")
    )
  )
}

inspect_csv_file <- function(file_path) {
  dat_head <- tryCatch(
    read_csv(
      file_path,
      show_col_types = FALSE,
      n_max = 5
    ) %>%
      clean_names(),
    error = function(e) NULL
  )

  n_rows <- tryCatch(
    nrow(
      read_csv(
        file_path,
        show_col_types = FALSE
      )
    ),
    error = function(e) NA_integer_
  )

  tibble(
    file_path = file_path,
    file_name = basename(file_path),
    rows = n_rows,
    columns = if (!is.null(dat_head)) {
      paste(names(dat_head), collapse = ", ")
    } else {
      NA_character_
    }
  )
}

csv_inventory <- bind_rows(
  lapply(csv_files, inspect_csv_file)
)

message("CSV inventory found in raw-data folders:")
print(csv_inventory, n = Inf)

# Select the 280-row essential-care hub file.
hub_candidates <- csv_inventory %>%
  filter(
    rows == 280,
    str_detect(
      str_to_lower(file_name),
      "^essential_care_hub_attributes_280"
    )
  ) %>%
  arrange(file_path)

if (nrow(hub_candidates) == 0) {
  stop(
    paste0(
      "Could not find the 280-row essential-care hub attribute file.\n",
      "Expected filename beginning with essential_care_hub_attributes_280.\n",
      "CSV inventory printed above."
    )
  )
}

essential_hub_file <- hub_candidates$file_path[1]

# Select facilities master file.
master_candidates <- csv_inventory %>%
  filter(
    str_to_lower(file_name) == "facilities_master.csv"
  ) %>%
  arrange(file_path)

if (nrow(master_candidates) == 0) {
  stop(
    paste0(
      "Could not find facilities_master.csv.\n",
      "This file is required to add longitude and latitude to the hub layer.\n",
      "CSV inventory printed above."
    )
  )
}

facility_master_file <- master_candidates$file_path[1]

message("Selected essential-care 280-facility hub file:")
message(essential_hub_file)

message("Selected facility master file:")
message(facility_master_file)

# ----------------------------
# 5. Read inputs
# ----------------------------

if (exists("hub_raw")) rm(hub_raw)

hub_raw <- read_csv(
  essential_hub_file,
  show_col_types = FALSE
) %>%
  clean_names()

facility_master_raw <- read_csv(
  facility_master_file,
  show_col_types = FALSE
) %>%
  clean_names()

message("Rows read from hub file: ", nrow(hub_raw))
message("Hub columns:")
print(names(hub_raw))

message("Rows read from facility master: ", nrow(facility_master_raw))
message("Facility master columns:")
print(names(facility_master_raw))

# ----------------------------
# 6. Validate input structure
# ----------------------------

required_hub_cols <- c(
  "facility_id",
  "facility_name",
  "facility_type",
  "ownership",
  "subregion",
  "cl_binary",
  "hu_binary",
  "nbs_binary",
  "tr_binary",
  "hub_service_score",
  "hub_service_deficit",
  "hub_readiness"
)

missing_hub_cols <- setdiff(
  required_hub_cols,
  names(hub_raw)
)

if (length(missing_hub_cols) > 0) {
  stop(
    paste0(
      "The 280-row hub file is missing required columns:\n",
      paste(missing_hub_cols, collapse = "\n"),
      "\n\nColumns found:\n",
      paste(names(hub_raw), collapse = ", ")
    )
  )
}

required_master_cols <- c(
  "hf_id",
  "name",
  "type",
  "ownership",
  "authority",
  "status",
  "longitude",
  "latitude"
)

missing_master_cols <- setdiff(
  required_master_cols,
  names(facility_master_raw)
)

if (length(missing_master_cols) > 0) {
  stop(
    paste0(
      "facilities_master.csv is missing required columns:\n",
      paste(missing_master_cols, collapse = "\n"),
      "\n\nColumns found:\n",
      paste(names(facility_master_raw), collapse = ", ")
    )
  )
}

if (nrow(hub_raw) != 280) {
  stop(
    paste0(
      "Wrong hub file selected. Expected 280 rows but found ",
      nrow(hub_raw),
      "."
    )
  )
}

# ----------------------------
# 7. Prepare facility master coordinates
# ----------------------------

facility_master <- facility_master_raw %>%
  mutate(
    master_hf_id = as.character(hf_id),
    master_name = clean_text_field(name),
    master_name_clean = clean_facility_name(master_name),
    master_type = clean_text_field(type),
    master_ownership = clean_text_field(ownership),
    master_authority = clean_text_field(authority),
    master_status = clean_text_field(status),
    master_longitude = suppressWarnings(as.numeric(longitude)),
    master_latitude = suppressWarnings(as.numeric(latitude)),
    master_coordinate_valid = valid_uganda_coordinates(
      master_longitude,
      master_latitude
    ),

    master_region = if ("f15_regions" %in% names(.)) {
      clean_text_field(f15_regions)
    } else {
      NA_character_
    },

    master_district = if ("d_name2016" %in% names(.)) {
      clean_text_field(d_name2016)
    } else {
      NA_character_
    },

    master_county = if ("c_name2016" %in% names(.)) {
      clean_text_field(c_name2016)
    } else {
      NA_character_
    },

    master_subcounty = if ("s_name2016" %in% names(.)) {
      clean_text_field(s_name2016)
    } else {
      NA_character_
    }
  ) %>%
  filter(master_coordinate_valid) %>%
  arrange(master_hf_id, desc(master_status == "Functional")) %>%
  distinct(master_hf_id, .keep_all = TRUE) %>%
  select(
    master_hf_id,
    master_name,
    master_name_clean,
    master_type,
    master_ownership,
    master_authority,
    master_status,
    master_longitude,
    master_latitude,
    master_coordinate_valid,
    master_region,
    master_district,
    master_county,
    master_subcounty
  )

# ----------------------------
# 8. Join hub attributes to master coordinates
# ----------------------------

hub_layer <- hub_raw %>%
  mutate(
    facility_id = as.character(facility_id),
    hub_source_row_id = row_number(),
    hub_id = facility_id,
    hub_name = clean_text_field(facility_name),
    hub_name_clean = clean_facility_name(hub_name),
    hub_level = clean_text_field(facility_type),
    ownership = clean_text_field(ownership),
    subregion = clean_text_field(subregion),

    cl_binary = to_binary(cl_binary),
    hu_binary = to_binary(hu_binary),
    nbs_binary = to_binary(nbs_binary),
    tr_binary = to_binary(tr_binary),

    hub_service_score = cl_binary + hu_binary + nbs_binary + tr_binary,
    hub_service_deficit = 1 - (hub_service_score / 4),

    hub_readiness = case_when(
      hub_service_score == 4 ~ "full_hub_score_4",
      hub_service_score == 3 ~ "strong_hub_score_3",
      hub_service_score == 2 ~ "partial_hub_score_2",
      hub_service_score == 1 ~ "weak_hub_score_1",
      hub_service_score == 0 ~ "not_ready_score_0",
      TRUE ~ "unknown"
    ),

    eligible_primary_hub = hub_service_score >= 2,
    eligible_strong_hub = hub_service_score >= 3,
    eligible_full_hub = hub_service_score == 4,
    eligible_treatment_any_hub = hu_binary == 1 | cl_binary == 1 | tr_binary == 1,
    eligible_transfusion_ready_hub = tr_binary == 1,

    catchment_population = suppressWarnings(as.numeric(catchment_population)),
    scd_admissions = suppressWarnings(as.numeric(scd_admissions)),
    scd_deaths = suppressWarnings(as.numeric(scd_deaths)),
    hbs_mean = suppressWarnings(as.numeric(hbs_mean)),
    pfpr_mean = suppressWarnings(as.numeric(pfpr_mean))
  ) %>%
  left_join(
    facility_master,
    by = c("facility_id" = "master_hf_id")
  ) %>%
  mutate(
    longitude = master_longitude,
    latitude = master_latitude,
    coordinate_valid = valid_uganda_coordinates(longitude, latitude),
    coordinate_source = case_when(
      coordinate_valid ~ "facilities_master_by_facility_id",
      TRUE ~ "missing_or_invalid_master_coordinate"
    ),

    master_name_match_flag = case_when(
      is.na(master_name) ~ "not_found_in_master_by_facility_id",
      hub_name_clean == master_name_clean ~ "exact_clean_name_match",
      TRUE ~ "facility_id_match_name_differs"
    )
  ) %>%
  select(
    hub_id,
    facility_id,
    hub_source_row_id,
    hub_name,
    hub_name_clean,
    hub_level,
    ownership,
    subregion,
    longitude,
    latitude,
    coordinate_valid,
    coordinate_source,
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
    pfpr_mean,
    master_name,
    master_name_clean,
    master_type,
    master_ownership,
    master_authority,
    master_status,
    master_region,
    master_district,
    master_county,
    master_subcounty,
    master_name_match_flag,
    everything()
  )

# ----------------------------
# 9. Validate joined hub layer
# ----------------------------

hub_missing_coordinates <- hub_layer %>%
  filter(!coordinate_valid)

if (nrow(hub_missing_coordinates) > 0) {
  warning(
    paste0(
      nrow(hub_missing_coordinates),
      " hub records are missing valid coordinates after joining to facilities_master.csv."
    )
  )
}

eligible_primary_valid <- hub_layer %>%
  filter(
    eligible_primary_hub,
    coordinate_valid
  )

if (nrow(eligible_primary_valid) == 0) {
  stop(
    "No eligible primary hubs with valid coordinates after joining to facilities_master.csv."
  )
}

duplicate_hub_ids <- hub_layer %>%
  count(hub_id, name = "n_records") %>%
  filter(n_records > 1) %>%
  left_join(hub_layer, by = "hub_id") %>%
  arrange(hub_id, hub_name)

name_mismatch_review <- hub_layer %>%
  filter(master_name_match_flag != "exact_clean_name_match") %>%
  select(
    hub_id,
    hub_name,
    hub_level,
    master_name,
    master_type,
    master_district,
    longitude,
    latitude,
    coordinate_valid,
    master_name_match_flag
  ) %>%
  arrange(master_name_match_flag, hub_name)

# ----------------------------
# 10. Create summaries
# ----------------------------

hub_summary <- tibble(
  metric = c(
    "candidate_hub_records",
    "candidate_hubs_with_valid_coordinates",
    "candidate_hubs_missing_coordinates",
    "eligible_primary_hubs_score_ge_2",
    "eligible_primary_hubs_with_valid_coordinates",
    "eligible_strong_hubs_score_ge_3",
    "eligible_full_hubs_score_4",
    "eligible_treatment_any_hubs",
    "eligible_transfusion_ready_hubs",
    "full_hubs_score_4",
    "strong_hubs_score_3",
    "partial_hubs_score_2",
    "weak_hubs_score_1",
    "not_ready_hubs_score_0",
    "facilities_missing_hbs_mean",
    "facilities_missing_pfpr_mean",
    "hub_records_with_name_mismatch_or_missing_master"
  ),
  value = c(
    nrow(hub_layer),
    sum(hub_layer$coordinate_valid, na.rm = TRUE),
    sum(!hub_layer$coordinate_valid, na.rm = TRUE),
    sum(hub_layer$eligible_primary_hub, na.rm = TRUE),
    nrow(eligible_primary_valid),
    sum(hub_layer$eligible_strong_hub, na.rm = TRUE),
    sum(hub_layer$eligible_full_hub, na.rm = TRUE),
    sum(hub_layer$eligible_treatment_any_hub, na.rm = TRUE),
    sum(hub_layer$eligible_transfusion_ready_hub, na.rm = TRUE),
    sum(hub_layer$hub_service_score == 4, na.rm = TRUE),
    sum(hub_layer$hub_service_score == 3, na.rm = TRUE),
    sum(hub_layer$hub_service_score == 2, na.rm = TRUE),
    sum(hub_layer$hub_service_score == 1, na.rm = TRUE),
    sum(hub_layer$hub_service_score == 0, na.rm = TRUE),
    sum(is.na(hub_layer$hbs_mean)),
    sum(is.na(hub_layer$pfpr_mean)),
    nrow(name_mismatch_review)
  )
)

hub_level_summary <- hub_layer %>%
  count(
    hub_level,
    hub_readiness,
    eligible_primary_hub,
    name = "n_facilities"
  ) %>%
  arrange(hub_level, hub_readiness)

service_domain_summary <- hub_layer %>%
  summarise(
    candidate_hubs = n(),
    cl_available = sum(cl_binary == 1, na.rm = TRUE),
    hu_available = sum(hu_binary == 1, na.rm = TRUE),
    nbs_available = sum(nbs_binary == 1, na.rm = TRUE),
    tr_available = sum(tr_binary == 1, na.rm = TRUE),
    cl_missing = sum(cl_binary == 0, na.rm = TRUE),
    hu_missing = sum(hu_binary == 0, na.rm = TRUE),
    nbs_missing = sum(nbs_binary == 0, na.rm = TRUE),
    tr_missing = sum(tr_binary == 0, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  )

print(hub_summary)
print(hub_level_summary)
print(service_domain_summary)

# ----------------------------
# 11. Save tabular outputs
# ----------------------------

write_csv(
  hub_layer,
  file.path(
    processed_facility_dir,
    "essential_care_hub_layer.csv"
  )
)

write_csv(
  hub_summary,
  file.path(
    processed_facility_dir,
    "essential_care_hub_layer_summary.csv"
  )
)

write_csv(
  hub_level_summary,
  file.path(
    log_dir,
    "02_essential_care_hub_level_summary.csv"
  )
)

write_csv(
  service_domain_summary,
  file.path(
    log_dir,
    "02_essential_care_service_domain_summary.csv"
  )
)

write_csv(
  duplicate_hub_ids,
  file.path(
    log_dir,
    "02_essential_care_duplicate_hub_ids.csv"
  )
)

write_csv(
  name_mismatch_review,
  file.path(
    log_dir,
    "02_essential_care_name_mismatch_review.csv"
  )
)

write_xlsx(
  list(
    essential_care_hub_layer = hub_layer,
    hub_summary = hub_summary,
    hub_level_summary = hub_level_summary,
    service_domain_summary = service_domain_summary,
    duplicate_hub_ids = duplicate_hub_ids,
    name_mismatch_review = name_mismatch_review,
    hub_missing_coordinates = hub_missing_coordinates
  ),
  file.path(
    output_table_dir,
    "02_essential_care_hub_layer.xlsx"
  )
)

# ----------------------------
# 12. Save spatial output
# ----------------------------

hub_points <- hub_layer %>%
  filter(coordinate_valid) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(32636)

hub_gpkg <- file.path(
  processed_spatial_dir,
  "essential_care_hub_points.gpkg"
)

if (file.exists(hub_gpkg)) {
  file.remove(hub_gpkg)
}

st_write(
  hub_points,
  hub_gpkg,
  layer = "essential_care_hub_points",
  delete_layer = TRUE,
  quiet = TRUE
)

eligible_primary_points <- hub_points %>%
  filter(eligible_primary_hub)

st_write(
  eligible_primary_points,
  hub_gpkg,
  layer = "eligible_primary_hub_points",
  append = TRUE,
  quiet = TRUE
)

# ----------------------------
# 13. Log
# ----------------------------

log_path <- file.path(
  admin_dir,
  "analysis_log_updated_spoke_layer.md"
)

log_entry <- c(
  "",
  paste0("## ", Sys.Date(), " - Prepared essential-care candidate and eligible hub layer"),
  "",
  paste0("- Selected hub input file: ", essential_hub_file),
  paste0("- Selected facility master file: ", facility_master_file),
  paste0("- Candidate hub records: ", nrow(hub_layer), "."),
  paste0("- Candidate hubs with valid coordinates: ", sum(hub_layer$coordinate_valid, na.rm = TRUE), "."),
  paste0("- Primary eligible hubs, service score >=2: ", sum(hub_layer$eligible_primary_hub, na.rm = TRUE), "."),
  paste0("- Primary eligible hubs with valid coordinates: ", nrow(eligible_primary_valid), "."),
  paste0("- Strong hubs, service score >=3: ", sum(hub_layer$eligible_strong_hub, na.rm = TRUE), "."),
  paste0("- Full hubs, service score =4: ", sum(hub_layer$eligible_full_hub, na.rm = TRUE), "."),
  paste0("- Spatial output: ", hub_gpkg)
)

cat(
  paste(log_entry, collapse = "\n"),
  file = log_path,
  append = TRUE
)

message("Essential-care hub layer complete.")
message("Processed CSV:")
message(file.path(processed_facility_dir, "essential_care_hub_layer.csv"))
message("Spatial GeoPackage:")
message(hub_gpkg)
message("Summary workbook:")
message(file.path(output_table_dir, "02_essential_care_hub_layer.xlsx"))