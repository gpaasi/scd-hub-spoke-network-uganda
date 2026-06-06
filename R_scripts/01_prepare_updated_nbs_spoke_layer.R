# ============================================================
# 01_prepare_updated_nbs_spoke_layer.R
# Prepare updated NBS spoke layer from master-merged files
# ============================================================

# Purpose:
# This script prepares the corrected newborn screening spoke layer
# using the updated master-merged NBS files.
#
# Input files:
# - 01_raw_data/nbs_prioritisation/nbs_spokes_master_merged (1).csv
# - 01_raw_data/nbs_prioritisation/nbs_spokes_geocoded_accepted.csv
#
# Main analytic input:
# - nbs_spokes_geocoded_accepted.csv
#
# This file contains the accepted geocoded spoke records from the
# health facility master merge.
#
# Outputs:
# - 02_processed_data/facilities/nbs_spoke_layer_geocoded_accepted.csv
# - 02_processed_data/facilities/nbs_spoke_layer_geocoded_accepted_summary.csv
# - 02_processed_data/spatial/nbs_spoke_points_geocoded_accepted.gpkg
# - 04_outputs/logs/01_updated_nbs_spoke_layer_summary.csv
#
# Notes:
# - The full 211-record merged file is retained as an audit file.
# - The accepted geocoded file is used for provisional network analysis.
# - Manual-review records should remain excluded until validated.

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
  "writexl"
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

# ----------------------------
# 2. Define project paths
# ----------------------------

project_root <- "scd_hub_spoke_model"

raw_nbs_dir <- file.path(
  project_root,
  "01_raw_data",
  "nbs_prioritisation"
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
# 3. Define input files
# ----------------------------

full_merged_file_options <- c(
  file.path(raw_nbs_dir, "nbs_spokes_master_merged (1).csv"),
  file.path(raw_nbs_dir, "nbs_spokes_master_merged.csv")
)

accepted_geocoded_file <- file.path(
  raw_nbs_dir,
  "nbs_spokes_geocoded_accepted.csv"
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

full_merged_file <- first_existing_file(
  full_merged_file_options,
  "full merged NBS spoke audit file"
)

required_files <- c(
  full_merged_file,
  accepted_geocoded_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required file(s):\n",
      paste(missing_files, collapse = "\n")
    )
  )
}

message("Using full merged audit file: ", full_merged_file)
message("Using accepted geocoded spoke file: ", accepted_geocoded_file)

# ----------------------------
# 4. Read input files
# ----------------------------

nbs_spokes_all_211 <- read_csv(
  full_merged_file,
  show_col_types = FALSE
) %>%
  clean_names()

nbs_spokes_accepted_raw <- read_csv(
  accepted_geocoded_file,
  show_col_types = FALSE
) %>%
  clean_names()

# ----------------------------
# 5. Validate required columns
# ----------------------------

required_accepted_cols <- c(
  "source_facility_name",
  "spoke_source",
  "existing_nbs",
  "collocated_candidate_nbs",
  "accepted_match",
  "matched_facility_id",
  "matched_facility_name",
  "matched_facility_level",
  "matched_district",
  "point_longitude",
  "point_latitude",
  "coordinate_source",
  "coordinate_valid"
)

missing_accepted_cols <- setdiff(
  required_accepted_cols,
  names(nbs_spokes_accepted_raw)
)

if (length(missing_accepted_cols) > 0) {
  stop(
    paste0(
      "Accepted geocoded NBS file is missing required column(s):\n",
      paste(missing_accepted_cols, collapse = "\n")
    )
  )
}

# ----------------------------
# 6. Helper functions
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

# ----------------------------
# 7. Standardise accepted geocoded spokes
# ----------------------------

nbs_spoke_layer <- nbs_spokes_accepted_raw %>%
  mutate(
    source_facility_name = clean_text_field(source_facility_name),
    matched_facility_name = clean_text_field(matched_facility_name),
    matched_facility_level = clean_text_field(matched_facility_level),
    matched_district = clean_text_field(matched_district),

    existing_nbs = as.integer(existing_nbs),
    collocated_candidate_nbs = as.integer(collocated_candidate_nbs),
    accepted_match = as.logical(accepted_match),
    coordinate_valid = as.logical(coordinate_valid),

    point_longitude = as.numeric(point_longitude),
    point_latitude = as.numeric(point_latitude),

    coordinate_valid_rechecked = valid_uganda_coordinates(
      point_longitude,
      point_latitude
    ),

    coordinate_valid_final = coordinate_valid & coordinate_valid_rechecked,

    spoke_name = case_when(
      !is.na(matched_facility_name) & matched_facility_name != "" ~ matched_facility_name,
      TRUE ~ source_facility_name
    ),

    spoke_name_clean = clean_facility_name(spoke_name),

    spoke_type = case_when(
      existing_nbs == 1L & collocated_candidate_nbs == 1L ~
        "existing_and_collocated_candidate_nbs",

      existing_nbs == 1L & collocated_candidate_nbs == 0L ~
        "existing_nbs",

      existing_nbs == 0L & collocated_candidate_nbs == 1L ~
        "collocated_candidate_nbs",

      TRUE ~
        "unknown"
    )
  )

# ----------------------------
# 8. Create stable spoke IDs
# ----------------------------

nbs_spoke_layer <- nbs_spoke_layer %>%
  arrange(
    desc(collocated_candidate_nbs),
    matched_district,
    spoke_name
  ) %>%
  mutate(
    spoke_id = paste0(
      "SPOKE_",
      str_pad(row_number(), width = 4, pad = "0")
    )
  ) %>%
  select(
    spoke_id,
    spoke_name,
    spoke_name_clean,
    spoke_type,
    source_facility_name,
    spoke_source,
    existing_nbs,
    collocated_candidate_nbs,
    proposed_rank,
    matched_facility_id,
    matched_facility_name,
    matched_facility_level,
    matched_ownership,
    matched_authority,
    matched_status,
    matched_region,
    matched_district,
    matched_county,
    matched_subcounty,
    point_longitude,
    point_latitude,
    coordinate_source,
    coordinate_valid,
    coordinate_valid_rechecked,
    coordinate_valid_final,
    accepted_match,
    match_method,
    match_score,
    everything()
  )

# ----------------------------
# 9. Check for duplicate facility IDs and duplicate coordinates
# ----------------------------

duplicate_facility_ids <- nbs_spoke_layer %>%
  filter(!is.na(matched_facility_id)) %>%
  count(matched_facility_id, name = "n_records") %>%
  filter(n_records > 1) %>%
  left_join(
    nbs_spoke_layer,
    by = "matched_facility_id"
  ) %>%
  arrange(matched_facility_id, spoke_name)

duplicate_coordinates <- nbs_spoke_layer %>%
  count(point_longitude, point_latitude, name = "n_records") %>%
  filter(n_records > 1) %>%
  left_join(
    nbs_spoke_layer,
    by = c("point_longitude", "point_latitude")
  ) %>%
  arrange(point_longitude, point_latitude, spoke_name)

# ----------------------------
# 10. Create summaries
# ----------------------------

nbs_spoke_summary <- tibble(
  metric = c(
    "full_merged_spoke_records",
    "accepted_geocoded_spoke_records",
    "records_with_valid_coordinates",
    "records_with_invalid_coordinates_after_recheck",
    "existing_nbs_spokes",
    "collocated_candidate_nbs_spokes",
    "existing_and_collocated_candidate_spokes",
    "unique_matched_facility_ids",
    "duplicate_matched_facility_ids",
    "duplicate_coordinate_pairs"
  ),
  value = c(
    nrow(nbs_spokes_all_211),
    nrow(nbs_spoke_layer),
    sum(nbs_spoke_layer$coordinate_valid_final, na.rm = TRUE),
    sum(!nbs_spoke_layer$coordinate_valid_final, na.rm = TRUE),
    sum(nbs_spoke_layer$spoke_type == "existing_nbs", na.rm = TRUE),
    sum(nbs_spoke_layer$spoke_type == "collocated_candidate_nbs", na.rm = TRUE),
    sum(nbs_spoke_layer$spoke_type == "existing_and_collocated_candidate_nbs", na.rm = TRUE),
    n_distinct(nbs_spoke_layer$matched_facility_id),
    n_distinct(duplicate_facility_ids$matched_facility_id),
    nrow(
      duplicate_coordinates %>%
        distinct(point_longitude, point_latitude)
    )
  )
)

nbs_spoke_type_summary <- nbs_spoke_layer %>%
  count(
    spoke_type,
    coordinate_source,
    coordinate_valid_final,
    name = "n_spokes"
  ) %>%
  arrange(spoke_type, coordinate_source)

nbs_match_method_summary <- nbs_spoke_layer %>%
  count(
    spoke_type,
    match_method,
    name = "n_spokes"
  ) %>%
  arrange(spoke_type, match_method)

print(nbs_spoke_summary)
print(nbs_spoke_type_summary)
print(nbs_match_method_summary)

if (any(!nbs_spoke_layer$coordinate_valid_final)) {
  stop(
    paste0(
      "Some accepted spoke records failed coordinate validation. Review before continuing. Invalid records: ",
      sum(!nbs_spoke_layer$coordinate_valid_final)
    )
  )
}

# ----------------------------
# 11. Save tabular outputs
# ----------------------------

write_csv(
  nbs_spoke_layer,
  file.path(
    processed_facility_dir,
    "nbs_spoke_layer_geocoded_accepted.csv"
  )
)

write_csv(
  nbs_spoke_summary,
  file.path(
    processed_facility_dir,
    "nbs_spoke_layer_geocoded_accepted_summary.csv"
  )
)

write_csv(
  nbs_spoke_type_summary,
  file.path(
    log_dir,
    "01_nbs_spoke_type_summary.csv"
  )
)

write_csv(
  nbs_match_method_summary,
  file.path(
    log_dir,
    "01_nbs_spoke_match_method_summary.csv"
  )
)

write_csv(
  duplicate_facility_ids,
  file.path(
    log_dir,
    "01_nbs_spoke_duplicate_facility_ids.csv"
  )
)

write_csv(
  duplicate_coordinates,
  file.path(
    log_dir,
    "01_nbs_spoke_duplicate_coordinates.csv"
  )
)

write_xlsx(
  list(
    nbs_spoke_layer = nbs_spoke_layer,
    nbs_spoke_summary = nbs_spoke_summary,
    nbs_spoke_type_summary = nbs_spoke_type_summary,
    nbs_match_method_summary = nbs_match_method_summary,
    duplicate_facility_ids = duplicate_facility_ids,
    duplicate_coordinates = duplicate_coordinates
  ),
  file.path(
    output_table_dir,
    "01_nbs_spoke_layer_geocoded_accepted.xlsx"
  )
)

# ----------------------------
# 12. Save spatial output
# ----------------------------

nbs_spoke_points <- nbs_spoke_layer %>%
  filter(coordinate_valid_final) %>%
  st_as_sf(
    coords = c("point_longitude", "point_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(32636)

nbs_spoke_gpkg <- file.path(
  processed_spatial_dir,
  "nbs_spoke_points_geocoded_accepted.gpkg"
)

if (file.exists(nbs_spoke_gpkg)) {
  file.remove(nbs_spoke_gpkg)
}

st_write(
  nbs_spoke_points,
  nbs_spoke_gpkg,
  layer = "nbs_spoke_points_geocoded_accepted",
  delete_layer = TRUE,
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
  paste0("## ", Sys.Date(), " - Prepared updated accepted geocoded NBS spoke layer"),
  "",
  paste0("- Full merged audit records: ", nrow(nbs_spokes_all_211), "."),
  paste0("- Accepted geocoded spoke records: ", nrow(nbs_spoke_layer), "."),
  paste0("- Valid accepted spoke coordinates: ", sum(nbs_spoke_layer$coordinate_valid_final, na.rm = TRUE), "."),
  paste0("- Existing NBS spokes in accepted layer: ", sum(nbs_spoke_layer$spoke_type == "existing_nbs", na.rm = TRUE), "."),
  paste0("- Collocated candidate NBS spokes in accepted layer: ", sum(nbs_spoke_layer$spoke_type == "collocated_candidate_nbs", na.rm = TRUE), "."),
  paste0("- Spatial output: ", nbs_spoke_gpkg),
  "",
  "Note: manual-review records are not included in this provisional accepted-spoke analysis."
)

cat(
  paste(log_entry, collapse = "\n"),
  file = log_path,
  append = TRUE
)

message("Updated NBS spoke layer complete.")
message("Processed CSV:")
message(file.path(processed_facility_dir, "nbs_spoke_layer_geocoded_accepted.csv"))
message("Spatial GeoPackage:")
message(nbs_spoke_gpkg)
message("Summary workbook:")
message(file.path(output_table_dir, "01_nbs_spoke_layer_geocoded_accepted.xlsx"))