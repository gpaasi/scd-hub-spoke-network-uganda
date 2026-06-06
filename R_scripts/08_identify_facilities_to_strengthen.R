# ============================================================
# 08_identify_facilities_to_strengthen.R
# Identify SCD facilities for strengthening under updated network
# ============================================================

# Purpose:
# Identify facilities for SCD service strengthening using:
# - assigned primary spoke-to-hub routes
# - hub workload and route-risk outputs
# - the full 280-facility essential-care candidate hub layer
#
# Logic:
# Tier 1:
# - Current assigned hubs with high route risk, partial readiness,
#   weak or long routes, or moderate/high workload
#
# Tier 2:
# - Non-eligible or weak candidate facilities near spokes whose
#   assigned primary route is weak or long
#
# Monitor:
# - Eligible hubs used in the network but without immediate high-risk signal
#
# Outputs:
# - 02_processed_data/facilities/facilities_to_strengthen_scd_services.csv
# - 02_processed_data/facilities/facilities_to_strengthen_scd_services_summary.csv
# - 04_outputs/tables/08_facilities_to_strengthen_scd_services.xlsx

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
  "tidyr"
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

# ----------------------------
# 2. Define paths
# ----------------------------

project_root <- "scd_hub_spoke_model"

processed_route_dir <- file.path(
  project_root,
  "02_processed_data",
  "routes"
)

processed_facility_dir <- file.path(
  project_root,
  "02_processed_data",
  "facilities"
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

hub_layer_file <- file.path(
  processed_facility_dir,
  "essential_care_hub_layer.csv"
)

required_files <- c(
  assigned_primary_file,
  hub_workload_file,
  hub_layer_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required file(s):\n",
      paste(missing_files, collapse = "\n"),
      "\n\nRun Scripts 02, 04, and 05 first."
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

safe_int_col <- function(data, col_options) {
  existing <- intersect(col_options, names(data))

  if (length(existing) == 0) {
    return(rep(NA_integer_, nrow(data)))
  }

  suppressWarnings(as.integer(data[[existing[1]]]))
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

safe_min <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
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

hub_layer <- read_csv(
  hub_layer_file,
  show_col_types = FALSE
) %>%
  clean_names()

message("Rows read from assigned primary routes: ", nrow(assigned_primary))
message("Rows read from hub workload: ", nrow(hub_workload))
message("Rows read from hub layer: ", nrow(hub_layer))

# ----------------------------
# 5. Validate core columns
# ----------------------------

required_assigned_cols <- c(
  "spoke_id",
  "spoke_name",
  "spoke_type",
  "point_longitude",
  "point_latitude",
  "hub_id",
  "assigned_hub_name",
  "assigned_distance_km",
  "route_feasibility",
  "weak_or_long_route"
)

missing_assigned_cols <- setdiff(
  required_assigned_cols,
  names(assigned_primary)
)

if (length(missing_assigned_cols) > 0) {
  stop(
    paste0(
      "Assigned primary route file is missing required columns:\n",
      paste(missing_assigned_cols, collapse = "\n")
    )
  )
}

required_workload_cols <- c(
  "hub_id",
  "assigned_hub_name",
  "linked_spokes",
  "route_risk_category",
  "hub_priority_flag",
  "hub_service_score",
  "hub_readiness"
)

missing_workload_cols <- setdiff(
  required_workload_cols,
  names(hub_workload)
)

if (length(missing_workload_cols) > 0) {
  stop(
    paste0(
      "Hub workload file is missing required columns:\n",
      paste(missing_workload_cols, collapse = "\n")
    )
  )
}

required_hub_layer_cols <- c(
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
  "eligible_primary_hub"
)

missing_hub_layer_cols <- setdiff(
  required_hub_layer_cols,
  names(hub_layer)
)

if (length(missing_hub_layer_cols) > 0) {
  stop(
    paste0(
      "Essential-care hub layer is missing required columns:\n",
      paste(missing_hub_layer_cols, collapse = "\n"),
      "\n\nColumns found:\n",
      paste(names(hub_layer), collapse = ", ")
    )
  )
}

# ----------------------------
# 6. Standardise inputs
# ----------------------------

assigned_primary <- assigned_primary %>%
  mutate(
    assigned_distance_km = safe_num_col(cur_data(), c("assigned_distance_km")),
    point_longitude = safe_num_col(cur_data(), c("point_longitude")),
    point_latitude = safe_num_col(cur_data(), c("point_latitude")),
    weak_or_long_route = safe_lgl_col(cur_data(), c("weak_or_long_route")),
    long_route = safe_lgl_col(cur_data(), c("long_route")),
    very_long_route = safe_lgl_col(cur_data(), c("very_long_route")),
    existing_nbs = safe_int_col(cur_data(), c("existing_nbs")),
    collocated_candidate_nbs = safe_int_col(cur_data(), c("collocated_candidate_nbs"))
  )

hub_workload <- hub_workload %>%
  mutate(
    linked_spokes = safe_int_col(cur_data(), c("linked_spokes")),
    pct_weak_or_long_routes = safe_num_col(cur_data(), c("pct_weak_or_long_routes")),
    max_assigned_distance_km = safe_num_col(cur_data(), c("max_assigned_distance_km")),
    hub_service_score = safe_int_col(cur_data(), c("hub_service_score")),
    cl_binary = safe_int_col(cur_data(), c("cl_binary")),
    hu_binary = safe_int_col(cur_data(), c("hu_binary")),
    nbs_binary = safe_int_col(cur_data(), c("nbs_binary")),
    tr_binary = safe_int_col(cur_data(), c("tr_binary"))
  )

hub_layer <- hub_layer %>%
  mutate(
    longitude = safe_num_col(cur_data(), c("longitude")),
    latitude = safe_num_col(cur_data(), c("latitude")),
    coordinate_valid = safe_lgl_col(cur_data(), c("coordinate_valid")),
    coordinate_valid_rechecked = valid_uganda_coordinates(longitude, latitude),
    coordinate_valid_final = coordinate_valid | coordinate_valid_rechecked,

    eligible_primary_hub = safe_lgl_col(cur_data(), c("eligible_primary_hub")),
    eligible_strong_hub = safe_lgl_col(cur_data(), c("eligible_strong_hub")),
    eligible_full_hub = safe_lgl_col(cur_data(), c("eligible_full_hub")),

    cl_binary = safe_int_col(cur_data(), c("cl_binary", "cl")),
    hu_binary = safe_int_col(cur_data(), c("hu_binary", "hu")),
    nbs_binary = safe_int_col(cur_data(), c("nbs_binary", "nbs")),
    tr_binary = safe_int_col(cur_data(), c("tr_binary", "tr")),
    hub_service_score = safe_int_col(cur_data(), c("hub_service_score")),

    facility_name_standard = safe_chr_col(cur_data(), c("hub_name", "facility_name", "master_name")),
    facility_level_standard = safe_chr_col(cur_data(), c("hub_level", "facility_type", "master_type")),
    facility_ownership_standard = safe_chr_col(cur_data(), c("ownership", "master_ownership")),
    facility_subregion_standard = safe_chr_col(cur_data(), c("subregion", "master_region"))
  ) %>%
  filter(coordinate_valid_final)

# ----------------------------
# 7. Create Tier 1 current hub strengthening list
# ----------------------------

tier1_current_hubs <- hub_workload %>%
  mutate(
    strengthening_tier = case_when(
      route_risk_category == "high_route_risk" |
        hub_priority_flag == "partial_service_hub_with_route_risk" ~
        "tier_1_current_hub_strengthening",

      route_risk_category == "moderate_route_risk" |
        hub_service_score == 2 |
        linked_spokes >= 5 ~
        "tier_1b_current_hub_review",

      TRUE ~
        "monitor"
    ),

    strengthening_reason = case_when(
      route_risk_category == "high_route_risk" & hub_service_score <= 2 ~
        "Current assigned hub has high route risk and partial SCD service readiness",

      route_risk_category == "high_route_risk" ~
        "Current assigned hub has high route risk",

      hub_service_score == 2 & linked_spokes >= 5 ~
        "Current partial hub has moderate or higher linked-spoke workload",

      hub_service_score == 2 ~
        "Current assigned hub has partial SCD service readiness",

      linked_spokes >= 5 ~
        "Current assigned hub has moderate or higher linked-spoke workload",

      TRUE ~
        "Current assigned hub retained for monitoring"
    ),

    missing_specialised_followup_clinic = cl_binary == 0,
    missing_hydroxyurea = hu_binary == 0,
    missing_newborn_screening = nbs_binary == 0,
    missing_transfusion = tr_binary == 0,

    priority_score = case_when(
      strengthening_tier == "tier_1_current_hub_strengthening" ~ 5L,
      strengthening_tier == "tier_1b_current_hub_review" ~ 4L,
      TRUE ~ 2L
    )
  ) %>%
  transmute(
    facility_id = hub_id,
    facility_name = assigned_hub_name,
    facility_level = assigned_hub_level,
    ownership = assigned_hub_ownership,
    subregion = assigned_hub_subregion,
    longitude = NA_real_,
    latitude = NA_real_,
    strengthening_tier,
    strengthening_reason,
    priority_score,
    source_role = "current_assigned_hub",
    linked_spokes,
    linked_existing_nbs_spokes,
    linked_collocated_candidate_spokes,
    median_assigned_distance_km,
    max_assigned_distance_km,
    weak_or_long_routes,
    pct_weak_or_long_routes,
    route_risk_category,
    workload_category,
    hub_priority_flag,
    cl_binary,
    hu_binary,
    nbs_binary,
    tr_binary,
    hub_service_score,
    hub_service_deficit,
    hub_readiness,
    missing_specialised_followup_clinic,
    missing_hydroxyurea,
    missing_newborn_screening,
    missing_transfusion
  )

# Attach coordinates from hub layer to Tier 1 hubs.
tier1_current_hubs <- tier1_current_hubs %>%
  left_join(
    hub_layer %>%
      select(
        facility_id = hub_id,
        hub_longitude = longitude,
        hub_latitude = latitude
      ),
    by = "facility_id"
  ) %>%
  mutate(
    longitude = if_else(is.na(longitude), hub_longitude, longitude),
    latitude = if_else(is.na(latitude), hub_latitude, latitude)
  ) %>%
  select(
    -hub_longitude,
    -hub_latitude
  )

# ----------------------------
# 8. Identify weak-route spokes
# ----------------------------

weak_route_spokes <- assigned_primary %>%
  filter(
    weak_or_long_route
  ) %>%
  select(
    spoke_id,
    spoke_name,
    spoke_type,
    existing_nbs,
    collocated_candidate_nbs,
    point_longitude,
    point_latitude,
    assigned_hub_id = hub_id,
    assigned_hub_name,
    assigned_distance_km,
    route_feasibility,
    long_route,
    very_long_route
  )

# ----------------------------
# 9. Identify nearby candidate facilities for weak-route spokes
# ----------------------------

# Candidate upgrade sites are non-primary or weak facilities from the 280-facility
# layer that are geographically close to spokes with weak or long routes.
#
# We use 25 km as a local search radius for potential upgrade candidates.
# These facilities are NOT treated as ready hubs. They are validation candidates.

if (nrow(weak_route_spokes) > 0) {
  weak_spokes_sf <- weak_route_spokes %>%
    st_as_sf(
      coords = c("point_longitude", "point_latitude"),
      crs = 4326,
      remove = FALSE
    ) %>%
    st_transform(32636)

  candidate_facilities_sf <- hub_layer %>%
    filter(
      !hub_id %in% tier1_current_hubs$facility_id
    ) %>%
    st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    ) %>%
    st_transform(32636)

  candidate_distance_m <- st_distance(
    weak_spokes_sf,
    candidate_facilities_sf
  )

  candidate_distance_km <- as.numeric(candidate_distance_m) / 1000

  candidate_distance_tbl <- as_tibble(
    matrix(
      candidate_distance_km,
      nrow = nrow(weak_spokes_sf),
      ncol = nrow(candidate_facilities_sf),
      byrow = FALSE
    )
  )

  names(candidate_distance_tbl) <- candidate_facilities_sf$hub_id

  weak_spoke_candidate_pairs <- candidate_distance_tbl %>%
    mutate(
      spoke_id = weak_spokes_sf$spoke_id
    ) %>%
    pivot_longer(
      cols = -spoke_id,
      names_to = "candidate_facility_id",
      values_to = "candidate_distance_km"
    ) %>%
    filter(candidate_distance_km <= 25) %>%
    left_join(
      weak_spokes_sf %>%
        st_drop_geometry() %>%
        select(
          spoke_id,
          spoke_name,
          spoke_type,
          assigned_hub_id,
          assigned_hub_name,
          assigned_distance_km,
          route_feasibility
        ),
      by = "spoke_id"
    ) %>%
    left_join(
      candidate_facilities_sf %>%
        st_drop_geometry() %>%
        select(
          candidate_facility_id = hub_id,
          facility_name = facility_name_standard,
          facility_level = facility_level_standard,
          ownership = facility_ownership_standard,
          subregion = facility_subregion_standard,
          longitude,
          latitude,
          cl_binary,
          hu_binary,
          nbs_binary,
          tr_binary,
          hub_service_score,
          hub_service_deficit,
          hub_readiness,
          eligible_primary_hub
        ),
      by = "candidate_facility_id"
    ) %>%
    mutate(
      candidate_distance_km = round(candidate_distance_km, 1)
    )

  tier2_candidate_facilities <- weak_spoke_candidate_pairs %>%
    group_by(
      candidate_facility_id,
      facility_name,
      facility_level,
      ownership,
      subregion,
      longitude,
      latitude,
      cl_binary,
      hu_binary,
      nbs_binary,
      tr_binary,
      hub_service_score,
      hub_service_deficit,
      hub_readiness,
      eligible_primary_hub
    ) %>%
    summarise(
      nearby_weak_route_spokes = n_distinct(spoke_id),
      nearest_weak_route_spoke_distance_km = round(safe_min(candidate_distance_km), 1),
      median_weak_route_spoke_distance_km = round(safe_median(candidate_distance_km), 1),
      weak_route_spoke_names = paste(sort(unique(spoke_name)), collapse = "; "),
      current_assigned_hubs_for_nearby_spokes = paste(sort(unique(assigned_hub_name)), collapse = "; "),
      .groups = "drop"
    ) %>%
    mutate(
      strengthening_tier = case_when(
        eligible_primary_hub ~ "monitor_existing_eligible_candidate_near_weak_route",
        hub_service_score == 1 ~ "tier_2_candidate_facility_near_weak_route",
        hub_service_score == 0 ~ "tier_2_candidate_facility_near_weak_route",
        TRUE ~ "tier_2_candidate_facility_near_weak_route"
      ),

      strengthening_reason = case_when(
        eligible_primary_hub ~
          "Eligible facility near weak-route spoke, consider as alternative or backup hub after validation",

        TRUE ~
          "Non-primary or weak facility near weak-route spoke, consider for field validation and possible staged upgrade"
      ),

      priority_score = case_when(
        nearby_weak_route_spokes >= 3 & hub_service_score >= 1 ~ 4L,
        nearby_weak_route_spokes >= 3 ~ 3L,
        nearby_weak_route_spokes >= 1 & hub_service_score >= 1 ~ 3L,
        TRUE ~ 2L
      ),

      source_role = "candidate_facility_near_weak_route",

      linked_spokes = NA_integer_,
      linked_existing_nbs_spokes = NA_integer_,
      linked_collocated_candidate_spokes = NA_integer_,
      median_assigned_distance_km = NA_real_,
      max_assigned_distance_km = NA_real_,
      weak_or_long_routes = NA_integer_,
      pct_weak_or_long_routes = NA_real_,
      route_risk_category = NA_character_,
      workload_category = NA_character_,
      hub_priority_flag = NA_character_,

      missing_specialised_followup_clinic = cl_binary == 0,
      missing_hydroxyurea = hu_binary == 0,
      missing_newborn_screening = nbs_binary == 0,
      missing_transfusion = tr_binary == 0
    ) %>%
    transmute(
      facility_id = candidate_facility_id,
      facility_name,
      facility_level,
      ownership,
      subregion,
      longitude,
      latitude,
      strengthening_tier,
      strengthening_reason,
      priority_score,
      source_role,
      linked_spokes,
      linked_existing_nbs_spokes,
      linked_collocated_candidate_spokes,
      median_assigned_distance_km,
      max_assigned_distance_km,
      weak_or_long_routes,
      pct_weak_or_long_routes,
      route_risk_category,
      workload_category,
      hub_priority_flag,
      cl_binary,
      hu_binary,
      nbs_binary,
      tr_binary,
      hub_service_score,
      hub_service_deficit,
      hub_readiness,
      missing_specialised_followup_clinic,
      missing_hydroxyurea,
      missing_newborn_screening,
      missing_transfusion,
      nearby_weak_route_spokes,
      nearest_weak_route_spoke_distance_km,
      median_weak_route_spoke_distance_km,
      weak_route_spoke_names,
      current_assigned_hubs_for_nearby_spokes
    )
} else {
  weak_spoke_candidate_pairs <- tibble()
  tier2_candidate_facilities <- tibble()
}

# ----------------------------
# 10. Combine strengthening facilities
# ----------------------------

facilities_to_strengthen <- bind_rows(
  tier1_current_hubs,
  tier2_candidate_facilities
) %>%
  mutate(
    missing_service_count =
      as.integer(missing_specialised_followup_clinic) +
      as.integer(missing_hydroxyurea) +
      as.integer(missing_newborn_screening) +
      as.integer(missing_transfusion),

    dominant_service_gap = case_when(
      missing_specialised_followup_clinic ~ "specialised_scd_clinic_followup_gap",
      missing_hydroxyurea ~ "hydroxyurea_gap",
      missing_transfusion ~ "transfusion_gap",
      missing_newborn_screening ~ "newborn_screening_gap",
      TRUE ~ "no_major_gap"
    ),

    missing_service_domains = paste(
      c(
        ifelse(missing_specialised_followup_clinic, "specialised_scd_clinic_followup", NA_character_),
        ifelse(missing_hydroxyurea, "hydroxyurea", NA_character_),
        ifelse(missing_newborn_screening, "newborn_screening", NA_character_),
        ifelse(missing_transfusion, "transfusion", NA_character_)
      ),
      collapse = "; "
    ),

    missing_service_domains = str_replace_all(
      missing_service_domains,
      "NA; |; NA|NA",
      ""
    ),

    missing_service_domains = str_squish(missing_service_domains),

    missing_service_domains = if_else(
      missing_service_domains == "",
      "none",
      missing_service_domains
    ),

    coordinate_valid = valid_uganda_coordinates(longitude, latitude)
  ) %>%
  arrange(
    desc(priority_score),
    strengthening_tier,
    desc(linked_spokes),
    facility_name
  )

# ----------------------------
# 11. Summaries
# ----------------------------

strengthening_summary <- tibble(
  metric = c(
    "facilities_identified_for_strengthening_or_monitoring",
    "tier_1_current_hub_strengthening",
    "tier_1b_current_hub_review",
    "tier_2_candidate_facility_near_weak_route",
    "monitor_or_backup_facilities",
    "facilities_with_valid_coordinates",
    "facilities_missing_specialised_followup_clinic",
    "facilities_missing_hydroxyurea",
    "facilities_missing_newborn_screening",
    "facilities_missing_transfusion",
    "facilities_with_no_major_service_gap"
  ),
  value = c(
    nrow(facilities_to_strengthen),
    sum(facilities_to_strengthen$strengthening_tier == "tier_1_current_hub_strengthening", na.rm = TRUE),
    sum(facilities_to_strengthen$strengthening_tier == "tier_1b_current_hub_review", na.rm = TRUE),
    sum(facilities_to_strengthen$strengthening_tier == "tier_2_candidate_facility_near_weak_route", na.rm = TRUE),
    sum(str_detect(facilities_to_strengthen$strengthening_tier, "monitor"), na.rm = TRUE),
    sum(facilities_to_strengthen$coordinate_valid, na.rm = TRUE),
    sum(facilities_to_strengthen$missing_specialised_followup_clinic, na.rm = TRUE),
    sum(facilities_to_strengthen$missing_hydroxyurea, na.rm = TRUE),
    sum(facilities_to_strengthen$missing_newborn_screening, na.rm = TRUE),
    sum(facilities_to_strengthen$missing_transfusion, na.rm = TRUE),
    sum(facilities_to_strengthen$dominant_service_gap == "no_major_gap", na.rm = TRUE)
  )
)

strengthening_tier_summary <- facilities_to_strengthen %>%
  count(
    strengthening_tier,
    name = "n_facilities"
  ) %>%
  mutate(
    pct_facilities = round(100 * n_facilities / sum(n_facilities), 1)
  )

dominant_gap_summary <- facilities_to_strengthen %>%
  count(
    dominant_service_gap,
    name = "n_facilities"
  ) %>%
  mutate(
    pct_facilities = round(100 * n_facilities / sum(n_facilities), 1)
  ) %>%
  arrange(
    desc(n_facilities)
  )

service_gap_summary <- facilities_to_strengthen %>%
  summarise(
    missing_specialised_followup_clinic = sum(missing_specialised_followup_clinic, na.rm = TRUE),
    missing_hydroxyurea = sum(missing_hydroxyurea, na.rm = TRUE),
    missing_newborn_screening = sum(missing_newborn_screening, na.rm = TRUE),
    missing_transfusion = sum(missing_transfusion, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "service_gap",
    values_to = "n_facilities"
  ) %>%
  mutate(
    pct_facilities = round(100 * n_facilities / nrow(facilities_to_strengthen), 1)
  ) %>%
  arrange(desc(n_facilities))

top_strengthening_facilities <- facilities_to_strengthen %>%
  arrange(
    desc(priority_score),
    desc(linked_spokes),
    desc(nearby_weak_route_spokes),
    facility_name
  ) %>%
  slice_head(n = 30)

print(strengthening_summary)
print(strengthening_tier_summary)
print(dominant_gap_summary)
print(service_gap_summary)
print(top_strengthening_facilities)

# ----------------------------
# 12. Save outputs
# ----------------------------

write_csv(
  facilities_to_strengthen,
  file.path(
    processed_facility_dir,
    "facilities_to_strengthen_scd_services.csv"
  )
)

write_csv(
  strengthening_summary,
  file.path(
    processed_facility_dir,
    "facilities_to_strengthen_scd_services_summary.csv"
  )
)

write_csv(
  strengthening_tier_summary,
  file.path(
    processed_facility_dir,
    "facilities_to_strengthen_tier_summary.csv"
  )
)

write_csv(
  dominant_gap_summary,
  file.path(
    processed_facility_dir,
    "facilities_to_strengthen_dominant_gap_summary.csv"
  )
)

write_csv(
  service_gap_summary,
  file.path(
    processed_facility_dir,
    "facilities_to_strengthen_service_gap_summary.csv"
  )
)

write_csv(
  top_strengthening_facilities,
  file.path(
    processed_facility_dir,
    "top_facilities_to_strengthen_scd_services.csv"
  )
)

write_csv(
  weak_spoke_candidate_pairs,
  file.path(
    processed_facility_dir,
    "weak_route_spoke_candidate_facility_pairs.csv"
  )
)

write_xlsx(
  list(
    strengthening_summary = strengthening_summary,
    facilities_to_strengthen = facilities_to_strengthen,
    strengthening_tier_summary = strengthening_tier_summary,
    dominant_gap_summary = dominant_gap_summary,
    service_gap_summary = service_gap_summary,
    top_strengthening_facilities = top_strengthening_facilities,
    weak_route_spoke_candidate_pairs = weak_spoke_candidate_pairs
  ),
  file.path(
    output_table_dir,
    "08_facilities_to_strengthen_scd_services.xlsx"
  )
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
  paste0("## ", Sys.Date(), " - Identified SCD facilities for strengthening"),
  "",
  paste0("- Facilities identified: ", nrow(facilities_to_strengthen), "."),
  paste0("- Tier 1 current hub strengthening: ",
         sum(facilities_to_strengthen$strengthening_tier == "tier_1_current_hub_strengthening", na.rm = TRUE), "."),
  paste0("- Tier 2 candidate facilities near weak routes: ",
         sum(facilities_to_strengthen$strengthening_tier == "tier_2_candidate_facility_near_weak_route", na.rm = TRUE), "."),
  paste0("- Missing specialised follow-up clinic: ",
         sum(facilities_to_strengthen$missing_specialised_followup_clinic, na.rm = TRUE), "."),
  paste0("- Missing hydroxyurea: ",
         sum(facilities_to_strengthen$missing_hydroxyurea, na.rm = TRUE), "."),
  paste0("- Missing newborn screening: ",
         sum(facilities_to_strengthen$missing_newborn_screening, na.rm = TRUE), "."),
  paste0("- Missing transfusion: ",
         sum(facilities_to_strengthen$missing_transfusion, na.rm = TRUE), ".")
)

cat(
  paste(log_entry, collapse = "\n"),
  file = log_path,
  append = TRUE
)

message("Facility strengthening identification complete.")
message("Main output:")
message(file.path(processed_facility_dir, "facilities_to_strengthen_scd_services.csv"))
message("Summary workbook:")
message(file.path(output_table_dir, "08_facilities_to_strengthen_scd_services.xlsx"))