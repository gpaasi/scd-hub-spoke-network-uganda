# ============================================================
# 04_assign_spokes_to_nearest_hubs.R
# Assign each NBS spoke to nearest eligible SCD care hub
# ============================================================

# Purpose:
# Convert the all-pair scenario route matrix into an operational
# hub-and-spoke assignment.
#
# For each scenario:
# - each spoke is assigned to the nearest eligible hub
# - assigned distance is classified
# - route feasibility is classified using distance and hub readiness
#
# Main input:
# - 02_processed_data/routes/spoke_hub_distance_matrix_by_scenario.csv
#
# Main outputs:
# - 02_processed_data/routes/assigned_spoke_hub_routes_by_scenario.csv
# - 02_processed_data/routes/assigned_spoke_hub_routes_primary.csv
# - 02_processed_data/routes/assigned_spoke_hub_routes_summary_by_scenario.csv
# - 04_outputs/tables/04_assigned_spoke_hub_routes_summary.xlsx

# ----------------------------
# 1. Load packages
# ----------------------------

required_packages <- c(
  "readr",
  "dplyr",
  "stringr",
  "janitor",
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

scenario_route_file <- file.path(
  processed_route_dir,
  "spoke_hub_distance_matrix_by_scenario.csv"
)

if (!file.exists(scenario_route_file)) {
  stop(
    paste0(
      "Missing scenario route matrix:\n",
      scenario_route_file,
      "\n\nRun Script 03 first."
    )
  )
}

# ----------------------------
# 3. Helper functions
# ----------------------------

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

  dplyr::case_when(
    x_chr %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ FALSE
  )
}

assigned_distance_band <- function(distance_km) {
  case_when(
    is.na(distance_km) ~ "missing",
    distance_km <= 25 ~ "within_25_km",
    distance_km > 25 & distance_km <= 50 ~ "25_to_50_km",
    distance_km > 50 & distance_km <= 100 ~ "50_to_100_km",
    distance_km > 100 ~ "over_100_km",
    TRUE ~ "missing"
  )
}

route_feasibility_class <- function(distance_km, hub_service_score) {
  case_when(
    is.na(distance_km) | is.na(hub_service_score) ~ "missing",

    distance_km <= 25 & hub_service_score >= 3 ~
      "functional",

    distance_km <= 25 & hub_service_score == 2 ~
      "partial",

    distance_km > 25 & distance_km <= 50 & hub_service_score >= 3 ~
      "moderate_functional",

    distance_km > 25 & distance_km <= 50 & hub_service_score == 2 ~
      "moderate_partial",

    distance_km > 50 & distance_km <= 100 ~
      "long",

    distance_km > 100 ~
      "very_long",

    TRUE ~ "unclassified"
  )
}

route_feasibility_group <- function(route_feasibility) {
  case_when(
    route_feasibility %in% c("functional", "moderate_functional") ~
      "stronger_linkage",

    route_feasibility %in% c("partial", "moderate_partial") ~
      "partial_linkage",

    route_feasibility %in% c("long", "very_long") ~
      "distance_constrained_linkage",

    TRUE ~ "unclassified"
  )
}

# ----------------------------
# 4. Read scenario route matrix
# ----------------------------

scenario_routes <- read_csv(
  scenario_route_file,
  show_col_types = FALSE
) %>%
  clean_names()

message("Rows read from scenario route matrix: ", nrow(scenario_routes))
message("Columns found:")
print(names(scenario_routes))

# ----------------------------
# 5. Validate required columns
# ----------------------------

required_cols <- c(
  "scenario",
  "scenario_label",
  "spoke_id",
  "spoke_name",
  "spoke_type",
  "hub_id",
  "hub_name",
  "hub_level",
  "distance_km",
  "hub_service_score",
  "hub_readiness",
  "cl_binary",
  "hu_binary",
  "nbs_binary",
  "tr_binary"
)

missing_cols <- setdiff(
  required_cols,
  names(scenario_routes)
)

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Scenario route matrix is missing required columns:\n",
      paste(missing_cols, collapse = "\n"),
      "\n\nColumns found:\n",
      paste(names(scenario_routes), collapse = ", ")
    )
  )
}

# Optional columns are created if missing, so downstream scripts remain stable.
if (!"ownership" %in% names(scenario_routes)) {
  scenario_routes$ownership <- NA_character_
}

if (!"subregion" %in% names(scenario_routes)) {
  scenario_routes$subregion <- NA_character_
}

if (!"matched_district" %in% names(scenario_routes)) {
  scenario_routes$matched_district <- NA_character_
}

if (!"catchment_population" %in% names(scenario_routes)) {
  scenario_routes$catchment_population <- NA_real_
}

if (!"scd_admissions" %in% names(scenario_routes)) {
  scenario_routes$scd_admissions <- NA_real_
}

if (!"scd_deaths" %in% names(scenario_routes)) {
  scenario_routes$scd_deaths <- NA_real_
}

if (!"hbs_mean" %in% names(scenario_routes)) {
  scenario_routes$hbs_mean <- NA_real_
}

if (!"pfpr_mean" %in% names(scenario_routes)) {
  scenario_routes$pfpr_mean <- NA_real_
}

# ----------------------------
# 6. Standardise values
# ----------------------------

scenario_routes <- scenario_routes %>%
  mutate(
    distance_km = suppressWarnings(as.numeric(distance_km)),
    hub_service_score = suppressWarnings(as.integer(hub_service_score)),
    cl_binary = suppressWarnings(as.integer(cl_binary)),
    hu_binary = suppressWarnings(as.integer(hu_binary)),
    nbs_binary = suppressWarnings(as.integer(nbs_binary)),
    tr_binary = suppressWarnings(as.integer(tr_binary)),
    existing_nbs = if ("existing_nbs" %in% names(.)) {
      suppressWarnings(as.integer(existing_nbs))
    } else {
      NA_integer_
    },
    collocated_candidate_nbs = if ("collocated_candidate_nbs" %in% names(.)) {
      suppressWarnings(as.integer(collocated_candidate_nbs))
    } else {
      NA_integer_
    }
  )

if (any(is.na(scenario_routes$distance_km))) {
  stop("Some scenario routes have missing distance_km. Check Script 03 output.")
}

# ----------------------------
# 7. Assign nearest hub per spoke per scenario
# ----------------------------

assigned_routes <- scenario_routes %>%
  group_by(
    scenario,
    scenario_label,
    spoke_id
  ) %>%
  slice_min(
    order_by = distance_km,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  mutate(
    assigned_distance_km = distance_km,

    assigned_distance_band = assigned_distance_band(
      assigned_distance_km
    ),

    route_feasibility = route_feasibility_class(
      assigned_distance_km,
      hub_service_score
    ),

    route_feasibility_group = route_feasibility_group(
      route_feasibility
    ),

    assigned_hub_readiness_group = case_when(
      hub_service_score == 4 ~ "full_hub",
      hub_service_score == 3 ~ "strong_hub",
      hub_service_score == 2 ~ "partial_hub",
      hub_service_score == 1 ~ "weak_hub",
      hub_service_score == 0 ~ "not_ready",
      TRUE ~ "unknown"
    ),

    weak_or_long_route = route_feasibility %in% c(
      "partial",
      "moderate_partial",
      "long",
      "very_long"
    ),

    long_route = assigned_distance_km > 50,

    very_long_route = assigned_distance_km > 100,

    route_priority_flag = case_when(
      route_feasibility %in% c("functional", "moderate_functional") ~
        "standard_linkage",

      route_feasibility %in% c("partial", "moderate_partial") ~
        "strengthen_assigned_hub",

      route_feasibility %in% c("long", "very_long") ~
        "review_distance_and_hub_options",

      TRUE ~ "review"
    )
  ) %>%
  select(
    scenario,
    scenario_label,
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
    assigned_hub_name = hub_name,
    assigned_hub_level = hub_level,
    assigned_hub_ownership = ownership,
    assigned_hub_subregion = subregion,
    longitude,
    latitude,
    assigned_distance_km,
    assigned_distance_band,
    route_feasibility,
    route_feasibility_group,
    route_priority_flag,
    weak_or_long_route,
    long_route,
    very_long_route,
    cl_binary,
    hu_binary,
    nbs_binary,
    tr_binary,
    hub_service_score,
    hub_service_deficit,
    hub_readiness,
    assigned_hub_readiness_group,
    catchment_population,
    scd_admissions,
    scd_deaths,
    hbs_mean,
    pfpr_mean
  ) %>%
  arrange(
    scenario,
    assigned_distance_km,
    spoke_name
  )

# ----------------------------
# 8. Primary scenario output
# ----------------------------

assigned_primary <- assigned_routes %>%
  filter(
    scenario == "primary_score_ge_2"
  ) %>%
  arrange(
    assigned_distance_km,
    spoke_name
  )

if (nrow(assigned_primary) == 0) {
  stop("No assigned primary routes found. Check scenario names from Script 03.")
}

# ----------------------------
# 9. Quality checks
# ----------------------------

assignment_qc <- assigned_routes %>%
  group_by(
    scenario,
    scenario_label
  ) %>%
  summarise(
    assigned_spokes = n_distinct(spoke_id),
    assigned_routes = n(),
    assigned_hubs_used = n_distinct(hub_id),
    missing_assigned_distance = sum(is.na(assigned_distance_km)),
    missing_assigned_hub = sum(is.na(assigned_hub_name)),
    .groups = "drop"
  ) %>%
  arrange(
    scenario
  )

primary_qc <- tibble(
  metric = c(
    "primary_assigned_spokes",
    "primary_assigned_routes",
    "primary_hubs_used",
    "primary_routes_with_missing_distance",
    "primary_routes_with_missing_hub",
    "primary_weak_or_long_routes",
    "primary_long_routes",
    "primary_very_long_routes"
  ),
  value = c(
    n_distinct(assigned_primary$spoke_id),
    nrow(assigned_primary),
    n_distinct(assigned_primary$hub_id),
    sum(is.na(assigned_primary$assigned_distance_km)),
    sum(is.na(assigned_primary$assigned_hub_name)),
    sum(assigned_primary$weak_or_long_route, na.rm = TRUE),
    sum(assigned_primary$long_route, na.rm = TRUE),
    sum(assigned_primary$very_long_route, na.rm = TRUE)
  )
)

print(assignment_qc)
print(primary_qc)

if (any(assignment_qc$missing_assigned_distance > 0)) {
  stop("Some assigned routes have missing distances.")
}

if (any(assignment_qc$missing_assigned_hub > 0)) {
  stop("Some assigned routes have missing hub names.")
}

# ----------------------------
# 10. Scenario summaries
# ----------------------------

assigned_summary_by_scenario <- assigned_routes %>%
  group_by(
    scenario,
    scenario_label
  ) %>%
  summarise(
    assigned_spokes = n_distinct(spoke_id),
    assigned_routes = n(),
    assigned_hubs_used = n_distinct(hub_id),
    median_distance_km = round(safe_median(assigned_distance_km), 1),
    mean_distance_km = round(safe_mean(assigned_distance_km), 1),
    min_distance_km = round(safe_min(assigned_distance_km), 1),
    max_distance_km = round(safe_max(assigned_distance_km), 1),
    within_25_km = sum(assigned_distance_km <= 25, na.rm = TRUE),
    within_50_km = sum(assigned_distance_km <= 50, na.rm = TRUE),
    within_100_km = sum(assigned_distance_km <= 100, na.rm = TRUE),
    pct_within_25_km = round(100 * within_25_km / assigned_routes, 1),
    pct_within_50_km = round(100 * within_50_km / assigned_routes, 1),
    pct_within_100_km = round(100 * within_100_km / assigned_routes, 1),
    weak_or_long_routes = sum(weak_or_long_route, na.rm = TRUE),
    long_routes = sum(long_route, na.rm = TRUE),
    very_long_routes = sum(very_long_route, na.rm = TRUE),
    pct_weak_or_long_routes = round(100 * weak_or_long_routes / assigned_routes, 1),
    .groups = "drop"
  )

assigned_distance_band_summary <- assigned_routes %>%
  count(
    scenario,
    scenario_label,
    assigned_distance_band,
    name = "n_routes"
  ) %>%
  group_by(
    scenario,
    scenario_label
  ) %>%
  mutate(
    pct_routes = round(100 * n_routes / sum(n_routes), 1)
  ) %>%
  ungroup()

assigned_feasibility_summary <- assigned_routes %>%
  count(
    scenario,
    scenario_label,
    route_feasibility,
    route_feasibility_group,
    name = "n_routes"
  ) %>%
  group_by(
    scenario,
    scenario_label
  ) %>%
  mutate(
    pct_routes = round(100 * n_routes / sum(n_routes), 1)
  ) %>%
  ungroup()

assigned_hub_readiness_summary <- assigned_routes %>%
  count(
    scenario,
    scenario_label,
    assigned_hub_readiness_group,
    name = "n_routes"
  ) %>%
  group_by(
    scenario,
    scenario_label
  ) %>%
  mutate(
    pct_routes = round(100 * n_routes / sum(n_routes), 1)
  ) %>%
  ungroup()

primary_feasibility_summary <- assigned_primary %>%
  count(
    route_feasibility,
    route_feasibility_group,
    name = "n_routes"
  ) %>%
  mutate(
    pct_routes = round(100 * n_routes / sum(n_routes), 1)
  )

primary_hub_readiness_summary <- assigned_primary %>%
  count(
    assigned_hub_readiness_group,
    name = "n_routes"
  ) %>%
  mutate(
    pct_routes = round(100 * n_routes / sum(n_routes), 1)
  )

print(assigned_summary_by_scenario)
print(primary_feasibility_summary)
print(primary_hub_readiness_summary)

# ----------------------------
# 11. Save outputs
# ----------------------------

write_csv(
  assigned_routes,
  file.path(
    processed_route_dir,
    "assigned_spoke_hub_routes_by_scenario.csv"
  )
)

write_csv(
  assigned_primary,
  file.path(
    processed_route_dir,
    "assigned_spoke_hub_routes_primary.csv"
  )
)

write_csv(
  assigned_summary_by_scenario,
  file.path(
    processed_route_dir,
    "assigned_spoke_hub_routes_summary_by_scenario.csv"
  )
)

write_csv(
  assigned_distance_band_summary,
  file.path(
    processed_route_dir,
    "assigned_spoke_hub_distance_band_summary.csv"
  )
)

write_csv(
  assigned_feasibility_summary,
  file.path(
    processed_route_dir,
    "assigned_spoke_hub_feasibility_summary.csv"
  )
)

write_csv(
  assigned_hub_readiness_summary,
  file.path(
    processed_route_dir,
    "assigned_spoke_hub_readiness_summary.csv"
  )
)

write_csv(
  primary_feasibility_summary,
  file.path(
    processed_route_dir,
    "assigned_primary_feasibility_summary.csv"
  )
)

write_csv(
  primary_hub_readiness_summary,
  file.path(
    processed_route_dir,
    "assigned_primary_hub_readiness_summary.csv"
  )
)

write_csv(
  assignment_qc,
  file.path(
    log_dir,
    "04_assignment_qc_by_scenario.csv"
  )
)

write_csv(
  primary_qc,
  file.path(
    log_dir,
    "04_assignment_primary_qc.csv"
  )
)

write_xlsx(
  list(
    assignment_qc = assignment_qc,
    primary_qc = primary_qc,
    assigned_summary_by_scenario = assigned_summary_by_scenario,
    primary_feasibility_summary = primary_feasibility_summary,
    primary_hub_readiness_summary = primary_hub_readiness_summary,
    assigned_distance_band_summary = assigned_distance_band_summary,
    assigned_feasibility_summary = assigned_feasibility_summary,
    assigned_hub_readiness_summary = assigned_hub_readiness_summary
  ),
  file.path(
    output_table_dir,
    "04_assigned_spoke_hub_routes_summary.xlsx"
  )
)

# ----------------------------
# 12. Log
# ----------------------------

log_path <- file.path(
  admin_dir,
  "analysis_log_updated_spoke_layer.md"
)

log_entry <- c(
  "",
  paste0("## ", Sys.Date(), " - Assigned spokes to nearest eligible hubs"),
  "",
  paste0("- Scenario assignments generated: ", nrow(assigned_routes), "."),
  paste0("- Primary assigned spokes: ", n_distinct(assigned_primary$spoke_id), "."),
  paste0("- Primary hubs used: ", n_distinct(assigned_primary$hub_id), "."),
  paste0("- Primary median assigned distance, km: ",
         round(safe_median(assigned_primary$assigned_distance_km), 1), "."),
  paste0("- Primary mean assigned distance, km: ",
         round(safe_mean(assigned_primary$assigned_distance_km), 1), "."),
  paste0("- Primary maximum assigned distance, km: ",
         round(safe_max(assigned_primary$assigned_distance_km), 1), "."),
  paste0("- Primary weak or long routes: ",
         sum(assigned_primary$weak_or_long_route, na.rm = TRUE), ".")
)

cat(
  paste(log_entry, collapse = "\n"),
  file = log_path,
  append = TRUE
)

message("Nearest-hub assignment complete.")
message("Assigned routes by scenario:")
message(file.path(processed_route_dir, "assigned_spoke_hub_routes_by_scenario.csv"))
message("Primary assigned routes:")
message(file.path(processed_route_dir, "assigned_spoke_hub_routes_primary.csv"))
message("Summary workbook:")
message(file.path(output_table_dir, "04_assigned_spoke_hub_routes_summary.xlsx"))