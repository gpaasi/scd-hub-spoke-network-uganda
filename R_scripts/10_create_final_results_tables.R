# ============================================================
# 10_create_final_results_tables.R
# Create final manuscript and supplementary results tables
# ============================================================

# Purpose:
# Consolidate the updated 168-spoke analysis outputs into final
# manuscript-ready and supplementary tables.
#
# Inputs:
# - 02_processed_data/facilities/nbs_spoke_layer_geocoded_accepted.csv
# - 02_processed_data/facilities/essential_care_hub_layer.csv
# - 02_processed_data/routes/spoke_hub_distance_matrix_by_scenario_summary.csv
# - 02_processed_data/routes/assigned_spoke_hub_routes_primary.csv
# - 02_processed_data/routes/assigned_spoke_hub_routes_summary_by_scenario.csv
# - 02_processed_data/routes/assigned_primary_feasibility_summary.csv
# - 02_processed_data/routes/hub_workload_route_risk_primary.csv
# - 02_processed_data/routes/hub_workload_route_risk_summary.csv
# - 02_processed_data/facilities/facilities_to_strengthen_scd_services.csv
# - 02_processed_data/facilities/facilities_to_strengthen_scd_services_summary.csv
#
# Outputs:
# - 04_outputs/tables/10_final_manuscript_results_tables.xlsx
# - 04_outputs/tables/10_final_supplementary_tables.xlsx
# - 04_outputs/tables/10_final_results_index.csv

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

for (d in c(output_table_dir, log_dir, admin_dir)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

# ----------------------------
# 3. Define input files
# ----------------------------

input_files <- list(
  nbs_spokes = file.path(
    processed_facility_dir,
    "nbs_spoke_layer_geocoded_accepted.csv"
  ),

  hub_layer = file.path(
    processed_facility_dir,
    "essential_care_hub_layer.csv"
  ),

  scenario_distance_summary = file.path(
    processed_route_dir,
    "spoke_hub_distance_matrix_by_scenario_summary.csv"
  ),

  assigned_primary = file.path(
    processed_route_dir,
    "assigned_spoke_hub_routes_primary.csv"
  ),

  assigned_summary_by_scenario = file.path(
    processed_route_dir,
    "assigned_spoke_hub_routes_summary_by_scenario.csv"
  ),

  assigned_primary_feasibility = file.path(
    processed_route_dir,
    "assigned_primary_feasibility_summary.csv"
  ),

  assigned_primary_hub_readiness = file.path(
    processed_route_dir,
    "assigned_primary_hub_readiness_summary.csv"
  ),

  hub_workload = file.path(
    processed_route_dir,
    "hub_workload_route_risk_primary.csv"
  ),

  hub_workload_summary = file.path(
    processed_route_dir,
    "hub_workload_route_risk_summary.csv"
  ),

  facilities_to_strengthen = file.path(
    processed_facility_dir,
    "facilities_to_strengthen_scd_services.csv"
  ),

  facilities_strengthening_summary = file.path(
    processed_facility_dir,
    "facilities_to_strengthen_scd_services_summary.csv"
  ),

  strengthening_tier_summary = file.path(
    processed_facility_dir,
    "facilities_to_strengthen_tier_summary.csv"
  ),

  strengthening_dominant_gap_summary = file.path(
    processed_facility_dir,
    "facilities_to_strengthen_dominant_gap_summary.csv"
  ),

  strengthening_service_gap_summary = file.path(
    processed_facility_dir,
    "facilities_to_strengthen_service_gap_summary.csv"
  )
)

missing_files <- input_files[!file.exists(unlist(input_files))]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required file(s):\n",
      paste(unlist(missing_files), collapse = "\n"),
      "\n\nRun the previous scripts before creating final results tables."
    )
  )
}

# ----------------------------
# 4. Helper functions
# ----------------------------

safe_min <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

safe_pct <- function(num, den) {
  ifelse(
    is.na(den) | den == 0,
    NA_real_,
    round(100 * num / den, 1)
  )
}

safe_lgl <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  x_chr <- str_to_lower(as.character(x))

  case_when(
    x_chr %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ FALSE
  )
}

# ----------------------------
# 5. Read inputs
# ----------------------------

nbs_spokes <- read_csv(
  input_files$nbs_spokes,
  show_col_types = FALSE
) %>%
  clean_names()

hub_layer <- read_csv(
  input_files$hub_layer,
  show_col_types = FALSE
) %>%
  clean_names()

scenario_distance_summary <- read_csv(
  input_files$scenario_distance_summary,
  show_col_types = FALSE
) %>%
  clean_names()

assigned_primary <- read_csv(
  input_files$assigned_primary,
  show_col_types = FALSE
) %>%
  clean_names()

assigned_summary_by_scenario <- read_csv(
  input_files$assigned_summary_by_scenario,
  show_col_types = FALSE
) %>%
  clean_names()

assigned_primary_feasibility <- read_csv(
  input_files$assigned_primary_feasibility,
  show_col_types = FALSE
) %>%
  clean_names()

assigned_primary_hub_readiness <- read_csv(
  input_files$assigned_primary_hub_readiness,
  show_col_types = FALSE
) %>%
  clean_names()

hub_workload <- read_csv(
  input_files$hub_workload,
  show_col_types = FALSE
) %>%
  clean_names()

hub_workload_summary <- read_csv(
  input_files$hub_workload_summary,
  show_col_types = FALSE
) %>%
  clean_names()

facilities_to_strengthen <- read_csv(
  input_files$facilities_to_strengthen,
  show_col_types = FALSE
) %>%
  clean_names()

facilities_strengthening_summary <- read_csv(
  input_files$facilities_strengthening_summary,
  show_col_types = FALSE
) %>%
  clean_names()

strengthening_tier_summary <- read_csv(
  input_files$strengthening_tier_summary,
  show_col_types = FALSE
) %>%
  clean_names()

strengthening_dominant_gap_summary <- read_csv(
  input_files$strengthening_dominant_gap_summary,
  show_col_types = FALSE
) %>%
  clean_names()

strengthening_service_gap_summary <- read_csv(
  input_files$strengthening_service_gap_summary,
  show_col_types = FALSE
) %>%
  clean_names()

# ----------------------------
# 6. Standardise important fields
# ----------------------------

assigned_primary <- assigned_primary %>%
  mutate(
    assigned_distance_km = suppressWarnings(as.numeric(assigned_distance_km)),
    hub_service_score = suppressWarnings(as.integer(hub_service_score)),
    weak_or_long_route = safe_lgl(weak_or_long_route),
    long_route = safe_lgl(long_route),
    very_long_route = safe_lgl(very_long_route)
  )

hub_layer <- hub_layer %>%
  mutate(
    hub_service_score = suppressWarnings(as.integer(hub_service_score)),
    eligible_primary_hub = safe_lgl(eligible_primary_hub),
    eligible_strong_hub = safe_lgl(eligible_strong_hub),
    eligible_full_hub = safe_lgl(eligible_full_hub),
    cl_binary = suppressWarnings(as.integer(cl_binary)),
    hu_binary = suppressWarnings(as.integer(hu_binary)),
    nbs_binary = suppressWarnings(as.integer(nbs_binary)),
    tr_binary = suppressWarnings(as.integer(tr_binary))
  )

hub_workload <- hub_workload %>%
  mutate(
    linked_spokes = suppressWarnings(as.integer(linked_spokes)),
    hub_service_score = suppressWarnings(as.integer(hub_service_score)),
    pct_weak_or_long_routes = suppressWarnings(as.numeric(pct_weak_or_long_routes))
  )

facilities_to_strengthen <- facilities_to_strengthen %>%
  mutate(
    priority_score = suppressWarnings(as.numeric(priority_score)),
    hub_service_score = suppressWarnings(as.integer(hub_service_score)),
    missing_specialised_followup_clinic = safe_lgl(missing_specialised_followup_clinic),
    missing_hydroxyurea = safe_lgl(missing_hydroxyurea),
    missing_newborn_screening = safe_lgl(missing_newborn_screening),
    missing_transfusion = safe_lgl(missing_transfusion)
  )

# ----------------------------
# 7. Main text table 1:
# Input facility and readiness profile
# ----------------------------

table_1_input_profile <- tibble(
  domain = c(
    "NBS spoke layer",
    "NBS spoke layer",
    "NBS spoke layer",
    "NBS spoke layer",
    "Candidate hub layer",
    "Candidate hub layer",
    "Candidate hub layer",
    "Candidate hub layer",
    "Candidate hub layer",
    "Candidate hub layer",
    "Candidate hub layer",
    "Candidate hub layer"
  ),
  metric = c(
    "Accepted geocoded NBS spokes",
    "Existing NBS spokes",
    "Collocated candidate NBS spokes",
    "Manual-review spoke records excluded from provisional model",
    "Candidate hub facilities",
    "Eligible primary hubs, service score >=2",
    "Strong hubs, service score >=3",
    "Full hubs, service score =4",
    "Facilities with specialised SCD clinic follow-up",
    "Facilities with hydroxyurea",
    "Facilities with newborn screening",
    "Facilities with transfusion"
  ),
  value = c(
    nrow(nbs_spokes),
    sum(nbs_spokes$existing_nbs == 1, na.rm = TRUE),
    sum(nbs_spokes$collocated_candidate_nbs == 1, na.rm = TRUE),
    43,
    nrow(hub_layer),
    sum(hub_layer$eligible_primary_hub, na.rm = TRUE),
    sum(hub_layer$eligible_strong_hub, na.rm = TRUE),
    sum(hub_layer$eligible_full_hub, na.rm = TRUE),
    sum(hub_layer$cl_binary == 1, na.rm = TRUE),
    sum(hub_layer$hu_binary == 1, na.rm = TRUE),
    sum(hub_layer$nbs_binary == 1, na.rm = TRUE),
    sum(hub_layer$tr_binary == 1, na.rm = TRUE)
  )
)

# ----------------------------
# 8. Main text table 2:
# Scenario-level distance and assignment summary
# ----------------------------

table_2_scenario_summary <- assigned_summary_by_scenario %>%
  select(
    scenario,
    scenario_label,
    assigned_spokes,
    assigned_hubs_used,
    median_distance_km,
    mean_distance_km,
    max_distance_km,
    pct_within_25_km,
    pct_within_50_km,
    pct_within_100_km,
    weak_or_long_routes,
    pct_weak_or_long_routes
  ) %>%
  arrange(
    match(
      scenario,
      c(
        "primary_score_ge_2",
        "strong_score_ge_3",
        "full_score_4",
        "treatment_any_hu_cl_tr",
        "transfusion_ready"
      )
    )
  )

# ----------------------------
# 9. Main text table 3:
# Primary route feasibility summary
# ----------------------------

table_3_primary_route_feasibility <- assigned_primary_feasibility %>%
  arrange(
    route_feasibility_group,
    route_feasibility
  )

# ----------------------------
# 10. Main text table 4:
# Hub workload and route risk
# ----------------------------

table_4_hub_workload_summary <- hub_workload_summary

table_4b_top_hubs_by_workload <- hub_workload %>%
  arrange(
    desc(linked_spokes),
    desc(pct_weak_or_long_routes),
    assigned_hub_name
  ) %>%
  select(
    hub_id,
    assigned_hub_name,
    assigned_hub_level,
    assigned_hub_ownership,
    assigned_hub_subregion,
    linked_spokes,
    linked_existing_nbs_spokes,
    linked_collocated_candidate_spokes,
    median_assigned_distance_km,
    max_assigned_distance_km,
    weak_or_long_routes,
    pct_weak_or_long_routes,
    route_risk_category,
    workload_category,
    hub_service_score,
    hub_readiness,
    missing_service_domains
  ) %>%
  slice_head(n = 20)

# ----------------------------
# 11. Main text table 5:
# Strengthening priorities
# ----------------------------

table_5_strengthening_summary <- facilities_strengthening_summary

table_5b_top_strengthening_facilities <- facilities_to_strengthen %>%
  arrange(
    desc(priority_score),
    strengthening_tier,
    desc(linked_spokes),
    desc(nearby_weak_route_spokes),
    facility_name
  ) %>%
  select(
    facility_id,
    facility_name,
    facility_level,
    ownership,
    subregion,
    strengthening_tier,
    source_role,
    priority_score,
    strengthening_reason,
    linked_spokes,
    nearby_weak_route_spokes,
    route_risk_category,
    workload_category,
    hub_service_score,
    hub_readiness,
    dominant_service_gap,
    missing_service_domains
  ) %>%
  slice_head(n = 30)

# ----------------------------
# 12. Main text table 6:
# Service gaps among strengthening facilities
# ----------------------------

table_6_service_gap_summary <- strengthening_service_gap_summary

table_6b_dominant_gap_summary <- strengthening_dominant_gap_summary

table_6c_tier_summary <- strengthening_tier_summary

# ----------------------------
# 13. Supplementary tables
# ----------------------------

supp_table_s1_all_nbs_spokes <- nbs_spokes

supp_table_s2_all_candidate_hubs <- hub_layer

supp_table_s3_primary_assigned_routes <- assigned_primary

supp_table_s4_hub_workload <- hub_workload

supp_table_s5_facilities_to_strengthen <- facilities_to_strengthen

supp_table_s6_scenario_distance_summary <- scenario_distance_summary

# ----------------------------
# 14. Results text numeric checklist
# ----------------------------

primary_summary <- assigned_primary %>%
  summarise(
    assigned_spokes = n_distinct(spoke_id),
    assigned_hubs_used = n_distinct(hub_id),
    median_distance_km = round(safe_median(assigned_distance_km), 1),
    mean_distance_km = round(safe_mean(assigned_distance_km), 1),
    min_distance_km = round(safe_min(assigned_distance_km), 1),
    max_distance_km = round(safe_max(assigned_distance_km), 1),
    within_25_km = sum(assigned_distance_km <= 25, na.rm = TRUE),
    within_50_km = sum(assigned_distance_km <= 50, na.rm = TRUE),
    within_100_km = sum(assigned_distance_km <= 100, na.rm = TRUE),
    pct_within_25_km = safe_pct(within_25_km, assigned_spokes),
    pct_within_50_km = safe_pct(within_50_km, assigned_spokes),
    pct_within_100_km = safe_pct(within_100_km, assigned_spokes),
    weak_or_long_routes = sum(weak_or_long_route, na.rm = TRUE),
    long_routes = sum(long_route, na.rm = TRUE),
    very_long_routes = sum(very_long_route, na.rm = TRUE),
    pct_weak_or_long_routes = safe_pct(weak_or_long_routes, assigned_spokes)
  )

hub_summary_checklist <- hub_workload %>%
  summarise(
    hubs_used = n_distinct(hub_id),
    median_linked_spokes = round(safe_median(linked_spokes), 1),
    mean_linked_spokes = round(safe_mean(linked_spokes), 1),
    max_linked_spokes = safe_max(linked_spokes),
    high_route_risk_hubs = sum(route_risk_category == "high_route_risk", na.rm = TRUE),
    moderate_route_risk_hubs = sum(route_risk_category == "moderate_route_risk", na.rm = TRUE),
    low_route_risk_hubs = sum(route_risk_category == "low_route_risk", na.rm = TRUE),
    full_hubs_used = sum(hub_service_score == 4, na.rm = TRUE),
    strong_hubs_used = sum(hub_service_score == 3, na.rm = TRUE),
    partial_hubs_used = sum(hub_service_score == 2, na.rm = TRUE)
  )

strengthening_checklist <- facilities_to_strengthen %>%
  summarise(
    facilities_identified = n(),
    tier_1_current_hub_strengthening = sum(strengthening_tier == "tier_1_current_hub_strengthening", na.rm = TRUE),
    tier_1b_current_hub_review = sum(strengthening_tier == "tier_1b_current_hub_review", na.rm = TRUE),
    tier_2_candidate_facility_near_weak_route = sum(strengthening_tier == "tier_2_candidate_facility_near_weak_route", na.rm = TRUE),
    monitor_facilities = sum(str_detect(strengthening_tier, "monitor"), na.rm = TRUE),
    missing_specialised_followup_clinic = sum(missing_specialised_followup_clinic, na.rm = TRUE),
    missing_hydroxyurea = sum(missing_hydroxyurea, na.rm = TRUE),
    missing_newborn_screening = sum(missing_newborn_screening, na.rm = TRUE),
    missing_transfusion = sum(missing_transfusion, na.rm = TRUE)
  )

results_numeric_checklist <- bind_rows(
  primary_summary %>%
    pivot_longer(
      cols = everything(),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(section = "primary_assignment"),

  hub_summary_checklist %>%
    pivot_longer(
      cols = everything(),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(section = "hub_workload_route_risk"),

  strengthening_checklist %>%
    pivot_longer(
      cols = everything(),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(section = "facility_strengthening")
) %>%
  select(
    section,
    metric,
    value
  )

# ----------------------------
# 15. Final table index
# ----------------------------

final_results_index <- tibble(
  table_id = c(
    "Table 1",
    "Table 2",
    "Table 3",
    "Table 4",
    "Table 5",
    "Table 6",
    "Supplementary Table S1",
    "Supplementary Table S2",
    "Supplementary Table S3",
    "Supplementary Table S4",
    "Supplementary Table S5",
    "Supplementary Table S6"
  ),
  table_name = c(
    "Input facility and readiness profile",
    "Scenario-level assignment and distance summary",
    "Primary route feasibility summary",
    "Hub workload and route-risk summary",
    "Facilities prioritised for SCD service strengthening",
    "Service gaps among strengthening facilities",
    "Accepted geocoded NBS spoke records",
    "Candidate SCD care hub layer",
    "Primary assigned spoke-to-hub routes",
    "Hub workload and route risk by assigned hub",
    "All facilities identified for strengthening or monitoring",
    "Scenario-level all-pair distance summary"
  ),
  proposed_location = c(
    "Main manuscript",
    "Main manuscript",
    "Main manuscript",
    "Main manuscript",
    "Main manuscript",
    "Main manuscript",
    "Supplementary file",
    "Supplementary file",
    "Supplementary file",
    "Supplementary file",
    "Supplementary file",
    "Supplementary file"
  ),
  source_file = c(
    basename(input_files$nbs_spokes),
    basename(input_files$assigned_summary_by_scenario),
    basename(input_files$assigned_primary_feasibility),
    basename(input_files$hub_workload_summary),
    basename(input_files$facilities_strengthening_summary),
    basename(input_files$strengthening_service_gap_summary),
    basename(input_files$nbs_spokes),
    basename(input_files$hub_layer),
    basename(input_files$assigned_primary),
    basename(input_files$hub_workload),
    basename(input_files$facilities_to_strengthen),
    basename(input_files$scenario_distance_summary)
  )
)

# ----------------------------
# 16. Save outputs
# ----------------------------

write_xlsx(
  list(
    results_numeric_checklist = results_numeric_checklist,
    table_1_input_profile = table_1_input_profile,
    table_2_scenario_summary = table_2_scenario_summary,
    table_3_primary_route_feasibility = table_3_primary_route_feasibility,
    table_4_hub_workload_summary = table_4_hub_workload_summary,
    table_4b_top_hubs_by_workload = table_4b_top_hubs_by_workload,
    table_5_strengthening_summary = table_5_strengthening_summary,
    table_5b_top_strengthening_facilities = table_5b_top_strengthening_facilities,
    table_6_service_gap_summary = table_6_service_gap_summary,
    table_6b_dominant_gap_summary = table_6b_dominant_gap_summary,
    table_6c_tier_summary = table_6c_tier_summary,
    final_results_index = final_results_index
  ),
  file.path(
    output_table_dir,
    "10_final_manuscript_results_tables.xlsx"
  )
)

write_xlsx(
  list(
    supp_table_s1_all_nbs_spokes = supp_table_s1_all_nbs_spokes,
    supp_table_s2_all_candidate_hubs = supp_table_s2_all_candidate_hubs,
    supp_table_s3_primary_assigned_routes = supp_table_s3_primary_assigned_routes,
    supp_table_s4_hub_workload = supp_table_s4_hub_workload,
    supp_table_s5_facilities_to_strengthen = supp_table_s5_facilities_to_strengthen,
    supp_table_s6_scenario_distance_summary = supp_table_s6_scenario_distance_summary
  ),
  file.path(
    output_table_dir,
    "10_final_supplementary_tables.xlsx"
  )
)

write_csv(
  final_results_index,
  file.path(
    output_table_dir,
    "10_final_results_index.csv"
  )
)

write_csv(
  results_numeric_checklist,
  file.path(
    output_table_dir,
    "10_results_numeric_checklist.csv"
  )
)

# ----------------------------
# 17. Print key checklist
# ----------------------------

print(results_numeric_checklist)
print(final_results_index)

# ----------------------------
# 18. Log
# ----------------------------

log_path <- file.path(
  admin_dir,
  "analysis_log_updated_spoke_layer.md"
)

log_entry <- c(
  "",
  paste0("## ", Sys.Date(), " - Created final manuscript and supplementary results tables"),
  "",
  paste0("- Final manuscript workbook: ",
         file.path(output_table_dir, "10_final_manuscript_results_tables.xlsx")),
  paste0("- Final supplementary workbook: ",
         file.path(output_table_dir, "10_final_supplementary_tables.xlsx")),
  paste0("- Numeric checklist: ",
         file.path(output_table_dir, "10_results_numeric_checklist.csv")),
  paste0("- Tables indexed: ", nrow(final_results_index), ".")
)

cat(
  paste(log_entry, collapse = "\n"),
  file = log_path,
  append = TRUE
)

message("Final results table export complete.")
message("Manuscript results workbook:")
message(file.path(output_table_dir, "10_final_manuscript_results_tables.xlsx"))
message("Supplementary tables workbook:")
message(file.path(output_table_dir, "10_final_supplementary_tables.xlsx"))
message("Numeric checklist:")
message(file.path(output_table_dir, "10_results_numeric_checklist.csv"))