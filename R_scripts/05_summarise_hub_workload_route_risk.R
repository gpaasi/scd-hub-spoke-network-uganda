# ============================================================
# 05_summarise_hub_workload_route_risk.R
# Summarise hub workload and route risk under primary scenario
# ============================================================

# Purpose:
# Summarise hub-level workload and route risk from the primary
# nearest-hub assignment.
#
# Input:
# - 02_processed_data/routes/assigned_spoke_hub_routes_primary.csv
#
# Outputs:
# - 02_processed_data/routes/hub_workload_route_risk_primary.csv
# - 02_processed_data/routes/hub_workload_route_risk_summary.csv
# - 04_outputs/tables/05_hub_workload_route_risk_summary.xlsx

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

assigned_primary_file <- file.path(
  processed_route_dir,
  "assigned_spoke_hub_routes_primary.csv"
)

if (!file.exists(assigned_primary_file)) {
  stop(
    paste0(
      "Missing primary assigned routes file:\n",
      assigned_primary_file,
      "\n\nRun Script 04 first."
    )
  )
}

# ----------------------------
# 3. Helper functions
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

workload_category <- function(linked_spokes) {
  case_when(
    is.na(linked_spokes) ~ "unknown",
    linked_spokes < 5 ~ "low_spoke_load",
    linked_spokes >= 5 & linked_spokes < 10 ~ "moderate_spoke_load",
    linked_spokes >= 10 & linked_spokes < 20 ~ "high_spoke_load",
    linked_spokes >= 20 ~ "very_high_spoke_load",
    TRUE ~ "unknown"
  )
}

route_risk_category <- function(
  pct_weak_or_long_routes,
  max_assigned_distance_km,
  hub_service_score
) {
  case_when(
    is.na(pct_weak_or_long_routes) ~ "unknown",

    hub_service_score <= 2 & pct_weak_or_long_routes >= 50 ~
      "high_route_risk",

    max_assigned_distance_km > 50 & pct_weak_or_long_routes >= 50 ~
      "high_route_risk",

    pct_weak_or_long_routes >= 50 ~
      "moderate_route_risk",

    max_assigned_distance_km > 50 ~
      "moderate_route_risk",

    TRUE ~
      "low_route_risk"
  )
}

priority_flag <- function(
  workload_category,
  route_risk_category,
  hub_service_score
) {
  case_when(
    route_risk_category == "high_route_risk" & hub_service_score <= 2 ~
      "partial_service_hub_with_route_risk",

    route_risk_category == "high_route_risk" ~
      "high_route_risk_hub",

    workload_category %in% c("high_spoke_load", "very_high_spoke_load") ~
      "high_workload_hub",

    workload_category == "moderate_spoke_load" & hub_service_score <= 2 ~
      "moderate_workload_partial_hub",

    TRUE ~
      "standard_network_hub"
  )
}

# ----------------------------
# 4. Read primary assignment
# ----------------------------

assigned_primary <- read_csv(
  assigned_primary_file,
  show_col_types = FALSE
) %>%
  clean_names()

message("Rows read from primary assignment: ", nrow(assigned_primary))
message("Columns found:")
print(names(assigned_primary))

# ----------------------------
# 5. Validate columns
# ----------------------------

required_cols <- c(
  "spoke_id",
  "spoke_name",
  "spoke_type",
  "existing_nbs",
  "collocated_candidate_nbs",
  "hub_id",
  "assigned_hub_name",
  "assigned_hub_level",
  "assigned_hub_ownership",
  "assigned_hub_subregion",
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
  "hub_service_deficit",
  "hub_readiness"
)

missing_cols <- setdiff(
  required_cols,
  names(assigned_primary)
)

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Primary assignment file is missing required columns:\n",
      paste(missing_cols, collapse = "\n"),
      "\n\nColumns found:\n",
      paste(names(assigned_primary), collapse = ", ")
    )
  )
}

# ----------------------------
# 6. Standardise values
# ----------------------------

assigned_primary <- assigned_primary %>%
  mutate(
    assigned_distance_km = suppressWarnings(as.numeric(assigned_distance_km)),
    existing_nbs = suppressWarnings(as.integer(existing_nbs)),
    collocated_candidate_nbs = suppressWarnings(as.integer(collocated_candidate_nbs)),
    weak_or_long_route = as.logical(weak_or_long_route),
    long_route = as.logical(long_route),
    very_long_route = as.logical(very_long_route),
    cl_binary = suppressWarnings(as.integer(cl_binary)),
    hu_binary = suppressWarnings(as.integer(hu_binary)),
    nbs_binary = suppressWarnings(as.integer(nbs_binary)),
    tr_binary = suppressWarnings(as.integer(tr_binary)),
    hub_service_score = suppressWarnings(as.integer(hub_service_score)),
    hub_service_deficit = suppressWarnings(as.numeric(hub_service_deficit))
  )

if (nrow(assigned_primary) == 0) {
  stop("Primary assignment file has zero rows.")
}

if (any(is.na(assigned_primary$assigned_distance_km))) {
  stop("Some assigned primary routes have missing assigned_distance_km.")
}

# ----------------------------
# 7. Hub workload and route-risk summary
# ----------------------------

hub_workload <- assigned_primary %>%
  group_by(
    hub_id,
    assigned_hub_name,
    assigned_hub_level,
    assigned_hub_ownership,
    assigned_hub_subregion,
    cl_binary,
    hu_binary,
    nbs_binary,
    tr_binary,
    hub_service_score,
    hub_service_deficit,
    hub_readiness
  ) %>%
  summarise(
    linked_spokes = n_distinct(spoke_id),
    linked_existing_nbs_spokes = sum(existing_nbs == 1, na.rm = TRUE),
    linked_collocated_candidate_spokes = sum(collocated_candidate_nbs == 1, na.rm = TRUE),

    min_assigned_distance_km = round(safe_min(assigned_distance_km), 1),
    median_assigned_distance_km = round(safe_median(assigned_distance_km), 1),
    mean_assigned_distance_km = round(safe_mean(assigned_distance_km), 1),
    max_assigned_distance_km = round(safe_max(assigned_distance_km), 1),

    functional_routes = sum(route_feasibility == "functional", na.rm = TRUE),
    partial_routes = sum(route_feasibility == "partial", na.rm = TRUE),
    moderate_functional_routes = sum(route_feasibility == "moderate_functional", na.rm = TRUE),
    moderate_partial_routes = sum(route_feasibility == "moderate_partial", na.rm = TRUE),
    long_routes = sum(route_feasibility == "long", na.rm = TRUE),
    very_long_routes = sum(route_feasibility == "very_long", na.rm = TRUE),

    weak_or_long_routes = sum(weak_or_long_route, na.rm = TRUE),
    pct_weak_or_long_routes = round(
      100 * weak_or_long_routes / linked_spokes,
      1
    ),

    .groups = "drop"
  ) %>%
  mutate(
    workload_category = workload_category(linked_spokes),

    route_risk_category = route_risk_category(
      pct_weak_or_long_routes,
      max_assigned_distance_km,
      hub_service_score
    ),

    hub_priority_flag = priority_flag(
      workload_category,
      route_risk_category,
      hub_service_score
    ),

    missing_cl = cl_binary == 0,
    missing_hu = hu_binary == 0,
    missing_nbs = nbs_binary == 0,
    missing_tr = tr_binary == 0,

    missing_service_domains = paste(
      c(
        ifelse(missing_cl, "CL", NA_character_),
        ifelse(missing_hu, "HU", NA_character_),
        ifelse(missing_nbs, "NBS", NA_character_),
        ifelse(missing_tr, "TR", NA_character_)
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
    )
  ) %>%
  arrange(
    desc(route_risk_category == "high_route_risk"),
    desc(linked_spokes),
    desc(pct_weak_or_long_routes),
    assigned_hub_name
  )

# ----------------------------
# 8. Network-level summaries
# ----------------------------

hub_workload_summary <- tibble(
  metric = c(
    "primary_assigned_spokes",
    "hubs_used_in_primary_assignment",
    "median_linked_spokes_per_hub",
    "mean_linked_spokes_per_hub",
    "max_linked_spokes_assigned_to_one_hub",
    "low_spoke_load_hubs",
    "moderate_spoke_load_hubs",
    "high_spoke_load_hubs",
    "very_high_spoke_load_hubs",
    "low_route_risk_hubs",
    "moderate_route_risk_hubs",
    "high_route_risk_hubs",
    "full_hubs_used_score_4",
    "strong_hubs_used_score_3",
    "partial_hubs_used_score_2",
    "spokes_linked_to_full_hubs",
    "spokes_linked_to_strong_hubs",
    "spokes_linked_to_partial_hubs",
    "hubs_flagged_partial_service_with_route_risk"
  ),
  value = c(
    n_distinct(assigned_primary$spoke_id),
    n_distinct(assigned_primary$hub_id),
    round(safe_median(hub_workload$linked_spokes), 1),
    round(safe_mean(hub_workload$linked_spokes), 1),
    safe_max(hub_workload$linked_spokes),
    sum(hub_workload$workload_category == "low_spoke_load", na.rm = TRUE),
    sum(hub_workload$workload_category == "moderate_spoke_load", na.rm = TRUE),
    sum(hub_workload$workload_category == "high_spoke_load", na.rm = TRUE),
    sum(hub_workload$workload_category == "very_high_spoke_load", na.rm = TRUE),
    sum(hub_workload$route_risk_category == "low_route_risk", na.rm = TRUE),
    sum(hub_workload$route_risk_category == "moderate_route_risk", na.rm = TRUE),
    sum(hub_workload$route_risk_category == "high_route_risk", na.rm = TRUE),
    sum(hub_workload$hub_service_score == 4, na.rm = TRUE),
    sum(hub_workload$hub_service_score == 3, na.rm = TRUE),
    sum(hub_workload$hub_service_score == 2, na.rm = TRUE),
    sum(assigned_primary$hub_service_score == 4, na.rm = TRUE),
    sum(assigned_primary$hub_service_score == 3, na.rm = TRUE),
    sum(assigned_primary$hub_service_score == 2, na.rm = TRUE),
    sum(hub_workload$hub_priority_flag == "partial_service_hub_with_route_risk", na.rm = TRUE)
  )
)

hub_workload_category_summary <- hub_workload %>%
  count(
    workload_category,
    name = "n_hubs"
  ) %>%
  mutate(
    pct_hubs = round(100 * n_hubs / sum(n_hubs), 1)
  )

hub_route_risk_summary <- hub_workload %>%
  count(
    route_risk_category,
    hub_priority_flag,
    name = "n_hubs"
  ) %>%
  mutate(
    pct_hubs = round(100 * n_hubs / sum(n_hubs), 1)
  ) %>%
  arrange(
    route_risk_category,
    hub_priority_flag
  )

hub_readiness_workload_summary <- hub_workload %>%
  group_by(
    hub_readiness,
    hub_service_score
  ) %>%
  summarise(
    hubs = n(),
    linked_spokes = sum(linked_spokes, na.rm = TRUE),
    median_linked_spokes = round(safe_median(linked_spokes), 1),
    max_linked_spokes = safe_max(linked_spokes),
    weak_or_long_routes = sum(weak_or_long_routes, na.rm = TRUE),
    pct_weak_or_long_routes = round(
      100 * weak_or_long_routes / linked_spokes,
      1
    ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(hub_service_score)
  )

top_hub_workload <- hub_workload %>%
  arrange(
    desc(linked_spokes),
    desc(pct_weak_or_long_routes),
    assigned_hub_name
  ) %>%
  slice_head(n = 20)

high_route_risk_hubs <- hub_workload %>%
  filter(
    route_risk_category == "high_route_risk"
  ) %>%
  arrange(
    desc(pct_weak_or_long_routes),
    desc(linked_spokes),
    assigned_hub_name
  )

print(hub_workload_summary)
print(hub_workload_category_summary)
print(hub_route_risk_summary)
print(hub_readiness_workload_summary)
print(top_hub_workload)
print(high_route_risk_hubs)

# ----------------------------
# 9. Save outputs
# ----------------------------

write_csv(
  hub_workload,
  file.path(
    processed_route_dir,
    "hub_workload_route_risk_primary.csv"
  )
)

write_csv(
  hub_workload_summary,
  file.path(
    processed_route_dir,
    "hub_workload_route_risk_summary.csv"
  )
)

write_csv(
  hub_workload_category_summary,
  file.path(
    processed_route_dir,
    "hub_workload_category_summary.csv"
  )
)

write_csv(
  hub_route_risk_summary,
  file.path(
    processed_route_dir,
    "hub_route_risk_summary.csv"
  )
)

write_csv(
  hub_readiness_workload_summary,
  file.path(
    processed_route_dir,
    "hub_readiness_workload_summary.csv"
  )
)

write_csv(
  top_hub_workload,
  file.path(
    processed_route_dir,
    "top_hub_workload_primary.csv"
  )
)

write_csv(
  high_route_risk_hubs,
  file.path(
    processed_route_dir,
    "high_route_risk_hubs_primary.csv"
  )
)

write_xlsx(
  list(
    hub_workload_summary = hub_workload_summary,
    hub_workload = hub_workload,
    hub_workload_category_summary = hub_workload_category_summary,
    hub_route_risk_summary = hub_route_risk_summary,
    hub_readiness_workload_summary = hub_readiness_workload_summary,
    top_hub_workload = top_hub_workload,
    high_route_risk_hubs = high_route_risk_hubs
  ),
  file.path(
    output_table_dir,
    "05_hub_workload_route_risk_summary.xlsx"
  )
)

# ----------------------------
# 10. Log
# ----------------------------

log_path <- file.path(
  admin_dir,
  "analysis_log_updated_spoke_layer.md"
)

log_entry <- c(
  "",
  paste0("## ", Sys.Date(), " - Summarised hub workload and route risk"),
  "",
  paste0("- Primary assigned spokes: ", n_distinct(assigned_primary$spoke_id), "."),
  paste0("- Hubs used in primary assignment: ", n_distinct(assigned_primary$hub_id), "."),
  paste0("- Median linked spokes per hub: ",
         round(safe_median(hub_workload$linked_spokes), 1), "."),
  paste0("- Maximum linked spokes assigned to one hub: ",
         safe_max(hub_workload$linked_spokes), "."),
  paste0("- High route-risk hubs: ",
         sum(hub_workload$route_risk_category == "high_route_risk", na.rm = TRUE), "."),
  paste0("- Partial-service hubs with route risk: ",
         sum(hub_workload$hub_priority_flag == "partial_service_hub_with_route_risk", na.rm = TRUE), ".")
)

cat(
  paste(log_entry, collapse = "\n"),
  file = log_path,
  append = TRUE
)

message("Hub workload and route-risk summary complete.")
message("Hub workload output:")
message(file.path(processed_route_dir, "hub_workload_route_risk_primary.csv"))
message("Summary workbook:")
message(file.path(output_table_dir, "05_hub_workload_route_risk_summary.xlsx"))