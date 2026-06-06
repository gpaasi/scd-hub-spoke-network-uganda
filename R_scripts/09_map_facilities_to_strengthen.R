# ============================================================
# 09_map_facilities_to_strengthen.R
# Map facilities prioritised for SCD service strengthening
# ============================================================

# Purpose:
# Create manuscript-ready maps of facilities identified for SCD
# service strengthening or monitoring.
#
# Inputs:
# - 02_processed_data/facilities/facilities_to_strengthen_scd_services.csv
# - 02_processed_data/spatial/scd_hub_spoke_network_outputs.gpkg
# - 01_raw_data/spatial/boundary_uganda_utm36S.gpkg
#
# Outputs:
# - 04_outputs/maps/map_06_facilities_to_strengthen.png
# - 04_outputs/maps/map_07_specialised_followup_clinic_gaps.png
# - 04_outputs/maps/map_08_dominant_service_gaps.png
# - 04_outputs/maps/map_09_strengthening_tiers.png
# - 04_outputs/tables/09_facilities_to_strengthen_map_summary.xlsx

# ----------------------------
# 1. Load packages
# ----------------------------

required_packages <- c(
  "sf",
  "dplyr",
  "readr",
  "stringr",
  "janitor",
  "ggplot2",
  "tibble",
  "writexl",
  "scales"
)

installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

library(sf)
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(ggplot2)
library(tibble)
library(writexl)
library(scales)

# ----------------------------
# 2. Define paths
# ----------------------------

project_root <- "scd_hub_spoke_model"

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

output_map_dir <- file.path(
  project_root,
  "04_outputs",
  "maps"
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
  output_map_dir,
  output_table_dir,
  log_dir,
  admin_dir
)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

strengthening_file <- file.path(
  processed_facility_dir,
  "facilities_to_strengthen_scd_services.csv"
)

network_gpkg <- file.path(
  processed_spatial_dir,
  "scd_hub_spoke_network_outputs.gpkg"
)

uganda_boundary_file <- file.path(
  project_root,
  "01_raw_data",
  "spatial",
  "boundary_uganda_utm36S.gpkg"
)

required_files <- c(
  strengthening_file,
  network_gpkg,
  uganda_boundary_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required file(s):\n",
      paste(missing_files, collapse = "\n"),
      "\n\nRun Scripts 06 and 08 first, and confirm the Uganda boundary GeoPackage path."
    )
  )
}

# Optional contextual layers
water_area_options <- c(
  file.path(project_root, "01_raw_data", "UGA_water_areas_dcw.shp"),
  file.path(project_root, "01_raw_data", "spatial", "UGA_water_areas_dcw.shp")
)

udhs_region_options <- c(
  file.path(project_root, "01_raw_data", "UDHS Regions 2019.shp"),
  file.path(project_root, "01_raw_data", "spatial", "UDHS Regions 2019.shp")
)

first_existing_file <- function(file_options, required = FALSE) {
  existing <- file_options[file.exists(file_options)]

  if (length(existing) == 0) {
    if (required) {
      stop(
        paste0(
          "Missing required file. Checked:\n",
          paste(file_options, collapse = "\n")
        )
      )
    }

    return(NA_character_)
  }

  existing[1]
}

water_area_file <- first_existing_file(
  water_area_options,
  required = FALSE
)

udhs_region_file <- first_existing_file(
  udhs_region_options,
  required = FALSE
)

message("Using strengthening file: ", strengthening_file)
message("Using network GeoPackage: ", network_gpkg)
message("Using Uganda boundary: ", uganda_boundary_file)
message("Using water areas: ", water_area_file)
message("Using UDHS regions: ", udhs_region_file)

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

read_first_gpkg_layer <- function(gpkg_path, label) {
  gpkg_layers <- sf::st_layers(gpkg_path)

  if (nrow(gpkg_layers) == 0) {
    stop(
      paste0(
        "No layers found in ",
        label,
        " GeoPackage:\n",
        gpkg_path
      )
    )
  }

  selected_layer <- gpkg_layers$name[1]

  message("Reading ", label, " layer from GeoPackage: ", selected_layer)

  sf::st_read(
    gpkg_path,
    layer = selected_layer,
    quiet = TRUE
  )
}

save_map <- function(plot_object, filename, width = 9, height = 8, dpi = 320) {
  ggsave(
    filename = file.path(output_map_dir, filename),
    plot = plot_object,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

# ----------------------------
# 4. Read data
# ----------------------------

facilities_to_strengthen <- read_csv(
  strengthening_file,
  show_col_types = FALSE
) %>%
  clean_names()

assigned_routes <- st_read(
  network_gpkg,
  layer = "assigned_routes_primary_lines",
  quiet = TRUE
)

assigned_spokes <- st_read(
  network_gpkg,
  layer = "assigned_spokes_primary_points",
  quiet = TRUE
)

uganda_boundary <- read_first_gpkg_layer(
  uganda_boundary_file,
  "Uganda boundary"
)

water_areas <- if (!is.na(water_area_file)) {
  st_read(water_area_file, quiet = TRUE)
} else {
  NULL
}

udhs_regions <- if (!is.na(udhs_region_file)) {
  st_read(udhs_region_file, quiet = TRUE)
} else {
  NULL
}

message("Facilities to strengthen read: ", nrow(facilities_to_strengthen))

# ----------------------------
# 5. Validate and standardise fields
# ----------------------------

required_cols <- c(
  "facility_id",
  "facility_name",
  "facility_level",
  "longitude",
  "latitude",
  "strengthening_tier",
  "priority_score",
  "source_role",
  "dominant_service_gap",
  "missing_specialised_followup_clinic",
  "missing_hydroxyurea",
  "missing_newborn_screening",
  "missing_transfusion"
)

missing_cols <- setdiff(
  required_cols,
  names(facilities_to_strengthen)
)

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Strengthening file is missing required columns:\n",
      paste(missing_cols, collapse = "\n"),
      "\n\nColumns found:\n",
      paste(names(facilities_to_strengthen), collapse = ", ")
    )
  )
}

facilities_to_strengthen <- facilities_to_strengthen %>%
  mutate(
    longitude = safe_num_col(cur_data(), c("longitude")),
    latitude = safe_num_col(cur_data(), c("latitude")),
    priority_score = safe_num_col(cur_data(), c("priority_score")),

    ownership = safe_chr_col(cur_data(), c("ownership")),
    subregion = safe_chr_col(cur_data(), c("subregion")),

    strengthening_tier = as.character(strengthening_tier),
    source_role = as.character(source_role),
    dominant_service_gap = as.character(dominant_service_gap),

    missing_specialised_followup_clinic = safe_lgl_col(
      cur_data(),
      c("missing_specialised_followup_clinic")
    ),

    missing_hydroxyurea = safe_lgl_col(
      cur_data(),
      c("missing_hydroxyurea")
    ),

    missing_newborn_screening = safe_lgl_col(
      cur_data(),
      c("missing_newborn_screening")
    ),

    missing_transfusion = safe_lgl_col(
      cur_data(),
      c("missing_transfusion")
    ),

    coordinate_valid = valid_uganda_coordinates(
      longitude,
      latitude
    )
  )

facilities_invalid_coordinates <- facilities_to_strengthen %>%
  filter(!coordinate_valid)

if (nrow(facilities_invalid_coordinates) > 0) {
  warning(
    paste0(
      nrow(facilities_invalid_coordinates),
      " strengthening facilities have invalid coordinates and will not be mapped."
    )
  )
}

facilities_sf <- facilities_to_strengthen %>%
  filter(coordinate_valid) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# ----------------------------
# 6. Standardise CRS
# ----------------------------

target_crs <- 4326

uganda_boundary <- st_transform(uganda_boundary, target_crs)
assigned_routes <- st_transform(assigned_routes, target_crs)
assigned_spokes <- st_transform(assigned_spokes, target_crs)
facilities_sf <- st_transform(facilities_sf, target_crs)

if (!is.null(water_areas)) {
  water_areas <- st_transform(water_areas, target_crs)
}

if (!is.null(udhs_regions)) {
  udhs_regions <- st_transform(udhs_regions, target_crs)
}

uganda_bbox <- st_bbox(uganda_boundary)

if (!is.null(water_areas)) {
  water_areas <- suppressWarnings(st_crop(water_areas, uganda_bbox))
}

if (!is.null(udhs_regions)) {
  udhs_regions <- suppressWarnings(st_crop(udhs_regions, uganda_bbox))
}

# ----------------------------
# 7. Base map theme and context layers
# ----------------------------

map_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(linewidth = 0.15, colour = "grey85"),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

add_context_layers <- function() {
  layers <- list(
    geom_sf(
      data = uganda_boundary,
      fill = "grey98",
      colour = "grey35",
      linewidth = 0.35
    )
  )

  if (!is.null(water_areas)) {
    layers <- c(
      layers,
      list(
        geom_sf(
          data = water_areas,
          fill = "lightblue",
          colour = NA,
          alpha = 0.45
        )
      )
    )
  }

  if (!is.null(udhs_regions)) {
    layers <- c(
      layers,
      list(
        geom_sf(
          data = udhs_regions,
          fill = NA,
          colour = "grey45",
          linewidth = 0.25,
          linetype = "dotted"
        )
      )
    )
  }

  layers
}

# ----------------------------
# 8. Prepare filtered layers
# ----------------------------

weak_routes <- assigned_routes %>%
  mutate(
    weak_or_long_route = safe_lgl_col(cur_data(), c("weak_or_long_route"))
  ) %>%
  filter(weak_or_long_route)

tier1_facilities <- facilities_sf %>%
  filter(
    strengthening_tier %in% c(
      "tier_1_current_hub_strengthening",
      "tier_1b_current_hub_review"
    )
  )

tier2_facilities <- facilities_sf %>%
  filter(
    str_detect(
      strengthening_tier,
      "tier_2"
    )
  )

followup_gap_facilities <- facilities_sf %>%
  filter(
    missing_specialised_followup_clinic
  )

# ----------------------------
# 9. Map 6: all facilities to strengthen
# ----------------------------

map_06_facilities_to_strengthen <- ggplot() +
  add_context_layers() +
  geom_sf(
    data = assigned_routes,
    colour = "grey78",
    linewidth = 0.18,
    alpha = 0.45
  ) +
  geom_sf(
    data = weak_routes,
    colour = "grey35",
    linewidth = 0.5,
    alpha = 0.8
  ) +
  geom_sf(
    data = assigned_spokes,
    shape = 3,
    size = 1.1,
    alpha = 0.5
  ) +
  geom_sf(
    data = facilities_sf,
    aes(
      shape = strengthening_tier,
      size = priority_score
    ),
    fill = "white",
    colour = "black",
    stroke = 0.55,
    alpha = 0.95
  ) +
  scale_size_continuous(
    name = "Priority score",
    range = c(1.8, 6.5),
    breaks = pretty_breaks(n = 5)
  ) +
  labs(
    title = "Facilities prioritised for SCD service strengthening",
    subtitle = "Priority reflects current hub role, weak-route exposure, workload, and service-readiness gaps",
    shape = "Strengthening tier"
  ) +
  coord_sf(
    xlim = c(uganda_bbox["xmin"], uganda_bbox["xmax"]),
    ylim = c(uganda_bbox["ymin"], uganda_bbox["ymax"]),
    expand = FALSE
  ) +
  map_theme

save_map(
  map_06_facilities_to_strengthen,
  "map_06_facilities_to_strengthen.png"
)

# ----------------------------
# 10. Map 7: specialised follow-up clinic gaps
# ----------------------------

map_07_followup_gaps <- ggplot() +
  add_context_layers() +
  geom_sf(
    data = assigned_routes,
    colour = "grey80",
    linewidth = 0.18,
    alpha = 0.4
  ) +
  geom_sf(
    data = assigned_spokes,
    shape = 3,
    size = 1.0,
    alpha = 0.45
  ) +
  geom_sf(
    data = followup_gap_facilities,
    aes(
      shape = source_role,
      size = priority_score
    ),
    fill = "white",
    colour = "black",
    stroke = 0.65,
    alpha = 0.95
  ) +
  scale_size_continuous(
    name = "Priority score",
    range = c(2.2, 7),
    breaks = pretty_breaks(n = 4)
  ) +
  labs(
    title = "Specialised SCD clinic follow-up gaps",
    subtitle = "Facilities shown lack specialised follow-up clinic capacity and were identified for strengthening or validation",
    shape = "Facility role"
  ) +
  coord_sf(
    xlim = c(uganda_bbox["xmin"], uganda_bbox["xmax"]),
    ylim = c(uganda_bbox["ymin"], uganda_bbox["ymax"]),
    expand = FALSE
  ) +
  map_theme

save_map(
  map_07_followup_gaps,
  "map_07_specialised_followup_clinic_gaps.png"
)

# ----------------------------
# 11. Map 8: dominant service gaps
# ----------------------------

map_08_dominant_gaps <- ggplot() +
  add_context_layers() +
  geom_sf(
    data = assigned_routes,
    colour = "grey82",
    linewidth = 0.16,
    alpha = 0.4
  ) +
  geom_sf(
    data = facilities_sf,
    aes(
      shape = dominant_service_gap,
      size = priority_score
    ),
    fill = "white",
    colour = "black",
    stroke = 0.55,
    alpha = 0.95
  ) +
  scale_size_continuous(
    name = "Priority score",
    range = c(1.8, 6.5),
    breaks = pretty_breaks(n = 5)
  ) +
  labs(
    title = "Dominant service gaps among strengthening facilities",
    subtitle = "Dominant gap prioritises specialised follow-up, hydroxyurea, transfusion, and newborn screening gaps",
    shape = "Dominant gap"
  ) +
  coord_sf(
    xlim = c(uganda_bbox["xmin"], uganda_bbox["xmax"]),
    ylim = c(uganda_bbox["ymin"], uganda_bbox["ymax"]),
    expand = FALSE
  ) +
  map_theme

save_map(
  map_08_dominant_gaps,
  "map_08_dominant_service_gaps.png"
)

# ----------------------------
# 12. Map 9: Tier 1 versus Tier 2 facilities
# ----------------------------

map_09_strengthening_tiers <- ggplot() +
  add_context_layers() +
  geom_sf(
    data = assigned_routes,
    colour = "grey82",
    linewidth = 0.16,
    alpha = 0.4
  ) +
  geom_sf(
    data = tier2_facilities,
    aes(size = priority_score),
    shape = 22,
    fill = "white",
    colour = "black",
    stroke = 0.5,
    alpha = 0.9
  ) +
  geom_sf(
    data = tier1_facilities,
    aes(size = priority_score),
    shape = 24,
    fill = "white",
    colour = "black",
    stroke = 0.75,
    alpha = 1
  ) +
  scale_size_continuous(
    name = "Priority score",
    range = c(2, 7),
    breaks = pretty_breaks(n = 5)
  ) +
  labs(
    title = "Tiered SCD service strengthening facilities",
    subtitle = "Triangles show current assigned hubs; squares show nearby candidate upgrade or validation facilities"
  ) +
  coord_sf(
    xlim = c(uganda_bbox["xmin"], uganda_bbox["xmax"]),
    ylim = c(uganda_bbox["ymin"], uganda_bbox["ymax"]),
    expand = FALSE
  ) +
  map_theme

save_map(
  map_09_strengthening_tiers,
  "map_09_strengthening_tiers.png"
)

# ----------------------------
# 13. Save mapped facility GeoPackage
# ----------------------------

strengthening_gpkg <- file.path(
  processed_spatial_dir,
  "facilities_to_strengthen_scd_services.gpkg"
)

if (file.exists(strengthening_gpkg)) {
  file.remove(strengthening_gpkg)
}

st_write(
  facilities_sf,
  strengthening_gpkg,
  layer = "facilities_to_strengthen_scd_services",
  delete_layer = TRUE,
  quiet = TRUE
)

st_write(
  tier1_facilities,
  strengthening_gpkg,
  layer = "tier1_current_hub_strengthening",
  append = TRUE,
  quiet = TRUE
)

st_write(
  tier2_facilities,
  strengthening_gpkg,
  layer = "tier2_candidate_facilities",
  append = TRUE,
  quiet = TRUE
)

st_write(
  followup_gap_facilities,
  strengthening_gpkg,
  layer = "specialised_followup_clinic_gaps",
  append = TRUE,
  quiet = TRUE
)

# ----------------------------
# 14. Save summaries
# ----------------------------

map_outputs_summary <- tibble(
  map_id = c(
    "Map 6",
    "Map 7",
    "Map 8",
    "Map 9"
  ),
  file_name = c(
    "map_06_facilities_to_strengthen.png",
    "map_07_specialised_followup_clinic_gaps.png",
    "map_08_dominant_service_gaps.png",
    "map_09_strengthening_tiers.png"
  ),
  description = c(
    "All facilities prioritised for strengthening or monitoring",
    "Facilities missing specialised SCD clinic follow-up capacity",
    "Dominant service gaps among strengthening facilities",
    "Tier 1 current hubs versus Tier 2 candidate upgrade facilities"
  )
)

map_qc <- tibble(
  metric = c(
    "facilities_to_strengthen_total",
    "facilities_mapped",
    "facilities_not_mapped_invalid_coordinates",
    "tier1_facilities_mapped",
    "tier2_facilities_mapped",
    "specialised_followup_gap_facilities_mapped",
    "assigned_route_lines_context",
    "weak_or_long_routes_context",
    "assigned_spokes_context"
  ),
  value = c(
    nrow(facilities_to_strengthen),
    nrow(facilities_sf),
    nrow(facilities_invalid_coordinates),
    nrow(tier1_facilities),
    nrow(tier2_facilities),
    nrow(followup_gap_facilities),
    nrow(assigned_routes),
    nrow(weak_routes),
    nrow(assigned_spokes)
  )
)

write_csv(
  map_outputs_summary,
  file.path(
    output_map_dir,
    "facilities_to_strengthen_map_outputs_summary.csv"
  )
)

write_csv(
  map_qc,
  file.path(
    log_dir,
    "09_facilities_to_strengthen_map_qc.csv"
  )
)

write_xlsx(
  list(
    map_outputs_summary = map_outputs_summary,
    map_qc = map_qc
  ),
  file.path(
    output_table_dir,
    "09_facilities_to_strengthen_map_summary.xlsx"
  )
)

print(map_outputs_summary)
print(map_qc)

# ----------------------------
# 15. Log
# ----------------------------

log_path <- file.path(
  admin_dir,
  "analysis_log_updated_spoke_layer.md"
)

log_entry <- c(
  "",
  paste0("## ", Sys.Date(), " - Mapped SCD facilities for strengthening"),
  "",
  paste0("- Facilities to strengthen or monitor: ", nrow(facilities_to_strengthen), "."),
  paste0("- Facilities mapped: ", nrow(facilities_sf), "."),
  paste0("- Tier 1 facilities mapped: ", nrow(tier1_facilities), "."),
  paste0("- Tier 2 facilities mapped: ", nrow(tier2_facilities), "."),
  paste0("- Specialised follow-up clinic gap facilities mapped: ", nrow(followup_gap_facilities), "."),
  paste0("- Strengthening GeoPackage: ", strengthening_gpkg),
  paste0("- Output map directory: ", output_map_dir)
)

cat(
  paste(log_entry, collapse = "\n"),
  file = log_path,
  append = TRUE
)

message("Facility strengthening maps complete.")
message("Maps saved in:")
message(output_map_dir)
message("Strengthening GeoPackage:")
message(strengthening_gpkg)