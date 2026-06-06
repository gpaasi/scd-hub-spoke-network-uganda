# ============================================================
# 07_map_updated_hub_spoke_network.R
# Map updated SCD hub-and-spoke network
# ============================================================

# Purpose:
# Create manuscript-ready maps for the updated 168-spoke SCD
# hub-and-spoke network.
#
# Inputs:
# - 02_processed_data/spatial/scd_hub_spoke_network_outputs.gpkg
# - Uganda boundary GeoPackage
# - Uganda water areas shapefile
# - UDHS 2019 subregion shapefile, optional
#
# Outputs:
# - 04_outputs/maps/map_01_primary_hub_spoke_network.png
# - 04_outputs/maps/map_02_route_feasibility.png
# - 04_outputs/maps/map_03_hub_workload.png
# - 04_outputs/maps/map_04_weak_or_long_routes.png
# - 04_outputs/maps/map_05_hub_readiness_and_spokes.png
# - 04_outputs/tables/07_map_outputs_summary.xlsx

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

network_gpkg <- file.path(
  processed_spatial_dir,
  "scd_hub_spoke_network_outputs.gpkg"
)

if (!file.exists(network_gpkg)) {
  stop(
    paste0(
      "Missing network GeoPackage:\n",
      network_gpkg,
      "\n\nRun Script 06 first."
    )
  )
}

# ----------------------------
# 3. Reference layer paths
# ----------------------------

# Uganda boundary is stored as a GeoPackage in the spatial raw-data folder.
# Optional water and UDHS region layers are read when present.

uganda_boundary_options <- c(
  file.path(
    project_root,
    "01_raw_data",
    "spatial",
    "boundary_uganda_utm36S.gpkg"
  ),
  file.path(
    project_root,
    "01_raw_data",
    "spatial",
    "boundary_uganda_utm36s.gpkg"
  )
)

water_area_options <- c(
  file.path(
    project_root,
    "01_raw_data",
    "UGA_water_areas_dcw.shp"
  ),
  file.path(
    project_root,
    "01_raw_data",
    "spatial",
    "UGA_water_areas_dcw.shp"
  )
)

udhs_region_options <- c(
  file.path(
    project_root,
    "01_raw_data",
    "UDHS Regions 2019.shp"
  ),
  file.path(
    project_root,
    "01_raw_data",
    "spatial",
    "UDHS Regions 2019.shp"
  )
)

first_existing_file <- function(file_options, file_label, required = TRUE) {
  existing <- file_options[file.exists(file_options)]

  if (length(existing) == 0) {
    if (required) {
      stop(
        paste0(
          "Missing required file for ",
          file_label,
          ". Checked:\n",
          paste(file_options, collapse = "\n")
        )
      )
    }

    return(NA_character_)
  }

  existing[1]
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

uganda_boundary_file <- first_existing_file(
  uganda_boundary_options,
  "Uganda boundary GeoPackage",
  required = TRUE
)

water_area_file <- first_existing_file(
  water_area_options,
  "water areas",
  required = FALSE
)

udhs_region_file <- first_existing_file(
  udhs_region_options,
  "UDHS 2019 regions",
  required = FALSE
)

message("Using Uganda boundary file: ", uganda_boundary_file)
message("Using water area file: ", water_area_file)
message("Using UDHS region file: ", udhs_region_file)

# ----------------------------
# 4. Read network layers
# ----------------------------

assigned_spokes <- st_read(
  network_gpkg,
  layer = "assigned_spokes_primary_points",
  quiet = TRUE
)

assigned_hubs <- st_read(
  network_gpkg,
  layer = "assigned_hubs_primary_points",
  quiet = TRUE
)

hub_workload <- st_read(
  network_gpkg,
  layer = "hub_workload_primary_points",
  quiet = TRUE
)

assigned_routes <- st_read(
  network_gpkg,
  layer = "assigned_routes_primary_lines",
  quiet = TRUE
)

# ----------------------------
# 5. Read contextual layers
# ----------------------------

uganda_boundary <- read_first_gpkg_layer(
  uganda_boundary_file,
  "Uganda boundary"
)

water_areas <- if (!is.na(water_area_file)) {
  st_read(
    water_area_file,
    quiet = TRUE
  )
} else {
  NULL
}

udhs_regions <- if (!is.na(udhs_region_file)) {
  st_read(
    udhs_region_file,
    quiet = TRUE
  )
} else {
  NULL
}

# ----------------------------
# 6. Standardise CRS
# ----------------------------

target_crs <- 4326

uganda_boundary <- st_transform(
  uganda_boundary,
  target_crs
)

assigned_spokes <- st_transform(
  assigned_spokes,
  target_crs
)

assigned_hubs <- st_transform(
  assigned_hubs,
  target_crs
)

hub_workload <- st_transform(
  hub_workload,
  target_crs
)

assigned_routes <- st_transform(
  assigned_routes,
  target_crs
)

if (!is.null(water_areas)) {
  water_areas <- st_transform(
    water_areas,
    target_crs
  )
}

if (!is.null(udhs_regions)) {
  udhs_regions <- st_transform(
    udhs_regions,
    target_crs
  )
}

# Crop reference layers to Uganda boundary extent where possible.
uganda_bbox <- st_bbox(uganda_boundary)

if (!is.null(water_areas)) {
  water_areas <- suppressWarnings(
    st_crop(water_areas, uganda_bbox)
  )
}

if (!is.null(udhs_regions)) {
  udhs_regions <- suppressWarnings(
    st_crop(udhs_regions, uganda_bbox)
  )
}

# ----------------------------
# 7. Standardise map fields
# ----------------------------

assigned_routes <- assigned_routes %>%
  mutate(
    route_feasibility = as.character(route_feasibility),
    route_feasibility_group = as.character(route_feasibility_group),
    weak_or_long_route = as.logical(weak_or_long_route),
    assigned_distance_km = suppressWarnings(as.numeric(assigned_distance_km)),
    hub_service_score = suppressWarnings(as.integer(hub_service_score))
  )

assigned_spokes <- assigned_spokes %>%
  mutate(
    spoke_type = as.character(spoke_type),
    assigned_distance_km = suppressWarnings(as.numeric(assigned_distance_km)),
    route_feasibility = as.character(route_feasibility),
    weak_or_long_route = as.logical(weak_or_long_route)
  )

assigned_hubs <- assigned_hubs %>%
  mutate(
    assigned_hub_level = as.character(assigned_hub_level),
    hub_service_score = suppressWarnings(as.integer(hub_service_score)),
    hub_readiness = as.character(hub_readiness)
  )

hub_workload <- hub_workload %>%
  mutate(
    linked_spokes = suppressWarnings(as.numeric(linked_spokes)),
    route_risk_category = as.character(route_risk_category),
    workload_category = as.character(workload_category),
    hub_priority_flag = as.character(hub_priority_flag),
    hub_service_score = suppressWarnings(as.integer(hub_service_score))
  )

# ----------------------------
# 8. Base map theme and helper
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
# 9. Map 1: primary network overview
# ----------------------------

map_01_primary_network <- ggplot() +
  add_context_layers() +
  geom_sf(
    data = assigned_routes,
    aes(linetype = route_feasibility_group),
    colour = "grey45",
    linewidth = 0.28,
    alpha = 0.65
  ) +
  geom_sf(
    data = assigned_spokes,
    aes(shape = spoke_type),
    size = 1.8,
    alpha = 0.9
  ) +
  geom_sf(
    data = hub_workload,
    aes(size = linked_spokes),
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 0.45,
    alpha = 0.95
  ) +
  scale_size_continuous(
    name = "Linked spokes",
    range = c(2, 7),
    breaks = pretty_breaks(n = 4)
  ) +
  labs(
    title = "Primary SCD hub-and-spoke network",
    subtitle = "Accepted geocoded NBS spokes linked to nearest eligible SCD care hubs",
    shape = "Spoke type",
    linetype = "Route group"
  ) +
  coord_sf(
    xlim = c(29, 36),
    ylim = c(-2, 5),
    expand = FALSE
  ) +
  map_theme

save_map(
  map_01_primary_network,
  "map_01_primary_hub_spoke_network.png"
)

# ----------------------------
# 10. Map 2: route feasibility
# ----------------------------

map_02_route_feasibility <- ggplot() +
  add_context_layers() +
  geom_sf(
    data = assigned_routes,
    aes(linetype = route_feasibility, linewidth = weak_or_long_route),
    colour = "grey30",
    alpha = 0.72
  ) +
  geom_sf(
    data = assigned_spokes,
    aes(shape = route_feasibility),
    size = 1.7,
    alpha = 0.9
  ) +
  geom_sf(
    data = hub_workload,
    aes(size = linked_spokes),
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 0.45
  ) +
  scale_linewidth_manual(
    values = c(
      "FALSE" = 0.25,
      "TRUE" = 0.65
    ),
    name = "Weak or long route"
  ) +
  scale_size_continuous(
    name = "Linked spokes",
    range = c(2, 7),
    breaks = pretty_breaks(n = 4)
  ) +
  labs(
    title = "Route feasibility in the assigned SCD referral network",
    subtitle = "Route classes combine distance and hub service-readiness",
    shape = "Route feasibility",
    linetype = "Route feasibility"
  ) +
  coord_sf(
    xlim = c(29, 36),
    ylim = c(-2, 5),
    expand = FALSE
  ) +
  map_theme

save_map(
  map_02_route_feasibility,
  "map_02_route_feasibility.png"
)

# ----------------------------
# 11. Map 3: hub workload
# ----------------------------

map_03_hub_workload <- ggplot() +
  add_context_layers() +
  geom_sf(
    data = assigned_routes,
    colour = "grey70",
    linewidth = 0.22,
    alpha = 0.55
  ) +
  geom_sf(
    data = assigned_spokes,
    shape = 3,
    size = 1.2,
    alpha = 0.65
  ) +
  geom_sf(
    data = hub_workload,
    aes(size = linked_spokes, shape = workload_category),
    fill = "white",
    colour = "black",
    stroke = 0.55,
    alpha = 0.95
  ) +
  scale_size_continuous(
    name = "Linked spokes",
    range = c(2.5, 8),
    breaks = pretty_breaks(n = 5)
  ) +
  labs(
    title = "Hub workload in the primary SCD care network",
    subtitle = "Hub symbol size reflects number of linked NBS spokes",
    shape = "Workload category"
  ) +
  coord_sf(
    xlim = c(29, 36),
    ylim = c(-2, 5),
    expand = FALSE
  ) +
  map_theme

save_map(
  map_03_hub_workload,
  "map_03_hub_workload.png"
)

# ----------------------------
# 12. Map 4: weak or long routes
# ----------------------------

weak_routes <- assigned_routes %>%
  filter(weak_or_long_route)

weak_route_spokes <- assigned_spokes %>%
  filter(weak_or_long_route)

weak_route_hubs <- hub_workload %>%
  filter(route_risk_category %in% c("moderate_route_risk", "high_route_risk"))

map_04_weak_or_long_routes <- ggplot() +
  add_context_layers() +
  geom_sf(
    data = assigned_routes,
    colour = "grey82",
    linewidth = 0.18,
    alpha = 0.45
  ) +
  geom_sf(
    data = weak_routes,
    aes(linetype = route_feasibility),
    colour = "black",
    linewidth = 0.65,
    alpha = 0.85
  ) +
  geom_sf(
    data = assigned_spokes,
    shape = 3,
    size = 1,
    alpha = 0.45
  ) +
  geom_sf(
    data = weak_route_spokes,
    aes(shape = route_feasibility),
    size = 2,
    alpha = 0.95
  ) +
  geom_sf(
    data = hub_workload,
    shape = 21,
    size = 2.2,
    fill = "white",
    colour = "grey40",
    stroke = 0.35,
    alpha = 0.8
  ) +
  geom_sf(
    data = weak_route_hubs,
    aes(size = linked_spokes),
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 0.75,
    alpha = 1
  ) +
  scale_size_continuous(
    name = "Linked spokes",
    range = c(3, 8),
    breaks = pretty_breaks(n = 4)
  ) +
  labs(
    title = "Weak or long assigned routes",
    subtitle = "Highlighted pathways indicate partial readiness, longer distance, or both",
    shape = "Route feasibility",
    linetype = "Route feasibility"
  ) +
  coord_sf(
    xlim = c(29, 36),
    ylim = c(-2, 5),
    expand = FALSE
  ) +
  map_theme

save_map(
  map_04_weak_or_long_routes,
  "map_04_weak_or_long_routes.png"
)

# ----------------------------
# 13. Map 5: hub readiness and linked spokes
# ----------------------------

map_05_hub_readiness <- ggplot() +
  add_context_layers() +
  geom_sf(
    data = assigned_routes,
    colour = "grey75",
    linewidth = 0.2,
    alpha = 0.5
  ) +
  geom_sf(
    data = assigned_spokes,
    aes(shape = spoke_type),
    size = 1.5,
    alpha = 0.75
  ) +
  geom_sf(
    data = hub_workload,
    aes(size = linked_spokes, shape = hub_readiness),
    fill = "white",
    colour = "black",
    stroke = 0.55,
    alpha = 0.95
  ) +
  scale_size_continuous(
    name = "Linked spokes",
    range = c(2.5, 8),
    breaks = pretty_breaks(n = 4)
  ) +
  labs(
    title = "Hub readiness among assigned SCD care hubs",
    subtitle = "Hub symbols show readiness category and linked-spoke workload",
    shape = "Facility category"
  ) +
  coord_sf(
    xlim = c(29, 36),
    ylim = c(-2, 5),
    expand = FALSE
  ) +
  map_theme

save_map(
  map_05_hub_readiness,
  "map_05_hub_readiness_and_spokes.png"
)

# ----------------------------
# 14. Save map summaries
# ----------------------------

map_outputs_summary <- tibble(
  map_id = c(
    "Figure map 1",
    "Figure map 2",
    "Figure map 3",
    "Figure map 4",
    "Figure map 5"
  ),
  file_name = c(
    "map_01_primary_hub_spoke_network.png",
    "map_02_route_feasibility.png",
    "map_03_hub_workload.png",
    "map_04_weak_or_long_routes.png",
    "map_05_hub_readiness_and_spokes.png"
  ),
  description = c(
    "Overview of primary assigned SCD hub-and-spoke network",
    "Assigned routes classified by feasibility",
    "Hub workload map based on linked NBS spokes",
    "Weak or long route map for priority pathway interpretation",
    "Assigned hub readiness and linked-spoke workload"
  )
)

map_qc <- tibble(
  metric = c(
    "assigned_spokes_mapped",
    "assigned_hubs_mapped",
    "hub_workload_points_mapped",
    "assigned_routes_mapped",
    "weak_or_long_routes_mapped",
    "uganda_boundary_features",
    "water_area_features",
    "udhs_region_features"
  ),
  value = c(
    nrow(assigned_spokes),
    nrow(assigned_hubs),
    nrow(hub_workload),
    nrow(assigned_routes),
    nrow(weak_routes),
    nrow(uganda_boundary),
    if (!is.null(water_areas)) nrow(water_areas) else 0,
    if (!is.null(udhs_regions)) nrow(udhs_regions) else 0
  )
)

write_csv(
  map_outputs_summary,
  file.path(
    output_map_dir,
    "map_outputs_summary.csv"
  )
)

write_csv(
  map_qc,
  file.path(
    log_dir,
    "07_map_outputs_qc.csv"
  )
)

write_xlsx(
  list(
    map_outputs_summary = map_outputs_summary,
    map_qc = map_qc
  ),
  file.path(
    output_table_dir,
    "07_map_outputs_summary.xlsx"
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
  paste0("## ", Sys.Date(), " - Created updated hub-and-spoke network maps"),
  "",
  paste0("- Assigned spokes mapped: ", nrow(assigned_spokes), "."),
  paste0("- Assigned hubs mapped: ", nrow(assigned_hubs), "."),
  paste0("- Assigned route lines mapped: ", nrow(assigned_routes), "."),
  paste0("- Weak or long routes mapped: ", nrow(weak_routes), "."),
  paste0("- Output map directory: ", output_map_dir)
)

cat(
  paste(log_entry, collapse = "\n"),
  file = log_path,
  append = TRUE
)

message("Map generation complete.")
message("Maps saved in:")
message(output_map_dir)