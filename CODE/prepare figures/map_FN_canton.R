# ==============================================================================
# FN Vote Share Map — CANTON level (replaces commune-level map_FN.R for Fig. 4)
# ==============================================================================
# The commune-level map is unreadable (35k tiny polygons). This script
# aggregates FN vote shares to the canton level (population-weighted mean),
# dissolves commune polygons into canton polygons, and draws a two-panel
# (1988 vs 2002) map with a shared scale.
#
# Output: OUTPUT/figures/map_FN_canton.png
# The dissolved canton geometry is cached in DATA/processed data/canton_geom.rds
# so re-runs are fast.
# ==============================================================================

library(dplyr)
library(ggplot2)
library(sf)

main_path <- here::here()
if (!endsWith(main_path, "/")) main_path <- paste0(main_path, "/")
source(file.path(main_path, "CODE/configurations.R"))

sf_use_s2(FALSE)

geom_cache <- file.path(processed_data_path, "canton_geom.rds")
data_cache <- file.path(processed_data_path, "canton_fn.rds")

# ------------------------------------------------------------------------------
# 1. Canton-level FN shares (population-weighted mean of commune shares)
# ------------------------------------------------------------------------------
# Official commune -> canton crosswalk from the 1999 INSEE communes file
# (built by extracting DEP+COM -> DEP+CT from DATA/raw data/france1999.dbf;
# covers all ~36.7k communes incl. Paris/Lyon/Alsace-Moselle)
bridge <- read.csv(file.path(processed_data_path, "commune_canton_1999.csv"),
                   colClasses = "character")

if (!file.exists(data_cache)) {
  # Full national election panel (all ~33k communes), NOT the RDD sample —
  # this is a descriptive map and should cover the whole country.
  load(file.path(processed_data_path, "FN_growth.RData"))
  # codecommune lost leading zeros upstream (dep 01-09 have 4-char codes) and
  # Corsica is coded with the pre-1976 dep "20" instead of 2A/2B — normalize:
  corsica_2a <- bridge$codecommune[substr(bridge$codecommune, 1, 2) == "2A"]
  canton_fn <- dfZRRControls %>%
    mutate(
      codecommune = as.character(codecommune),
      codecommune = ifelse(grepl("^[0-9]{4}$", codecommune),
                           paste0("0", codecommune), codecommune),
      codecommune = ifelse(substr(codecommune, 1, 2) == "20",
                           ifelse(paste0("2A", substr(codecommune, 3, 5)) %in% corsica_2a,
                                  paste0("2A", substr(codecommune, 3, 5)),
                                  paste0("2B", substr(codecommune, 3, 5))),
                           codecommune)
    ) %>%
    filter(year %in% c(1988, 2002), !is.na(FN),
           !substr(codecommune, 1, 2) %in% c("97", "98")) %>%
    inner_join(bridge, by = "codecommune") %>%
    group_by(canton1999, year) %>%
    summarise(FN = weighted.mean(FN, w = pop, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = year, values_from = FN, names_prefix = "FN") %>%
    rename(canton = canton1999)
  saveRDS(canton_fn, data_cache)
  cat("Saved", data_cache, "\n")
} else {
  canton_fn <- readRDS(data_cache)
}
cat("Cantons with FN data:", nrow(canton_fn), "\n")

# ------------------------------------------------------------------------------
# 2. Canton geometry: dissolve commune polygons by canton (cached)
# ------------------------------------------------------------------------------
if (!file.exists(geom_cache)) {
  shp <- st_read(file.path(raw_data_path, "communes-20220101-shp/communes-20220101.shp"),
                 quiet = TRUE) %>%
    filter(!substr(insee, 1, 2) %in% c("97", "98")) %>%
    st_simplify(dTolerance = 0.002, preserveTopology = TRUE) %>%
    inner_join(bridge %>% rename(canton = canton1999), by = c("insee" = "codecommune"))

  cat("Communes matched to cantons:", nrow(shp), "\n")

  canton_geom <- shp %>%
    group_by(canton) %>%
    summarise(.groups = "drop")   # sf dissolves geometry on summarise

  saveRDS(canton_geom, geom_cache)
  cat("Saved", geom_cache, "\n")
} else {
  canton_geom <- readRDS(geom_cache)
}
cat("Canton polygons:", nrow(canton_geom), "\n")

# ------------------------------------------------------------------------------
# 3. Plot: two panels, shared scale
# ------------------------------------------------------------------------------
map_df <- canton_geom %>%
  inner_join(canton_fn %>% select(canton, FN1988, FN2002), by = "canton") %>%
  tidyr::pivot_longer(c(FN1988, FN2002), names_to = "year", values_to = "FN") %>%
  mutate(year = ifelse(year == "FN1988", "Panel A: 1988", "Panel B: 2002")) %>%
  filter(!is.na(FN))

p <- ggplot(map_df) +
  geom_sf(aes(fill = FN), color = "white", linewidth = 0.03) +
  facet_wrap(~year) +
  scale_fill_distiller(
    palette = "OrRd", direction = 1,
    limits = c(0, 0.35), oob = scales::squish,
    labels = scales::percent_format(accuracy = 1),
    name = "FN vote share",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  ) +
  theme_void(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 13, margin = margin(b = 6)),
    legend.position = "bottom",
    legend.key.width = unit(1.6, "cm"),
    legend.key.height = unit(0.35, "cm"),
    plot.margin = margin(4, 4, 4, 4)
  )

out <- file.path(path_figures, "map_FN_canton.png")
ggsave(out, p, width = 11, height = 6.2, dpi = 300, bg = "white")
cat("Saved", out, "\n")
