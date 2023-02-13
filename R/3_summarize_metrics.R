# README
# compile separate metrics into a final set for use in scenario analyses and R
# package; plot each set of metrics and example metrics for presentation
# purposes

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')
key = readxl::read_excel('GIS/VEG_key.xlsx')

# standardize formatting
label.order = key %>% select(CODE_NAME, LABEL) %>% distinct() %>%
  filter(!CODE_NAME %in% c(
    'PERENNIAL_CROPS', 'ANNUAL_CROPS', 'GRAIN&HAY', 'FIELD',
    'GRASSLAND&PASTURE', 'WOODLAND&SCRUB', 'WATER', 'WETLAND',
    'WETLAND_MANAGED_PERENNIAL', 'WETLAND_MANAGED_SEASONAL')) %>%
  filter(!grepl('RIPARIAN_', CODE_NAME) & !is.na(CODE_NAME)) %>%
  pull(LABEL) %>% unique()
label.order = label.order[c(1:9, 11:13, 10, 14:22)]

# COMPILE ALL METRICS----------

waterquality = read_csv('data/pesticide_exposure.csv', col_types = cols()) %>%
  filter(METRIC %in%
           c('Risk to Aquatic Organisms', 'Critical Pesticides',
             'Groundwater Contaminant'))

economy = bind_rows(
  read_csv('data/crop_production_value.csv', col_types = cols()),
  read_csv('data/livelihoods.csv', col_types = cols()))

ccs = read_csv('data/climate_change_resilience.csv', col_types = cols()) %>%
  filter(METRIC %in% c('Heat', 'Drought', 'Flood'))


metrics = bind_rows(economy, waterquality, ccs) %>%
  left_join(key %>% select(LABEL, CODE_NAME), by = 'CODE_NAME') %>%
  mutate(
    CODE_NAME = factor(
      CODE_NAME,
      levels = key$CODE_NAME %>% na.omit()),
    METRIC_CATEGORY = recode(
      METRIC_CATEGORY,
      economy = 'Agricultural Livelihoods',
      `water quality` = 'Water Quality',
      climate = 'Climate Change Resilience',
      biodiversity = 'Biodiversity Support'),
    UNIT = recode(
      UNIT,
      'number of employees per ha' = 'FTE/ha/yr',
      'annual dollars per employee' = 'USD/FTE',
      'USD per ha per year' = 'USD/ha/yr',
      'kg per ha' = 'kg/ha/yr',
      'ranking' = 'qualitative score, 1-10')) %>%
  arrange(METRIC_CATEGORY, METRIC, CODE_NAME) %>%
  select(METRIC_CATEGORY, METRIC, UNIT, CODE_NAME, LABEL, SCORE_MEAN, SCORE_SE)
write_csv(metrics, 'output/metrics.csv')

metrics_meta = metrics %>% select(METRIC_CATEGORY, METRIC, UNIT) %>%
  distinct()
write_csv(metrics_meta, 'output/metrics_metadata.csv')

# SUMMARY TABLE---------
# for supplemental info in manuscript
metrics = read_csv('output/metrics.csv')

metrics_format = metrics %>%
  filter(LABEL %in% label.order) %>%
  mutate(LABEL = factor(LABEL, levels = label.order)) %>%
  arrange(LABEL) %>%
  # for the readability of this table, convert Ag Jobs units to FTE/100ha,
  # Annual wages to USD thousands
  mutate(across(c(SCORE_MEAN, SCORE_SE),
                ~case_when(METRIC == 'Agricultural Jobs' ~ .x*100,
                           METRIC == 'Annual Wages' ~ .x/1000,
                           METRIC == 'Gross Production Value' ~ .x/1000,
                           TRUE ~ .x)))

metrics_table = metrics_format %>%
  mutate(SCORE_MEAN = paste0("'", round(SCORE_MEAN, digits = 2) %>%
                               format(nsmall = 2)),
         SCORE_SE = paste0("'(", round(SCORE_SE, digits = 2) %>%
                             format(nsmall = 2), ')')) %>%
  select(LABEL, METRIC_CATEGORY, METRIC, SCORE_MEAN, SCORE_SE) %>%
  pivot_wider(names_from = c(METRIC_CATEGORY, METRIC),
              values_from = c(SCORE_MEAN, SCORE_SE)) %>%
  select(LABEL,
         contains('Jobs'), contains('Wages'), contains('Gross'),
         contains('Critical'), contains('Groundwater'), contains('Aquatic'),
         contains('Drought'), contains('Flood'), contains('Heat'))
write_csv(metrics_table, 'output/TABLE_metrics_summary.csv')



# PLOTS BY METRIC--------
library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')
showtext_auto()
showtext_opts(dpi = 300)

## ag livelihoods------
metrics %>%
  filter(METRIC_CATEGORY=='Agricultural Livelihoods' &
           LABEL %in% label.order) %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order))) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE_MEAN, SCORE_SE) %>%
  # for plotting purposes, convert to ag jobs/100 ha, wages and production value
  # in thousands
  mutate(across(c(SCORE_MEAN, SCORE_SE),
                ~case_when(METRIC == 'Agricultural Jobs' ~ .x*100,
                           TRUE ~ .x/1000))) %>%
  ggplot(aes(SCORE_MEAN, LABEL)) +
  geom_col(fill = pointblue.palette[4]) +
  geom_errorbar(aes(xmin = SCORE_MEAN - SCORE_SE,
                    xmax = SCORE_MEAN + SCORE_SE),
                width = 0.5) +
  facet_wrap(
    ~METRIC, ncol = 3, scales = 'free_x',
    labeller = labeller(
      METRIC = c(
        `Agricultural Jobs` = 'Agricultural Jobs\n(FTE / 100 ha)',
        `Annual Wages` = 'Annual Wages\n(USD, thousands)',
        `Gross Production Value` = 'Gross Crop Production Value\n(USD / ha, thousands)'))) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(family = 'sourcesans', size = 10),
        axis.title = element_text(family = 'sourcesans', size = 12),
        strip.text = element_text(family = 'sourcesans', size = 10,
                                  hjust = 0, vjust = 1),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/metrics_aglivelihoods.png', height = 4, width = 8)

## water quality-----
metrics %>%
  filter(METRIC_CATEGORY == 'Water Quality' & LABEL %in% label.order) %>%
  mutate(LABEL = factor(LABEL, levels = rev(label.order))) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE_MEAN, SCORE_SE) %>%
  ggplot(aes(SCORE_MEAN, LABEL)) + geom_col(fill = pointblue.palette[3]) +
  geom_errorbar(aes(xmin = SCORE_MEAN - SCORE_SE,
                    xmax = SCORE_MEAN + SCORE_SE),
                width = 0.5) +
  facet_wrap(~METRIC, scale = 'free_x') +
  # scale_fill_gradient(low = palette[5], high = palette[7]) +
  labs(x = 'Pesticide application rate (kg/ha/yr)', y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(family = 'sourcesans', size = 10),
        axis.title = element_text(family = 'sourcesans', size = 11),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/metrics_waterquality.png', height = 4, width = 8)

## climate change resilience---------
metrics %>%
  filter(METRIC_CATEGORY=='Climate Change Resilience' &
           LABEL %in% label.order) %>%
  mutate(
    LABEL = factor(LABEL, levels = rev(label.order)),
    METRIC = factor(METRIC,
                    levels = c('Drought', 'Flood', 'Heat'))) %>%
  select(LABEL, CODE_NAME, METRIC, SCORE_MEAN, SCORE_SE) %>%
  ggplot(aes(SCORE_MEAN, LABEL)) + geom_col(fill = 'seagreen') +
  geom_errorbar(aes(xmin = SCORE_MEAN - SCORE_SE,
                    xmax = SCORE_MEAN + SCORE_SE),
                width = 0.5) +
  facet_wrap(~METRIC, ncol = 3, scale = 'free_x') +
  labs(x = 'Climate change resilience score', y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(family = 'sourcesans', size = 9),
        axis.text.y = element_text(family = 'sourcesans', size = 10),
        axis.title = element_text(family = 'sourcesans', size = 11),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        strip.background = element_blank(),
        legend.position = 'none')
ggsave('fig/metrics_climate.png', height = 4, width = 8)

## vineyard example--------
# just for presentation purposes -- one of each metric for vineyards
metrics_example = read_csv('output/metrics.csv') %>%
  filter(LABEL == 'Vineyard') %>%
  mutate(
    #rescale to match units from netchange
    across(c(SCORE_MEAN, SCORE_SE),
           ~case_when(
             METRIC == 'Agricultural Jobs' ~ .*1000, #per ha to per 1000 ha
             METRIC == 'Annual Wages' ~ ./1000,
             METRIC == 'Gross Production Value' ~ ./1000,
             TRUE ~ .)),
    METRIC = factor(
      METRIC,
      levels = c("Agricultural Jobs", "Annual Wages", "Gross Production Value",
                 "Critical Pesticides", "Groundwater Contaminant",
                 "Risk to Aquatic Organisms", "Drought", "Flood", "Heat") %>%
        rev()),
    # add units to ag livelihood metric labels
    METRIC = recode(
      METRIC,
      'Agricultural Jobs' = 'Agricultural Jobs\n(FTE per 1000 ha)',
      'Annual Wages' = 'Annual Wages\n(USD per FTE, thousands)',
      'Gross Production Value' = 'Gross Production Value\n(USD per ha, thousands)',
      'Critical Pesticides' = 'Critical\nPesticides',
      'Groundwater Contaminant' = 'Groundwater\nContaminants',
      'Risk to Aquatic Organisms' = 'Risk to Aquatic\nOrganisms'))

# ag livelihoods
a = metrics_example %>%
  filter(METRIC_CATEGORY == 'Agricultural Livelihoods') %>%
  ggplot(aes(SCORE_MEAN, METRIC)) +
  geom_col(fill = pointblue.palette[4], na.rm = TRUE) +
  geom_errorbar(aes(xmin = SCORE_MEAN - SCORE_SE, xmax = SCORE_MEAN + SCORE_SE),
                width = 0.5) +
  labs(x = '', y = NULL, title = 'Agricultural Livelihoods') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 42)) +
  theme_bw() +
  theme(axis.text = element_text(family = 'sourcesans', size = 16),
        axis.title = element_text(family = 'sourcesans', size = 16),
        strip.text = element_text(family = 'sourcesans', size = 18, hjust = 0),
        plot.title = element_text(family = 'sourcesans', size = 18),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none')

# pesticides
b = metrics_example %>%
  filter(METRIC_CATEGORY == 'Water Quality') %>%
  ggplot(aes(SCORE_MEAN, METRIC)) +
  geom_col(fill = pointblue.palette[3]) +
  geom_errorbar(aes(xmin = SCORE_MEAN - SCORE_SE, xmax = SCORE_MEAN + SCORE_SE),
                width = 0.5) +
  labs(x = 'Pesticide application rate (kg/ha/yr)', y = NULL,
       title = 'Water Quality') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 3)) +
  theme_bw() +
  theme(axis.text = element_text(family = 'sourcesans', size = 16),
        axis.title = element_text(family = 'sourcesans', size = 16),
        strip.text = element_text(family = 'sourcesans', size = 18, hjust = 0),
        strip.background = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 18),
        panel.grid = element_blank(),
        legend.position = 'none')

# climate change resilience
c = metrics_example %>%
  filter(METRIC_CATEGORY == 'Climate Change Resilience') %>%
  ggplot(aes(SCORE_MEAN, METRIC)) +
  geom_col(fill = 'seagreen', na.rm = TRUE) +
  geom_errorbar(aes(xmin = SCORE_MEAN - SCORE_SE, xmax = SCORE_MEAN + SCORE_SE), width = 0.5) +
  labs(x = 'Mean score (1-10 scale)', y = NULL, title = 'Climate change resilience') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 10), breaks = seq(0, 10, 2)) +
  theme_bw() +
  theme(axis.text = element_text(family = 'sourcesans', size = 16),
        axis.title = element_text(family = 'sourcesans', size = 16),
        strip.text = element_text(family = 'sourcesans', size = 18, hjust = 0),
        strip.background = element_blank(),
        plot.title = element_text(family = 'sourcesans', size = 18),
        panel.grid = element_blank(),
        legend.position = 'none')

library(patchwork)
showtext_auto()
showtext_opts(dpi = 300) #default for ggsave
a + b + c + plot_layout(nrow = 2, byrow = FALSE)
ggsave('fig/presentation_metrics_overview.png',
       height = 7.5, width = 10, units = 'in')
showtext_auto(FALSE)


# PLOT CHANGE MAPS---------
# Note: these plots are slow to produce!

spp_key = read_csv('output/TABLE_species_key.csv')

delta_shp = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(crs = st_crs(32610))

water = terra::rast('GIS/scenario_rasters/baseline.tif') %>%
  mask(rast('GIS/boundaries/delta.tif')) %>%
  classify(rcl = data.frame(from = 90, to = 0) %>% as.matrix(),
           others = NA)
plot(water, col = 'blue')

library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')

library(tidyterra)

## riparian
# --> should be able to use purrr for multiple scenarios, but was getting errors
# with new device issues with ggsave

plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'riparian',
                landscape_name = 'scenario1_restoration',
                key = spp_key,
                ncol = 5,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_riparian_scenario1.png',
                height = 7, width = 10,
                legend.position = c(1, 0),
                legend.justification = c(1, 0))

plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'riparian',
                landscape_name = 'scenario2_perennialexpand',
                key = spp_key,
                ncol = 5,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_riparian_scenario2.png',
                height = 7, width = 10,
                legend.position = c(1, 0),
                legend.justification = c(1, 0))

plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'riparian',
                landscape_name = 'scenario3_combo',
                key = spp_key,
                ncol = 5,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_riparian_scenario3.png',
                height = 7, width = 10,
                legend.position = c(1, 0),
                legend.justification = c(1, 0))

## waterbird_fall
plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'waterbird_fall',
                landscape_name = 'scenario1_restoration',
                key = spp_key,
                ncol = 3,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_waterbird_fall_scenario1.png',
                height = 7, width = 7.5)

plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'waterbird_fall',
                landscape_name = 'scenario2_perennialexpand',
                key = spp_key,
                ncol = 3,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_waterbird_fall_scenario2.png',
                height = 7, width = 7.5)

plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'waterbird_fall',
                landscape_name = 'scenario3_combo',
                key = spp_key,
                ncol = 3,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_waterbird_fall_scenario3.png',
                height = 7, width = 7.5)

## waterbird_winter
plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'waterbird_win',
                landscape_name = 'scenario1_restoration_win',
                key = spp_key,
                ncol = 3,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_waterbird_win_scenario1.png',
                height = 7, width = 7.5)

plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'waterbird_win',
                landscape_name = 'scenario2_perennialexpand_win',
                key = spp_key,
                ncol = 3,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_waterbird_win_scenario2.png',
                height = 7, width = 7.5)

plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'waterbird_win',
                landscape_name = 'scenario3_combo_win',
                key = spp_key,
                ncol = 3,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_waterbird_win_scenario3.png',
                height = 7, width = 7.5)

## alt-------
plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'riparian',
                landscape_name = 'scenario2_perennialexpand_alt',
                key = spp_key,
                ncol = 5,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_riparian_scenario2_alt.png',
                height = 7, width = 10,
                legend.position = c(1, 0),
                legend.justification = c(1, 0))

plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'riparian',
                landscape_name = 'scenario3_combo_alt',
                key = spp_key,
                ncol = 5,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_riparian_scenario3_alt.png',
                height = 7, width = 10,
                legend.position = c(1, 0),
                legend.justification = c(1, 0))

## waterbird_fall
plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'waterbird_fall',
                landscape_name = 'scenario2_perennialexpand_alt',
                key = spp_key,
                ncol = 3,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_waterbird_fall_scenario2_alt.png',
                height = 7, width = 7.5)

plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'waterbird_fall',
                landscape_name = 'scenario3_combo_alt',
                key = spp_key,
                ncol = 3,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_waterbird_fall_scenario3_alt.png',
                height = 7, width = 7.5)

## waterbird_win
plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'waterbird_win',
                landscape_name = 'scenario2_perennialexpand_alt_win',
                key = spp_key,
                ncol = 3,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_waterbird_win_scenario2_alt.png',
                height = 7, width = 7.5)

plot_change_map(pathin = 'GIS/SDM_results_diff',
                SDM = 'waterbird_win',
                landscape_name = 'scenario3_combo_alt_win',
                key = spp_key,
                ncol = 3,
                studyarea = delta_shp,
                watermask = water,
                pathout = 'fig/changemap_waterbird_win_scenario3_alt.png',
                height = 7, width = 7.5)
showtext_auto(F)
