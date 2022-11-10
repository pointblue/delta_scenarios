# ANALYZE NET CHANGE IN MULTIPLE METRICS---------
# Calculate the total "landscape score" for each metric and landscape, and then
# the net change between each scenario and the baseline

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')
key = readxl::read_excel('GIS/VEG_key.xlsx')
# spp_key = read_csv('output/TABLE_species_key.csv')

# REFERENCE DATA
# delta = rast('GIS/boundaries/delta.tif')
# county_raster = rast('GIS/landscape_rasters/boundaries/counties.tif')

# SPATIAL DATA-----------
# total suitable habitat for each landscape, predicted from SDMs

## probability of presence:
habitat = DeltaMultipleBenefits::sum_habitat(
  pathin = 'GIS/prediction_rasters',
  subtype = 'distributions',
  keypath = 'output/TABLE_species_key.csv')
write_csv(habitat, 'output/scenario_habitat.csv')

habitat_county = DeltaMultipleBenefits::sum_habitat(
  pathin = 'GIS/prediction_rasters',
  zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
  subtype = 'distributions',
  keypath = 'output/TABLE_species_key.csv')
write_csv(habitat_county, 'output/scenario_habitat_county.csv')

## binary presence/absence (using thresholds):
habitat_binary = DeltaMultipleBenefits::sum_habitat(
  pathin = 'GIS/prediction_rasters_threshold',
  subtype = 'habitat',
  keypath = 'output/TABLE_species_key.csv')
write_csv(habitat_binary, 'output/scenario_habitat_binary.csv')

habitat_binary_county = DeltaMultipleBenefits::sum_habitat(
  pathin = 'GIS/prediction_rasters_threshold',
  zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
  subtype = 'habitat',
  keypath = 'output/TABLE_species_key.csv')
write_csv(habitat_binary_county, 'output/scenario_habitat_binary_county.csv')


# OTHER METRICS----------

## compile landcover scores----------
waterquality = read_csv('data/pesticide_exposure.csv', col_types = cols()) %>%
  filter(METRIC %in%
           c('Risk to Aquatic Organisms', 'Critical Pesticides',
             'Groundwater Contaminant'))
economy = bind_rows(
  read_csv('data/crop_production_value.csv', col_types = cols()),
  read_csv('data/livelihoods.csv', col_types = cols()))
ccs = read_csv('data/climate_change_resilience.csv', col_types = cols())
# acs = read_csv('data/avian_conservation_score.csv', col_types = cols())

metrics = bind_rows(economy, waterquality, ccs) %>%
  left_join(key %>% select(LABEL, CODE_NAME), by = 'CODE_NAME') %>%
  mutate(CODE_NAME = factor(CODE_NAME,
                            levels = key$CODE_NAME %>% na.omit()),
         METRIC_CATEGORY = factor(METRIC_CATEGORY,
                                  levels = c('economy', 'water quality',
                                             'climate', 'biodiversity')),
         METRIC_CATEGORY = recode(METRIC_CATEGORY,
                                  economy = 'Agricultural Livelihoods',
                                  `water quality` = 'Water Quality',
                                  climate = 'Climate Change Resilience',
                                  biodiversity = 'Biodiversity Support'),
         METRIC = gsub('_', ': ', METRIC)) %>%
  mutate_at(vars(METRIC_SUBTYPE:METRIC), factor) %>%
  arrange(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, CODE_NAME)
write_csv(metrics, 'output/metrics_final.csv')

## land cover totals------------
totals = sum_landcover(pathin = 'GIS/scenario_rasters',
                       maskpath = 'GIS/boundaries/delta.tif',
                       pixel_area = 0.09, rollup = TRUE) %>%
  arrange(scenario, CODE_NAME)
write_csv(totals, 'output/land_cover_totals.csv')

totals_county = sum_landcover(pathin = 'GIS/scenario_rasters',
                              zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
                              maskpath = 'GIS/boundaries/delta.tif',
                              pixel_area = 0.09, rollup = TRUE) %>%
  arrange(scenario, ZONE, CODE_NAME)
write_csv(totals_county, 'output/land_cover_totals_county.csv')

## sum total-----------
# combine the landscape-specific estimates of the total area of each land cover
# class with the per-unit-area metrics for each land cover class
# - for most metrics, this is the sum over all hectares
# - for Annual Wages, calculate the new weighted average wage across all
#    agricultural ha (i.e., those with a wage value)

# exclude riparian and managed wetland subclasses (to not double-count), and
# exclude tidal wetlands and water since not addressing them in these scenarios
# (and they shouldn't change)
# --> exclude winter scenarios/landscapes
# --> combine with habitat estimates from above
scores = sum_metrics(
  metricdat = metrics %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))),
  areadat = totals %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))) %>%
    filter(!grepl('win', scenario))) %>%
  bind_rows(habitat, habitat_binary) %>%
  filter(!grepl('win', scenario)) %>% #remove from habitat metrics
  arrange(scenario, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC)
write_csv(scores, 'output/scenario_scores.csv')

scores_county = sum_metrics(
  metricdat = metrics %>% filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))),
  areadat = totals_county %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))) %>%
    filter(!grepl('win', scenario))) %>%
  bind_rows(habitat_county, habitat_binary_county) %>%
  filter(!grepl('win', scenario)) %>% #remove from habitat metrics
  arrange(scenario, ZONE, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC)
write_csv(scores_county, 'output/scenario_scores_county.csv')

# NET CHANGE------
# compare each scenario to baseline

# LANDCOVER
# for landcover, have to align field names to match fields expected in sum_change()
totals_change = totals %>%
  filter(!grepl('win', scenario)) %>%
  rename(SCORE_TOTAL = area) %>%
  mutate(SCORE_TOTAL_SE = 0) %>%
  sum_change() %>%
  select(-ends_with('SE', ignore.case = TRUE)) %>%
  left_join(key %>% select(CODE_NAME, LABEL), by = 'CODE_NAME') %>%
  select(scenario, CODE_NAME, LABEL, SCENARIO, BASELINE, net_change:change_pct)
write_csv(totals_change, 'output/land_cover_change.csv')

totals_change_county = totals_county %>%
  filter(!grepl('win', scenario)) %>%
  rename(SCORE_TOTAL = area) %>%
  mutate(SCORE_TOTAL_SE = 0) %>%
  sum_change() %>%
  select(-ends_with('SE', ignore.case = TRUE)) %>%
  left_join(key %>% select(CODE_NAME, LABEL), by = 'CODE_NAME') %>%
  select(scenario, ZONE, CODE_NAME, LABEL, SCENARIO, BASELINE, net_change:change_pct)
write_csv(totals_change_county, 'output/land_cover_change_county.csv')

# METRICS & HABITAT
net_change = sum_change(scores)
write_csv(net_change, 'output/scenario_change.csv')

net_change_county = sum_change(scores_county)
write_csv(net_change_county, 'output/scenario_change_county.csv')


# CALCULATE CHANGE MAPS--------
# for species distribution models only

## RIPARIAN-----
change1_rip = purrr::map(
  names(preds$riparian$baseline),
  ~diff(c(preds$riparian$baseline[[.x]],
          preds$riparian$scenario1_restoration[[.x]]))) %>%
  setNames(names(preds$riparian$baseline)) %>%
  rast()
writeRaster(change1_rip,
            file.path('GIS/change_rasters/riparian/scenario1_restoration',
                      paste0(names(change1), '.tif')),
            overwrite = TRUE)

change2_rip = purrr::map(
  names(preds$riparian$baseline),
  ~diff(c(preds$riparian$baseline[[.x]],
          preds$riparian$scenario2_perennialexpand[[.x]]))) %>%
  setNames(names(preds$riparian$baseline)) %>%
  rast()
writeRaster(change2_rip,
            file.path('GIS/change_rasters/riparian/scenario2_perennialexpand',
                      paste0(names(change2), '.tif')),
            overwrite = TRUE)

## WATERBIRDS FALL---------
change1_fall = purrr::map(
  names(preds$waterbird_fall$baseline),
  ~diff(c(preds$waterbird_fall$baseline[[.x]],
          preds$waterbird_fall$scenario1_restoration[[.x]]))) %>%
  setNames(names(preds$waterbird_fall$baseline)) %>%
  rast()
writeRaster(change1_fall,
            file.path('GIS/change_rasters/waterbird_fall/scenario1_restoration',
                      paste0(names(change1_fall), '.tif')),
            overwrite = TRUE)

change2_fall = purrr::map(
  names(preds$waterbird_fall$baseline),
  ~diff(c(preds$waterbird_fall$baseline[[.x]],
          preds$waterbird_fall$scenario2_perennialexpand[[.x]]))) %>%
  setNames(names(preds$waterbird_fall$baseline)) %>%
  rast()
writeRaster(change2_fall,
            file.path('GIS/change_rasters/waterbird_fall/scenario2_perennialexpand',
                      paste0(names(change1_fall), '.tif')),
            overwrite = TRUE)


## WATERBIRDS WINTER-------
change1_win = purrr::map(
  names(preds$waterbird_win$baseline),
  ~diff(c(preds$waterbird_win$baseline[[.x]],
          preds$waterbird_win$scenario1_restoration[[.x]]))) %>%
  setNames(names(preds$waterbird_win$baseline)) %>%
  rast()
writeRaster(change1_win,
            file.path('GIS/change_rasters/waterbird_win/scenario1_restoration',
                      paste0(names(change1_win), '.tif')),
            overwrite = TRUE)

change2_win = purrr::map(
  names(preds$waterbird_win$baseline),
  ~diff(c(preds$waterbird_win$baseline[[.x]],
          preds$waterbird_win$scenario2_perennialexpand[[.x]]))) %>%
  setNames(names(preds$waterbird_win$baseline)) %>%
  rast()
writeRaster(change2_win,
            file.path('GIS/change_rasters/waterbird_win/scenario2_perennialexpand',
                      paste0(names(change2_win), '.tif')),
            overwrite = TRUE)
