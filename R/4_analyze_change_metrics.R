# ANALYZE NET CHANGE IN MULTIPLE METRICS---------
# Calculate the total "landscape score" for each metric and landscape, and then
# the net change between each scenario and the baseline

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

# REFERENCE DATA
key = readxl::read_excel('GIS/VEG_key.xlsx')
metrics = read_csv('output/metrics_final.csv')

# COMPILE RESULTS--------
# stats by scenario


## habitat-----------
# total suitable habitat for each landscape, predicted from SDMs

## probability of presence:
habitat = DeltaMultipleBenefits::sum_habitat(
  pathin = 'GIS/prediction_rasters',
  subtype = 'distributions',
  rollup = TRUE,
  keypath = 'output/TABLE_species_key.csv',
  scale = 0.09) %>%
  mutate(UNIT = 'ha')
write_csv(habitat, 'output/scenario_habitat.csv')


## binary presence/absence (using thresholds):
habitat_binary = DeltaMultipleBenefits::sum_habitat(
  pathin = 'GIS/prediction_rasters_threshold',
  subtype = 'habitat',
  rollup = TRUE,
  keypath = 'output/TABLE_species_key.csv',
  scale = 0.09) %>%
  mutate(UNIT = 'ha')
write_csv(habitat_binary, 'output/scenario_habitat_binary.csv')


## land cover totals------------
landcover = DeltaMultipleBenefits::sum_landcover(
  pathin = 'GIS/scenario_rasters',
  maskpath = 'GIS/boundaries/delta.tif',
  pixel_area = 0.09,
  rollup = TRUE) %>%
  # add LABEL fields
  left_join(key %>% select(CODE_NAME, LABEL), by = 'CODE_NAME') %>%
  select(scenario, CODE_NAME, LABEL, area) %>%
  arrange(scenario, CODE_NAME)
write_csv(landcover, 'output/landcover_totals.csv')

## scores-----------
# combine the landscape-specific estimates of the total area of each land cover
# class with the per-unit-area metrics for each land cover class
# - for most metrics, this is the sum over all hectares
# - for Annual Wages, calculate the new weighted average wage across all
#    agricultural ha (i.e., those with a wage value)
# - for climate change resilience, calculate the new overall average

# exclude riparian and managed wetland subclasses (to not double-count), and
# exclude tidal wetlands and water since not addressing them in these scenarios
# (and they shouldn't change)
# --> exclude winter scenarios/landscapes
# --> combine with habitat estimates from above

# check units:
metrics %>% select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT) %>%
  distinct() %>% print(width = Inf)

scores = DeltaMultipleBenefits::sum_metrics(
  metricdat = metrics %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))),
  areadat = landcover %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))) %>%
    filter(!grepl('win', scenario))) %>%
  bind_rows(habitat, habitat_binary) %>%
  mutate(scenario = gsub('_win', '', scenario)) %>% #rename in habitat metrics
  arrange(scenario, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC)

# check updated units:
scores %>% select(METRIC_CATEGORY, METRIC_SUBTYPE, UNIT) %>% distinct()

write_csv(scores, 'output/scenario_scores.csv')



# NET CHANGE------
# compare each scenario to baseline

## landcover----
# for landcover, have to align field names to match fields expected in sum_change()
landcover_change = landcover %>% filter(!grepl('win', scenario)) %>%
  rename(SCORE_TOTAL = area) %>%
  mutate(SCORE_TOTAL_SE = 0) %>%
  DeltaMultipleBenefits::sum_change(scoredat = .) %>%
  select(-ends_with('SE', ignore.case = TRUE))
write_csv(landcover_change, 'output/netchange_landcover.csv')


## metrics-----
scores_change = DeltaMultipleBenefits::sum_change(scores)
write_csv(scores_change, 'output/netchange_scores.csv')



# COUNTY-SPECIFIC ESTIMATES---------

## habitat-------
habitat_county = DeltaMultipleBenefits::sum_habitat(
  pathin = 'GIS/prediction_rasters',
  zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
  subtype = 'distributions',
  rollup = TRUE,
  keypath = 'output/TABLE_species_key.csv')
write_csv(habitat_county, 'output/scenario_habitat_county.csv')

habitat_binary_county = DeltaMultipleBenefits::sum_habitat(
  pathin = 'GIS/prediction_rasters_threshold',
  zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
  subtype = 'habitat',
  rollup = TRUE,
  keypath = 'output/TABLE_species_key.csv')
write_csv(habitat_binary_county, 'output/scenario_habitat_binary_county.csv')

## land cover totals-------
landcover_county = DeltaMultipleBenefits::sum_landcover(
  pathin = 'GIS/scenario_rasters',
  zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
  maskpath = 'GIS/boundaries/delta.tif',
  pixel_area = 0.09,
  rollup = TRUE) %>%
  # add LABEL fields
  left_join(key %>% select(CODE_NAME, LABEL), by = 'CODE_NAME') %>%
  select(scenario, ZONE, CODE_NAME, LABEL, area) %>%
  arrange(scenario, ZONE, CODE_NAME)
write_csv(landcover_county, 'output/landcover_totals_county.csv')

## scores------
scores_county = DeltaMultipleBenefits::sum_metrics(
  metricdat = metrics %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))),
  areadat = landcover_county %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))) %>%
    filter(!grepl('win', scenario))) %>%
  bind_rows(habitat_county, habitat_binary_county) %>%
  mutate(scenario = gsub('_win', '', scenario)) %>% #rename in habitat metrics
  arrange(scenario, ZONE, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC)
# check updated units:
scores_county %>% select(METRIC_CATEGORY, METRIC_SUBTYPE, UNIT) %>% distinct()

write_csv(scores_county, 'output/scenario_scores_county.csv')

## net change--------
landcover_change_county = landcover_county %>% filter(!grepl('win', scenario)) %>%
  rename(SCORE_TOTAL = area) %>%
  mutate(SCORE_TOTAL_SE = 0) %>%
  DeltaMultipleBenefits::sum_change(scoredat = .) %>%
  select(-ends_with('SE', ignore.case = TRUE))
write_csv(landcover_change_county, 'output/netchange_landcover_county.csv')

scores_change_county = DeltaMultipleBenefits::sum_change(scores_county)
write_csv(scores_change_county, 'output/netchange_scores_county.csv')


# CALCULATE CHANGE MAPS--------
# for species distribution models only

## RIPARIAN-----
pred_rip_binary = list(
  'baseline' = list.files('GIS/prediction_rasters_threshold/riparian/baseline', '.tif',
                          full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario1' = list.files('GIS/prediction_rasters_threshold/riparian/scenario1_restoration/', '.tif',
                          full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario2' = list.files('GIS/prediction_rasters_threshold/riparian/scenario2_perennialexpand/', '.tif',
                          full.names = TRUE, recursive = TRUE) %>% rast()
  )
change1_rip = purrr::map(
  names(pred_rip_binary$baseline),
  ~diff(c(pred_rip_binary$baseline[[.x]],
          pred_rip_binary$scenario1[[.x]]))) %>%
  setNames(names(pred_rip_binary$baseline)) %>%
  rast()
writeRaster(change1_rip,
            file.path('GIS/change_rasters/riparian/scenario1_restoration',
                      paste0(names(change1_rip), '.tif')),
            overwrite = TRUE)

change2_rip = purrr::map(
  names(pred_rip_binary$baseline),
  ~diff(c(pred_rip_binary$baseline[[.x]],
          pred_rip_binary$scenario2[[.x]]))) %>%
  setNames(names(pred_rip_binary$baseline)) %>%
  rast()
writeRaster(change2_rip,
            file.path('GIS/change_rasters/riparian/scenario2_perennialexpand',
                      paste0(names(change2_rip), '.tif')),
            overwrite = TRUE)

## WATERBIRDS FALL---------
pred_fall_binary = list(
  'baseline' = list.files('GIS/prediction_rasters_threshold/waterbird_fall/baseline', '.tif',
                          full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario1' = list.files('GIS/prediction_rasters_threshold/waterbird_fall/scenario1_restoration/', '.tif',
                           full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario2' = list.files('GIS/prediction_rasters_threshold/waterbird_fall/scenario2_perennialexpand/', '.tif',
                           full.names = TRUE, recursive = TRUE) %>% rast()
)

change1_fall = purrr::map(
  names(pred_fall_binary$baseline),
  ~diff(c(pred_fall_binary$baseline[[.x]],
          pred_fall_binary$scenario1[[.x]]))) %>%
  setNames(names(pred_fall_binary$baseline)) %>%
  rast()
writeRaster(change1_fall,
            file.path('GIS/change_rasters/waterbird_fall/scenario1_restoration',
                      paste0(names(change1_fall), '.tif')),
            overwrite = TRUE)

change2_fall = purrr::map(
  names(pred_fall_binary$baseline),
  ~diff(c(pred_fall_binary$baseline[[.x]],
          pred_fall_binary$scenario2[[.x]]))) %>%
  setNames(names(pred_fall_binary$baseline)) %>%
  rast()
writeRaster(change2_fall,
            file.path('GIS/change_rasters/waterbird_fall/scenario2_perennialexpand',
                      paste0(names(change2_fall), '.tif')),
            overwrite = TRUE)


## WATERBIRDS WINTER-------
pred_win_binary = list(
  'baseline' = list.files('GIS/prediction_rasters_threshold/waterbird_win/baseline_win', '.tif',
                          full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario1' = list.files('GIS/prediction_rasters_threshold/waterbird_win/scenario1_restoration_win', '.tif',
                           full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario2' = list.files('GIS/prediction_rasters_threshold/waterbird_win/scenario2_perennialexpand_win', '.tif',
                           full.names = TRUE, recursive = TRUE) %>% rast()
)

change1_win = purrr::map(
  names(pred_win_binary$baseline),
  ~diff(c(pred_win_binary$baseline[[.x]],
          pred_win_binary$scenario1[[.x]]))) %>%
  setNames(names(pred_win_binary$baseline)) %>%
  rast()
writeRaster(change1_win,
            file.path('GIS/change_rasters/waterbird_win/scenario1_restoration',
                      paste0(names(change1_win), '.tif')),
            overwrite = TRUE)

change2_win = purrr::map(
  names(pred_win_binary$baseline),
  ~diff(c(pred_win_binary$baseline[[.x]],
          pred_win_binary$scenario2[[.x]]))) %>%
  setNames(names(pred_win_binary$baseline)) %>%
  rast()
writeRaster(change2_win,
            file.path('GIS/change_rasters/waterbird_win/scenario2_perennialexpand',
                      paste0(names(change2_win), '.tif')),
            overwrite = TRUE)
