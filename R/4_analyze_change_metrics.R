# ANALYZE NET CHANGE IN MULTIPLE METRICS---------
# Calculate the total "landscape score" for each metric and landscape, and then
# the net change between each scenario and the baseline

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

# REFERENCE DATA
key = readxl::read_excel('GIS/VEG_key.xlsx')
metrics = read_csv('output/metrics.csv')
habitat = read_csv('output/scenario_habitat.csv')
habitat_binary = read_csv('output/scenario_habitat_binary.csv')

# # calculate land cover totals------------
# # total area of each land cover class/subclass in each landscape:
# landcover = DeltaMultipleBenefits::sum_landcover(
#   pathin = 'GIS/scenario_rasters',
#   maskpath = 'GIS/boundaries/delta.tif',
#   pixel_area = 0.09,
#   rollup = TRUE) %>%
#   # add LABEL fields
#   left_join(key %>% select(CODE_NAME, LABEL), by = 'CODE_NAME') %>%
#   select(scenario, CODE_NAME, LABEL, area) %>%
#   arrange(scenario, CODE_NAME)
# write_csv(landcover, 'output/landcover_totals.csv')

# LANDSCAPE SCORES-----------
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
metrics %>% select(METRIC_CATEGORY, METRIC, UNIT) %>%
  distinct() %>% print(width = Inf)

# land cover totals
landcover = read_csv('output/landcover_totals.csv')

scores = DeltaMultipleBenefits::sum_metrics(
  metricdat = metrics %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))),
  areadat = landcover %>%
    filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))) %>%
    filter(!grepl('win', scenario))) %>%
  bind_rows(habitat, habitat_binary) %>%
  mutate(scenario = gsub('_win', '', scenario)) %>% #rename in habitat metrics
  arrange(scenario, METRIC_CATEGORY, METRIC) %>%
  mutate(UNIT = gsub('/ha', '', UNIT)) # now scores are not per ha

# check updated units:
scores %>% select(METRIC_CATEGORY, METRIC_SUBTYPE, UNIT) %>% distinct()

write_csv(scores, 'output/scenario_scores.csv')

## table--------
scores_table = scores %>% filter(is.na(METRIC_SUBTYPE) | grepl('habitat', METRIC_SUBTYPE)) %>%
  filter(METRIC_CATEGORY != 'Biodiversity Support' | grepl('Total', METRIC)) %>%
  mutate(across(c(SCORE_TOTAL, SCORE_TOTAL_SE),
                ~case_when(METRIC == 'Gross Production Value' ~ ./1000,
                           METRIC_CATEGORY == 'Water Quality' ~ ./1000,
                           TRUE ~ .)),
         UNIT = case_when(METRIC == 'Gross Production Value' ~ 'USD/yr (thousands)',
                          METRIC_CATEGORY == 'Water Quality' ~ 'MT/yr',
                          TRUE ~ UNIT),
         across(c(SCORE_TOTAL, SCORE_TOTAL_SE),
                ~case_when(METRIC_CATEGORY %in%
                             c('Water Quality', 'Climate Change Resilience') ~
                             round(., digits = 2) %>% format(nsmall = 2),
                           TRUE ~ round(., digits = 0) %>% format(nsmall = 0)))) %>%
  select(scenario, METRIC_CATEGORY, METRIC, UNIT, SCORE_TOTAL, SCORE_TOTAL_SE) %>%
  pivot_wider(names_from = scenario,
              values_from = c(SCORE_TOTAL, SCORE_TOTAL_SE)) %>%
  select(METRIC_CATEGORY, METRIC, UNIT, ends_with('baseline'),
         ends_with('restoration'), ends_with('expand'))
write_csv(scores_table, 'output/TABLE_scores_summary.csv')

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



# # COUNTY-SPECIFIC ESTIMATES---------
#
# ## habitat-------
# habitat_county = DeltaMultipleBenefits::sum_habitat(
#   pathin = 'GIS/prediction_rasters',
#   zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
#   subtype = 'distributions',
#   rollup = TRUE,
#   keypath = 'output/TABLE_species_key.csv')
# write_csv(habitat_county, 'output/scenario_habitat_county.csv')
#
# habitat_binary_county = DeltaMultipleBenefits::sum_habitat(
#   pathin = 'GIS/prediction_rasters_threshold',
#   zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
#   subtype = 'habitat',
#   rollup = TRUE,
#   keypath = 'output/TABLE_species_key.csv')
# write_csv(habitat_binary_county, 'output/scenario_habitat_binary_county.csv')
#
# ## land cover totals-------
# landcover_county = DeltaMultipleBenefits::sum_landcover(
#   pathin = 'GIS/scenario_rasters',
#   zonepath = 'GIS/landscape_rasters/boundaries/counties.tif',
#   maskpath = 'GIS/boundaries/delta.tif',
#   pixel_area = 0.09,
#   rollup = TRUE) %>%
#   # add LABEL fields
#   left_join(key %>% select(CODE_NAME, LABEL), by = 'CODE_NAME') %>%
#   select(scenario, ZONE, CODE_NAME, LABEL, area) %>%
#   arrange(scenario, ZONE, CODE_NAME)
# write_csv(landcover_county, 'output/landcover_totals_county.csv')
#
# ## scores------
# scores_county = DeltaMultipleBenefits::sum_metrics(
#   metricdat = metrics %>%
#     filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))),
#   areadat = landcover_county %>%
#     filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_|WETLAND_TIDAL|WATER', CODE_NAME))) %>%
#     filter(!grepl('win', scenario))) %>%
#   bind_rows(habitat_county, habitat_binary_county) %>%
#   mutate(scenario = gsub('_win', '', scenario)) %>% #rename in habitat metrics
#   arrange(scenario, ZONE, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC)
# # check updated units:
# scores_county %>% select(METRIC_CATEGORY, METRIC_SUBTYPE, UNIT) %>% distinct()
#
# write_csv(scores_county, 'output/scenario_scores_county.csv')
#
# ## net change--------
# landcover_change_county = landcover_county %>% filter(!grepl('win', scenario)) %>%
#   rename(SCORE_TOTAL = area) %>%
#   mutate(SCORE_TOTAL_SE = 0) %>%
#   DeltaMultipleBenefits::sum_change(scoredat = .) %>%
#   select(-ends_with('SE', ignore.case = TRUE))
# write_csv(landcover_change_county, 'output/netchange_landcover_county.csv')
#
# scores_change_county = DeltaMultipleBenefits::sum_change(scores_county)
# write_csv(scores_change_county, 'output/netchange_scores_county.csv')
