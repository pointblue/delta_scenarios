# ANALYZE NON-SPATIAL METRICS---------
# Calculate the net change in multiple metrics based solely on the net
# change in land cover classes/sub-classes, and not dependent on spatial
# location (i.e. no spatial distribution model involved)

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')
key = readxl::read_excel('GIS/VEG_key.xlsx')

# REFERENCE DATA
totals = read_csv('output/land_cover_totals.csv')
totals_county = read_csv('output/land_cover_totals_county.csv')

delta = rast('GIS/boundaries/delta.tif')
county_raster = rast('GIS/landscape_rasters/boundaries/counties.tif')

# SOURCE DATA----------

## nonspatial metrics----------
waterquality = read_csv('data/pesticide_exposure.csv', col_types = cols()) %>%
  filter(METRIC %in%
           c('Risk to Aquatic Organisms', 'Critical Pesticides',
             'Groundwater Contaminant'))
economy = bind_rows(
  read_csv('data/crop_production_value.csv', col_types = cols()),
  read_csv('data/livelihoods.csv', col_types = cols()))
ccs = read_csv('data/climate_change_resilience2.csv', col_types = cols())
acs = read_csv('data/avian_conservation_score.csv', col_types = cols())

metrics = bind_rows(economy, waterquality, ccs, acs) %>%
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

## SDM predictions---------
preds = list(
  ## RIPARIAN
  riparian = list(
    'baseline' = list.files(
      'GIS/prediction_rasters/riparian/baseline', '.tif$',
      full.names = TRUE) %>% rast(),
    'scenario1_restoration' = list.files(
      'GIS/prediction_rasters/riparian/scenario1_restoration/', '.tif$',
      full.names = TRUE) %>% rast(),
    'scenario2_perennialexpand' = list.files(
      'GIS/prediction_rasters/riparian/scenario2_perennialexpand/', '.tif$',
      full.names = TRUE) %>% rast()
  ),
  # WATERBIRDS FALL
  waterbird_fall = list(
    'baseline' = list.files(
      'GIS/prediction_rasters/waterbird_fall/baseline', '.tif$',
      full.names = TRUE) %>% rast(),
    'scenario1_restoration' = list.files(
      'GIS/prediction_rasters/waterbird_fall/scenario1_restoration', '.tif$',
      full.names = TRUE) %>% rast(),
    'scenario2_perennialexpand' = list.files(
      'GIS/prediction_rasters/waterbird_fall/scenario2_perennialexpand',
      '.tif$', full.names = TRUE) %>% rast()
  ),
  # WATERBIRDS WINTER
  waterbird_win = list(
    'baseline' = list.files(
      'GIS/prediction_rasters/waterbird_win/baseline_win', '.tif$',
      full.names = TRUE) %>% rast(),
    'scenario1_restoration' = list.files(
      'GIS/prediction_rasters/waterbird_win/scenario1_restoration_win', '.tif$',
      full.names = TRUE) %>% rast(),
    'scenario2_perennialexpand' = list.files(
      'GIS/prediction_rasters/waterbird_win/scenario2_perennialexpand_win',
      '.tif$', full.names = TRUE) %>% rast()
  )
)

spp_key = tibble(
  spp = c(names(preds$riparian$baseline),
          names(preds$waterbird_win$baseline))
) %>%
  mutate(spp = factor(spp,
                      levels = c('NUWO', 'ATFL', 'BHGR', 'LAZB', 'COYE',
                                 'YEWA', 'SPTO', 'SOSP', 'YBCH',
                                 'geese', 'dblr', 'divduck', 'crane', 'shore',
                                 'cicon')),
         label = recode(spp,
                        NUWO = "Nuttall's Woodpecker",
                        ATFL = 'Ash-throated Flycatcher',
                        BHGR = 'Black-headed Grosbeak',
                        LAZB = 'Lazuli Bunting',
                        COYE = 'Common Yellowthroat',
                        YEWA = 'Yellow Warbler',
                        SPTO = 'Spotted Towhee',
                        SOSP = 'Song Sparrow',
                        YBCH = 'Yellow-breasted Chat',
                        dblr = 'Dabbling Ducks',
                        shore = 'Shorebirds',
                        cicon = 'Herons/Egrets',
                        crane = 'Cranes',
                        geese = 'Geese',
                        divduck = 'Diving Ducks')) %>%
  arrange(spp)


# SCENARIO SCORES--------
# represent each metric in each scenario by a total landscape score

## ENTIRE DELTA--------

# SDM habitat: calculate the total suitable habitat for each species/group and
# scenario
habitat = purrr::map_dfr(
  preds,
  function(SDM) {
    purrr::map_dfr(SDM,
                   function(scenario) {
                     purrr::map(as.list(scenario) %>% setNames(names(scenario)),
                                function(spp) {
                                  values(spp) %>% sum(na.rm = TRUE)
                                })
                   }, .id = 'scenario') %>%
      pivot_longer(-scenario)
  }, .id = 'SDM') %>%
  mutate(METRIC_CATEGORY = 'Biodiversity Support',
         METRIC_SUBTYPE = 'species distribution',
         SCORE_TOTAL = value / .09, # convert to ha
         UNIT = 'ha') %>%
  unite('METRIC_SUBTYPE', c('METRIC_SUBTYPE', 'SDM'), sep = ': ') %>%
  left_join(spp_key %>% select(name = spp, METRIC = label), by = 'name') %>%
  select(scenario, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT, SCORE_TOTAL)
write_csv(habitat, 'output/scenario_habitat.csv')

# combine with all other metrics:
# join with the areas of each land cover class in each scenario to
# generate total scores, being aware of different units involved
metrics %>% select(METRIC, UNIT) %>% distinct()

# for most, including ranking scores, multiply by number of hectares; for annual
# wages, average wages per employee doesn't change just from changing the total
# area of each land cover
scores = full_join(totals, metrics, by = 'CODE_NAME') %>%
  mutate(
    # keep mean for Annual Wages for now, otherwise multiply by area for total score
    SCORE_TOTAL = if_else(METRIC == 'Annual Wages', SCORE_MEAN, area * SCORE_MEAN),
    # multiplication by a constant, so no other propagation of error needed
    SCORE_TOTAL_SE = if_else(METRIC == 'Annual Wages', SCORE_SE, area * SCORE_SE)) %>%
  # # for ACS: keep distinct data for riparian subclasses (introscrub gets its own ACS score)
  # filter(!(CODE_NAME == 'RIPARIAN' &
  #            METRIC_CATEGORY %in% c('Biodiversity Support'))) %>%
  # # for water quality & economy: drop riparian subclasses (all zero anyway)
  # filter(!(grepl('RIPARIAN_', CODE_NAME) &
  #            METRIC_CATEGORY %in% c('Climate Change Resilience',
  #                                   'Agricultural Livelihoods',
  #                                   'Water Quality',
  #                                   'Biodiversity Support'))) %>%
  #only keep general riparian and managed wetland scores (no distinction for
  #subclasses)
  filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_', CODE_NAME))) %>%
  #not addressing these in these scenarios (and they shouldn't change)
  filter(!CODE_NAME %in% c('WETLAND_TIDAL', 'WATER'))

# # check representation across land covers & missing values for a subset of metrics:
# scores %>% filter(scenario == 'baseline') %>%
#   filter(METRIC %in%
#            c('Breeding Landbirds: Riparian', 'Drought', 'Agricultural Jobs',
#              'Groundwater Contaminant')) %>%
#   select(CODE_NAME, METRIC, SCORE_TOTAL) %>%
#   pivot_wider(names_from = METRIC, values_from = SCORE_TOTAL) %>% print(n = Inf)

# summarize each metric for the entire Delta, for baseline and each scenario:
# - for most metrics, this is the sum over all hectares
# - for Annual Wages, calculate the new weighted average wage across all
#    agricultural ha (i.e., those with a wage value)
scores_sum = bind_rows(
  # for all but annual wages:
  scores %>% filter(METRIC != 'Annual Wages') %>%
    mutate(
      SCORE_TOTAL_SE = case_when(
        METRIC_CATEGORY == 'Biodiversity Support' ~ SCORE_TOTAL_SE,
        is.na(SCORE_TOTAL_SE) ~ 0,
        TRUE ~ SCORE_TOTAL_SE)) %>%
    # only SE with NA is for biodiversity (assume all others = 0: assumed values
    # for economic, water quality metrics where no data reported, and resilience
    # to salinity)
    group_by(scenario, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT) %>%
    summarize(SCORE_TOTAL = sum(SCORE_TOTAL),
              SCORE_TOTAL_SE = sqrt(sum(SCORE_TOTAL_SE^2)),
              .groups = 'drop'),
  # for annual wages: multiply the average wage per-land cover by the proportion
  # of the total ag landscape made up by that land cover
  scores %>% filter(METRIC == 'Annual Wages' & SCORE_TOTAL > 0) %>%
    group_by(scenario, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT) %>%
    mutate(area_ag_total = sum(area),
           area_prop = area / area_ag_total) %>%
    summarize(SCORE_TOTAL = sum(SCORE_TOTAL * area_prop),
              SCORE_TOTAL_SE = sqrt(sum((SCORE_TOTAL_SE * area_prop)^2)),
              .groups = 'drop'),
  # add habitat
  habitat
) %>% arrange(scenario, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC)
write_csv(scores_sum, 'output/scenario_scores.csv')

## BY COUNTY---------
# repeat above, but summarize by county

# SDM habitat:
habitat_county = purrr::map_dfr(
  preds,
  function(SDM) {
    purrr::map_dfr(SDM,
                   function(scenario) {
                     purrr::map_dfr(
                       as.list(scenario) %>% setNames(names(scenario)),
                       function(spp) {
                         zonal(spp, county_raster, 'sum', na.rm = TRUE) %>%
                           setNames(c('COUNTY_NAME', 'value'))
                       }, .id = 'name')
                   }, .id = 'scenario')
  }, .id = 'SDM') %>%
  mutate(METRIC_CATEGORY = 'Biodiversity Support',
         METRIC_SUBTYPE = 'species distribution',
         SCORE_TOTAL = value / .09, # convert to ha
         UNIT = 'ha') %>%
  unite('METRIC_SUBTYPE', c('METRIC_SUBTYPE', 'SDM'), sep = ': ') %>%
  left_join(spp_key %>% select(name = spp, METRIC = label), by = 'name') %>%
  select(scenario, county = COUNTY_NAME, METRIC_CATEGORY, METRIC_SUBTYPE,
         METRIC, UNIT, SCORE_TOTAL)
write_csv(habitat_county, 'output/scenario_habitat_county.csv')

# all other scores:
scores_county = full_join(totals_county, metrics, by = 'CODE_NAME') %>%
  mutate(
    SCORE_TOTAL = if_else(METRIC == 'Annual Wages', SCORE_MEAN, area * SCORE_MEAN),
    # multiplication by a constant, so no other propagation of error needed
    SCORE_TOTAL_SE = if_else(METRIC == 'Annual Wages', SCORE_SE, area * SCORE_SE)) %>%
  # # for ACS: keep distinct data for riparian subclasses (introscrub gets its own ACS score)
  # filter(!(CODE_NAME == 'RIPARIAN' &
  #            METRIC_CATEGORY %in% c('Biodiversity Support'))) %>%
  # for water quality & economy: assume no data/pesticides either way
  # filter(!(grepl('RIPARIAN_', CODE_NAME) &
  #            METRIC_CATEGORY %in% c('Climate Change Resilience',
  #                                   'Agricultural Livelihoods',
  #                                   'Water Quality',
  #                                   'Biodiversity Support'))) %>%
  #only keep general riparian and managed wetland category (no distinction for
  #subclasses with any metric)
  filter(!(grepl('RIPARIAN_|WETLAND_MANAGED_', CODE_NAME))) %>%
  #not addressing these in these scenarios (and they shouldn't change)
  filter(!CODE_NAME %in% c('WETLAND_TIDAL', 'WATER'))

# # check representation across land covers & missing values for a subset of metrics:
# scores_county %>% filter(scenario == 'baseline' & county == 'Yolo') %>%
#   filter(METRIC %in%
#            c('Breeding Landbirds: Riparian', 'Drought', 'Agricultural Jobs',
#              'Groundwater Contaminant')) %>%
#   select(CODE_NAME, METRIC, SCORE_TOTAL) %>%
#   pivot_wider(names_from = METRIC, values_from = SCORE_TOTAL) %>% print(n = Inf)

# summarize each metric for the entire Delta, for baseline and each scenario:
# - for most metrics, this is the sum over all hectares
# - for Annual Wages, calculate the new weighted average wage across all
#    agricultural ha (i.e., those with a wage value)
scores_county_sum = bind_rows(
  # for all but annual wages:
  scores_county %>% filter(METRIC != 'Annual Wages') %>%
    mutate(
      SCORE_TOTAL_SE = case_when(
        METRIC_CATEGORY == 'Biodiversity Support' ~ SCORE_TOTAL_SE,
        is.na(SCORE_TOTAL_SE) ~ 0,
        TRUE ~ SCORE_TOTAL_SE)) %>%
    # only SE with NA is for biodiversity (assume all others = 0: assumed values
    # for economic, water quality metrics where no data reported, and resilience
    # to salinity)
    group_by(scenario, county, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT) %>%
    summarize(SCORE_TOTAL = sum(SCORE_TOTAL),
              SCORE_TOTAL_SE = sqrt(sum(SCORE_TOTAL_SE^2)),
              .groups = 'drop'),
  # for annual wages: multiply the average wage per-land cover by the proportion
  # of the total ag landscape made up by that land cover
  scores_county %>% filter(METRIC == 'Annual Wages' & SCORE_TOTAL > 0) %>%
    group_by(scenario, county, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC, UNIT) %>%
    mutate(area_ag_total = sum(area),
           area_prop = area / area_ag_total) %>%
    summarize(SCORE_TOTAL = sum(SCORE_TOTAL * area_prop),
              SCORE_TOTAL_SE = sqrt(sum((SCORE_TOTAL_SE * area_prop)^2)),
              .groups = 'drop'),
  # add habitat
  habitat_county
) %>% arrange(scenario, county, METRIC_CATEGORY, METRIC_SUBTYPE, METRIC)
write_csv(scores_county_sum, 'output/scenario_scores_county.csv')


# NET CHANGE------
# compare each scenario to baseline scores for each metric

## ENTIRE DELTA--------
change_scores = left_join(
  scores_sum %>% filter(scenario != 'baseline') %>%
    rename(SCENARIO = SCORE_TOTAL, SCENARIO_SE = SCORE_TOTAL_SE),
  scores_sum %>% filter(scenario == 'baseline') %>%
    rename(BASELINE = SCORE_TOTAL, BASELINE_SE = SCORE_TOTAL_SE) %>%
    select(-scenario),
  by = c('METRIC_CATEGORY', 'METRIC_SUBTYPE', 'METRIC', 'UNIT')) %>%
  mutate(net_change = SCENARIO - BASELINE,
         net_change_se = sqrt(SCENARIO_SE^2 + BASELINE_SE^2),
         change_prop = net_change/BASELINE,
         change_prop_se = abs(change_prop) * sqrt((net_change_se/net_change)^2 + (BASELINE_SE/BASELINE)^2),
         change_pct = change_prop * 100,
         change_pct_se = change_prop_se * 100,
         change_pct = if_else(METRIC_CATEGORY == 'Water Quality', -1 * change_pct, change_pct))
write_csv(change_scores, 'output/scenario_change.csv')

## BY COUNTY---------
change_scores_county = left_join(
  scores_county_sum %>% filter(scenario != 'baseline') %>%
    rename(SCENARIO = SCORE_TOTAL, SCENARIO_SE = SCORE_TOTAL_SE),
  scores_county_sum %>% filter(scenario == 'baseline') %>%
    rename(BASELINE = SCORE_TOTAL, BASELINE_SE = SCORE_TOTAL_SE) %>%
    select(-scenario),
  by = c('county', 'METRIC_CATEGORY', 'METRIC_SUBTYPE', 'METRIC', 'UNIT')) %>%
  mutate(net_change = SCENARIO - BASELINE,
         net_change_se = sqrt(SCENARIO_SE^2 + BASELINE_SE^2),
         change_prop = net_change/BASELINE,
         change_prop_se = abs(change_prop) * sqrt((net_change_se/net_change)^2 + (BASELINE_SE/BASELINE)^2),
         change_pct = change_prop * 100,
         change_pct_se = change_prop_se * 100,
         change_pct = if_else(METRIC_CATEGORY == 'Water Quality', -1 * change_pct, change_pct))
write_csv(change_scores_county, 'output/scenario_change_county.csv')

## CHANGE MAPS--------
# for species distribution models only

# CHANGE MAPS--------

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
