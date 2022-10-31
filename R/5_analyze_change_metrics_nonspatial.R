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


# COMPILE METRICS----------
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


# SCENARIO SCORES--------
# combine metrics with the areas of each land cover class in each scenario to
# generate total scores, being aware of different units involved
metrics %>% select(METRIC, UNIT) %>% distinct()

# for most, including ranking scores, multiply by number of hectares; for annual
# wages, average wages per employee doesn't change just from changing the total
# area of each land cover

# Delta overall:
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

# check representation across land covers & missing values for a subset of metrics:
scores %>% filter(scenario == 'baseline') %>%
  filter(METRIC %in%
           c('Breeding Landbirds: Riparian', 'Drought', 'Agricultural Jobs',
             'Groundwater Contaminant')) %>%
  select(CODE_NAME, METRIC, SCORE_TOTAL) %>%
  pivot_wider(names_from = METRIC, values_from = SCORE_TOTAL) %>% print(n = Inf)

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
              .groups = 'drop')
)
write_csv(scores_sum, 'output/scenario_scores.csv')

# by county

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

# check representation across land covers & missing values for a subset of metrics:
scores_county %>% filter(scenario == 'baseline' & county == 'Yolo') %>%
  filter(METRIC %in%
           c('Breeding Landbirds: Riparian', 'Drought', 'Agricultural Jobs',
             'Groundwater Contaminant')) %>%
  select(CODE_NAME, METRIC, SCORE_TOTAL) %>%
  pivot_wider(names_from = METRIC, values_from = SCORE_TOTAL) %>% print(n = Inf)

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
              .groups = 'drop')
)
write_csv(scores_county_sum, 'output/scenario_scores_county.csv')



# NET CHANGE------
# overall
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
write_csv(change_scores, 'output/scenario_change_nonspatial.csv')

# by county
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
write_csv(change_scores, 'output/scenario_change_nonspatial_county.csv')
