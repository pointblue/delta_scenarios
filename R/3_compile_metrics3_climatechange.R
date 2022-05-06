# CLIMATE CHANGE SENSITIVITY INDICES
# including expert opinion rankings for climate change sensitivity from MBCP
# multiple benefits synthesis report (including management rigidity, drought,
# flood intolerance, temperature) and salt tolerance (from Delta Adapts)

# [also have rankings for climate change exposure (including pest & disease
# impacts, market volatility/pollution, land use/land cover change, and capacity
# gap) - but these are more generic/less specific to the Delta]

# PACKAGES & FUNCTIONS
source('R/0_packages.R')

# ORIGINAL DATA----------
# Scores for each factor within a land cover were 1, 2, or 3 (also N/A or 0=no
# answer), with 1 being the lowest sensitivity/exposure to the given factor and
# 3 being the highest sensitivity/exposure.

ccv_raw <- read_csv('data_orig/Peterson_et_al_2020/ccvulnerability_index_full.csv',
                        col_types = cols()) %>%
  pivot_longer(-scorer,
               names_to = c('FACTOR', 'LANDCOVER'), names_pattern = '(.*)_(.*)',
               values_to = 'value') %>%
  mutate(FACTOR_TYPE = if_else(FACTOR %in% c('mgmt', 'spec', 'dr', 'fl', 'temp'),
                               'SENSITIVITY', 'EXPOSURE'),
         FACTOR = recode(FACTOR,
                         cg = 'capacity gap',
                         lulc = 'lulc change',
                         mar = 'market volatility',
                         po = 'pollution',
                         pd = 'pest & disease',
                         dr = 'drought sensitivity',
                         fl = 'flood intolerance',
                         mgmt = 'mgmt', #refers rigidity in mgmt for crops
                         spec = 'mgmt', #refers to specificity in mgmt for natural landcovers
                         temp = 'heat')) %>%
  filter(!is.na(value))

# keep only SENSITIVITY factors, and invert meaning from "sensitivity" to
# "tolerance/resilience" so a score of 3 is "better"
ccv_invert = ccv_raw %>%
  filter(FACTOR_TYPE == 'SENSITIVITY' & FACTOR != 'mgmt') %>%
  mutate(value = case_when(value == 1 ~ 3,
                           value == 3 ~ 1,
                           TRUE ~ 2),
         FACTOR_TYPE = 'RESILIENCE',
         FACTOR = gsub(' sensitivity| intolerance', '', FACTOR))

## estimate average------
# across scorers, average score for each factor and landcover

ccv_avg = ccv_invert %>%
  group_by(LANDCOVER, FACTOR_TYPE, FACTOR) %>%
  summarize(SCORE = mean(value, na.rm = TRUE),
            SCORE_MIN = min(value, na.rm = TRUE),
            SCORE_MAX = max(value, na.rm = TRUE),
            .groups = 'drop') %>%
  arrange(LANDCOVER, FACTOR_TYPE, FACTOR)

ccv_avg %>% select(LANDCOVER:SCORE) %>% pivot_wider(names_from = 'FACTOR', values_from = SCORE)

## classify--------
ccv_classify = ccv_avg %>%
  mutate(CODE_NAME = case_when(
    LANDCOVER == 'orchard' ~ 'ORCHARD_DECIDUOUS',
    LANDCOVER %in% c('citrus', 'olives') ~ 'ORCHARD_CITRUS&SUBTROPICAL',
    LANDCOVER == 'vineyard' ~ 'VINEYARD',
    LANDCOVER %in% c('cereal', 'grain') ~ 'GRAIN&HAY',
    LANDCOVER %in% c('cotton','field crops') ~ 'FIELD', #is cotton relevant to Delta?
    LANDCOVER %in% c('corn', 'cotn') ~ 'FIELD_CORN', # assume cotn is a typo for corn?
    LANDCOVER %in% c('tomato', 'truck crops') ~ 'ROW',
    LANDCOVER == 'rice' ~ 'RICE',
    LANDCOVER %in% c('pasture', 'irrigated pasture') ~ 'PASTURE',
    LANDCOVER == 'alfalfa' ~ 'PASTURE_ALFALFA',
    LANDCOVER %in% c('grass', 'grassland', 'dryland pasture') ~ 'GRASSLAND',
    LANDCOVER == 'riparian' ~ 'RIPARIAN',
    LANDCOVER %in% c('wetl', 'wetland') ~ 'WETLAND_MANAGED')) %>%
  filter(LANDCOVER != 'cotton')

ccv_classify %>% select(CODE_NAME, FACTOR, SCORE) %>%
  pivot_wider(names_from = 'FACTOR', values_from = SCORE)

# SUPPLEMENTAL DATA--------
## Delta Adapts-----
# and other relevant Delta-specific literature:
# - ag production & crop yield tech memo, p2-8
# (https://deltacouncil.ca.gov/pdf/delta-plan/2021-06-07-crop-yield-and-agricultural-production-technical-memorandum.pdf)
# - Medellin-Azuara et al. 2014 (https://escholarship.org/uc/item/4b7295m9#main)

da_raw = bind_rows(
  tibble(scorer = 'DeltaAdapts',
         FACTOR = 'salinity',
         FACTOR_TYPE = 'RESILIENCE',
         LANDCOVER = c('truck crops', 'corn', 'alfalfa',
                       'vineyard', 'orchard',
                       'grain', 'rice', 'field crops', 'olives'),
         SCORE = c(2, 2, 2, #mod sensitive
                   1, 1,  #sensitive
                   3, 3, 3, 3)), #mod tolerant (assume "grains" includes rice?)
  tibble(scorer = 'medellin',
         FACTOR = 'salinity',
         FACTOR_TYPE = 'RESILIENCE',
         LANDCOVER = c('irrigated pasture', 'dryland pasture'),
         SCORE = c(2, 2) #mod sensitive
         ),
  tibble(scorer = 'CVSALTS',
         FACTOR = 'salinity',
         FACTOR_TYPE = 'RESILIENCE',
         LANDCOVER = c('wetland'),
         SCORE = 3) #common wetland plants less sensitive than most crops
)

da_classify = da_raw %>%
  mutate(CODE_NAME = case_when(
    LANDCOVER == 'orchard' ~ 'ORCHARD_DECIDUOUS',
    LANDCOVER %in% c('citrus', 'olives') ~ 'ORCHARD_CITRUS&SUBTROPICAL',
    LANDCOVER == 'vineyard' ~ 'VINEYARD',
    LANDCOVER %in% c('cereal', 'grain') ~ 'GRAIN&HAY',
    LANDCOVER %in% c('cotton','field crops') ~ 'FIELD', #is cotton relevant to Delta?
    LANDCOVER %in% c('corn', 'cotn') ~ 'FIELD_CORN', # assume cotn is a typo for corn?
    LANDCOVER %in% c('tomato', 'truck crops') ~ 'ROW',
    LANDCOVER == 'rice' ~ 'RICE',
    LANDCOVER %in% c('pasture', 'irrigated pasture') ~ 'PASTURE',
    LANDCOVER == 'alfalfa' ~ 'PASTURE_ALFALFA',
    LANDCOVER %in% c('grass', 'grassland', 'dryland pasture') ~ 'GRASSLAND',
    LANDCOVER == 'riparian' ~ 'RIPARIAN',
    LANDCOVER %in% c('wetl', 'wetland') ~ 'WETLAND_MANAGED'))

# --> missing data for riparian; wetland data filled in from a search -
# references to wetland plants (swamp timothy, watergrass) being less sensitive
# than many crops

## DRERIP conceptual model-----
# sensitivity data for specific riparian species

# riptable = tabulizer::locate_areas('data_orig/DRERIP_Rip_Veg_Model_ Oct08-EMG_FINAL_fmtd3.pdf',
#                                    pages = 37)

ripdat_raw = tabulizer::extract_tables(
  'data_orig/DRERIP_Rip_Veg_Model_ Oct08-EMG_FINAL_fmtd3.pdf',
  pages = 37, area = list(c(100, 24, 1122, 1122)))[[1]] %>%
  as_tibble() %>%
  rlang::set_names(
    c('scientific_name', 'common_name', 'growth_form', 'evergreen',
      'foliage_porosity_summer', 'foliage_porosity_winter', 'shade_tolerance',
      'root_depth_min_in', 'height_20yrs_ft', 'height_max_ft',
      'precip_min_in', 'precip_max_in', 'temp_min_F', 'CA_wetl_ind',
      'flood_tolerance', 'drought_tolerance', 'moisture_use', 'growth_rate',
      'herbivory_tolerance', 'fruit_seed_period',
      'repro_spread_rate', 'veg_spread_rate', 'adapted_fines', 'adapted_medium',
      'adapted_coarse', 'n_fixation', 'fertility_req', 'CN_ratio',
      'CaCO3_tolerance', 'pH_min', 'pH_max', 'salinity_tolerance', 'impact_IPC',
      'invasiveness_IPC', 'protection_status', 'comments'))

ripdat_classify = ripdat_raw %>%
  select(scientific_name, common_name, drought_tolerance, flood_tolerance,
         salinity_tolerance) %>%
  mutate(CODE_NAME = case_when(
    scientific_name == 'Populus fremontii' ~ 'RIPARIAN_FOREST_POFR',
    scientific_name == 'Quercus lobata' ~ 'RIPARIAN_FOREST_QULO',
    scientific_name %in% c('Salix gooddingii',
                           'Salix laevigata') ~ 'RIPARIAN_FOREST_SALIX',
    scientific_name %in% c('Acer negundo',
                           'Fraxinus latifolia',
                           'Juglans hindsii\r(based on Juglans major in USDA)',
                           'Alnus rhombifolia',
                           'Platanus racemosa') ~ 'RIPARIAN_FOREST_MIXED',
    scientific_name %in% c('Salix exigua',
                           'Salix lasiolepis',
                           'Salix lucida') ~ 'RIPARIAN_SCRUB_SALIX',
    scientific_name %in% c('Cephalanthus occidentalis',
                           'Sambucus nigra',
                           'Vitis californica',
                           'Toxicodendron diversilobum') ~'RIPARIAN_SCRUB_MIXED',
    scientific_name %in% c('Tamarix parviflora (2)',
                           'Tamarix ramosissima (2)',
                           'Arundo Donax',
                           'Rubus discolor') ~ 'RIPARIAN_SCRUB_INTRO')) %>%
  filter(!is.na(CODE_NAME)) %>%
  pivot_longer(drought_tolerance:salinity_tolerance, names_to = 'FACTOR') %>%
  mutate(FACTOR = recode(FACTOR,
                         'drought_tolerance' = 'drought',
                         'flood_tolerance' = 'flood',
                         'salinity_tolerance' = 'salinity'),
         FACTOR_TYPE = 'RESILIENCE',
         # scale relative to crops - low tolerance for riparian veg may be
         # higher relative to crops; ensure overall scores line up with expert
         # opinion scores for RIPARIAN in general
         SCORE = case_when(value %in% c('None', 'Low/None') ~ 2,
                           value %in% c('Low', 'Low: prefers well\rdrained soils') ~ 2.25,
                           value == 'Medium' ~ 2.5,
                           value == 'High/Moderate' ~ 2.75,
                           value == 'High' ~ 3,
                           value == '' ~ NA_real_),
         scorer = 'ripmod') %>%
  select(scorer, FACTOR_TYPE, FACTOR, CODE_NAME, LANDCOVER = scientific_name,
         SCORE)

write_csv(ripdat_classify, 'data_orig/riparian_sensitivity.csv')

ripdat_avg = ripdat_classify %>%
  group_by(CODE_NAME, FACTOR_TYPE, FACTOR, scorer) %>%
  summarize(SCORE = mean(SCORE, na.rm = TRUE),
         SCORE_MIN = min(SCORE, na.rm = TRUE),
         SCORE_MAX = max(SCORE, na.rm = TRUE), .groups = 'drop')

# COMPILED-------
ccv_compiled = bind_rows(ccv_classify %>% mutate(scorer = 'Peterson'),
                         da_classify, ripdat_avg) %>%
  select(-LANDCOVER)

ccv_compiled %>% select(FACTOR, SCORE, CODE_NAME) %>%
  pivot_wider(names_from = FACTOR, values_from = SCORE) %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)

## fill in missing------
# - wheat: assume equivalent to grain&hay
# - field: assume equivalent to row (except for salinity, which has its own value)
# - idle & barren: assume low sensitivity to all/high resilience
# - riparian: missing general salinity (equivalent to average of subclasses?)
# - riparian subclasses: missing heat (equivalent to overall riparian)
# - wetland_other: assume equivalent to grassland
# - woodland & scrub: assume equivalent to riparian
# - urban: resilient to salinity, mod to drought/heat, low to flood?
# - water, wetland_tidal: IGNORE

ccv_fill = ccv_compiled %>%
  bind_rows(
    ccv_compiled %>% filter(CODE_NAME == 'GRAIN&HAY') %>%
      mutate(CODE_NAME = 'GRAIN&HAY_WHEAT', scorer = 'new'),
    ccv_compiled %>% filter(CODE_NAME == 'ROW' & FACTOR != 'salinity') %>%
      mutate(CODE_NAME = 'FIELD', scorer = 'new'),
    ccv_compiled %>% filter(CODE_NAME == 'ROW') %>%
      mutate(CODE_NAME = 'IDLE', SCORE = 3, SCORE_MIN = NA, SCORE_MAX = NA, scorer = 'new'),
    ccv_compiled %>% filter(CODE_NAME == 'ROW') %>%
      mutate(CODE_NAME = 'BARREN', SCORE = 3, SCORE_MIN = NA, SCORE_MAX = NA, scorer = 'new'),
    ccv_compiled %>% filter(CODE_NAME == 'GRASSLAND') %>%
      mutate(CODE_NAME = 'WETLAND_OTHER', scorer = 'new'),
    ccv_compiled %>% filter(CODE_NAME == 'GRASSLAND') %>%
      mutate(CODE_NAME = 'URBAN', scorer = 'new',
             SCORE = case_when(FACTOR == 'drought' ~ 2,
                               FACTOR == 'heat' ~ 2,
                               FACTOR == 'flood' ~ 1,
                               FACTOR == 'salinity' ~ 3)),
    # overall salinity tolerance scores for riparian
    ripdat_avg %>%
      filter(
        FACTOR == 'salinity' &
          # average over most relevant subclasses to the Delta
          CODE_NAME %in% c('RIPARIAN_FOREST_POFR', 'RIPARIAN_FOREST_QULO',
                           'RIPARIAN_SCRUB_SALIX')) %>%
      group_by(scorer, FACTOR_TYPE, FACTOR) %>%
      summarize(SCORE = mean(SCORE), .groups = 'drop') %>%
      mutate(CODE_NAME = 'RIPARIAN'),
    # heat tolerance scores for riparian subclasses
    expand_grid(
      ccv_compiled %>%
        filter(CODE_NAME == 'RIPARIAN' & FACTOR == 'heat') %>%
        select(-CODE_NAME),
      ripdat_avg %>% select(CODE_NAME) %>% distinct())
  )
ccv_fill = ccv_fill %>%
  bind_rows(
    ccv_fill %>% filter(CODE_NAME == 'RIPARIAN') %>%
      mutate(CODE_NAME = 'WOODLAND&SCRUB', scorer = 'new'))

# check again for missing:
ccv_fill %>% select(FACTOR, SCORE, CODE_NAME) %>%
  pivot_wider(names_from = FACTOR, values_from = SCORE) %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)


# explore plot
ccv_fill %>%
  filter(!grepl('_FOREST|_SCRUB', CODE_NAME)) %>%
  # mutate(CODE_NAME = factor(CODE_NAME,
  #                           levels = ccv_sum %>%
  #                             filter(FACTOR == 'climate change resilience') %>%
  #                             arrange(SCORE) %>% pull(CODE_NAME))) %>%
  ggplot(aes(SCORE, CODE_NAME)) +
  geom_col(aes(fill = SCORE)) +
  facet_wrap(~FACTOR)

# # rescale--------
# # rescale each factor to range 0-5
# ccv_rescale = ccv_fill %>%
#   group_by(FACTOR) %>%
#   mutate(max = max(SCORE),
#          min = min(SCORE),
#          range = max - min) %>%
#   ungroup() %>%
#   mutate(diff = SCORE - min,
#          diff_perc = diff / range,
#          SCORE_NEW = diff_perc * 3)
#
# # explore plot
# ccv_rescale %>%
#   filter(!grepl('_FOREST|_SCRUB', CODE_NAME)) %>%
#   # mutate(CODE_NAME = factor(CODE_NAME,
#   #                           levels = ccv_sum %>%
#   #                             filter(FACTOR == 'climate change resilience') %>%
#   #                             arrange(SCORE) %>% pull(CODE_NAME))) %>%
#   ggplot(aes(SCORE_NEW, CODE_NAME)) +
#   geom_col(aes(fill = SCORE_NEW), color = 'black') +
#   facet_wrap(~FACTOR)
#
# ggplot(ccv_rescale, aes(SCORE, SCORE_NEW)) + geom_point()

# FINALIZE--------
# add overall average score and metadata

ccv_final = ccv_fill %>%
  select(CODE_NAME, FACTOR, SCORE) %>%
  # bind_rows(
  #   ccv_rescale %>%
  #     group_by(CODE_NAME, FACTOR_TYPE) %>%
  #     summarize(SCORE = mean(SCORE_NEW),
  #               .groups = 'drop') %>%
  #     mutate(FACTOR = 'climate change resilience')) %>%
  mutate(METRIC_CATEGORY = 'climate',
         METRIC_SUBTYPE = 'climate change resilience',
         UNIT = 'ranking') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC = FACTOR, CODE_NAME,
         SCORE, UNIT)
write_csv(ccv_final,
          'data/climate_change_sensitivity.csv')

# explore plot
lc.order = ccv_final %>%
  filter(METRIC_SUBTYPE == 'climate change resilience') %>%
  arrange(SCORE) %>% pull(CODE_NAME) %>% unique()

ccv_final %>%
  filter(!grepl('_FOREST|_SCRUB', CODE_NAME)) %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = lc.order)) %>%
  ggplot(aes(SCORE, CODE_NAME)) +
  geom_col(aes(fill = SCORE), color = 'black') +
  facet_wrap(~METRIC)
