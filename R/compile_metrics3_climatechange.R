# CLIMATE CHANGE SENSITIVITY INDICES
# including expert opinion rankings for climate change sensitivity from MBCP
# multiple benefits synthesis report (including management rigidity, drought,
# flood intolerance, temperature) and salt tolerance (from Delta Adapts)

# [also have rankings for climate change exposure (including pest & disease
# impacts, market volatility/pollution, land use/land cover change, and capacity
# gap) - but these are more generic/less specific to the Delta]

# PACKAGES & FUNCTIONS
source('R/packages.R')

# compile raw data----------
# Scores for each factor within a land cover were 1, 2, or 3 (also N/A or 0=no
# answer), with 1 being the lowest sensitivity/exposure to the given factor and
# 3 being the highest sensitivity/exposure.

## MBCP---------
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
                         temp = 'temp')) %>%
  filter(!is.na(value))

## Delta Adapts-----
# and other relevant Delta-specific literature:
# - ag production & crop yield tech memo, p2-8
# (https://deltacouncil.ca.gov/pdf/delta-plan/2021-06-07-crop-yield-and-agricultural-production-technical-memorandum.pdf)
# - Medellin-Azuara et al. 2014 (https://escholarship.org/uc/item/4b7295m9#main)

da_raw = bind_rows(
  tibble(scorer = 'DeltaAdapts',
         FACTOR = 'salinity sensitivity',
         FACTOR_TYPE = 'SENSITIVITY',
         LANDCOVER = c('truck crops', 'corn', 'alfalfa',
                       'vineyard', 'orchard',
                       'grain', 'rice', 'field crops', 'olives'),
         value = c(2, 2, 2, #mod sensitive
                   3, 3,  #sensitive
                   1, 1, 1, 1)), #mod tolerant (assume "grains" includes rice?)
  tibble(scorer = 'medellin',
         FACTOR = 'salinity sensitivity',
         FACTOR_TYPE = 'SENSITIVITY',
         LANDCOVER = c('irrigated pasture', 'dryland pasture'),
         value = c(2, 2) #mod sensitive
         ),
  tibble(scorer = 'CVSALTS',
         FACTOR = 'salinity sensitivity',
         FACTOR_TYPE = 'SENSITIVITY',
         LANDCOVER = c('wetland'),
         value = 1) #common wetland plants less sensitive than most crops
)

# --> missing data for riparian; wetland data filled in from a search -
# references to wetland plants (swamp timothy, watergrass) being less sensitive
# than many crops

## riparian conceptual model-----
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
                         'drought_tolerance' = 'drought sensitivity',
                         'flood_tolerance' = 'flood intolerance',
                         'salinity_tolerance' = 'salinity sensitivity'),
         FACTOR_TYPE = 'SENSITIVITY',
         # reverse coding so "high" tolerance becomes low score for sensitivity;
         # but also rescale relative to crops - high sensitivity for riparian
         # veg may be relatively low; ensure overall scores line up with expert
         # opinion scores for RIPARIAN in general
         value = case_when(value %in% c('None', 'Low/None') ~ 2,
                           value %in% c('Low', 'Low: prefers well\rdrained soils') ~ 1.75,
                           value == 'Medium' ~ 1.5,
                           value == 'High/Moderate' ~ 1.25,
                           value == 'High' ~ 1,
                           value == '' ~ NA_real_),
         scorer = 'ripmod') %>%
  select(scorer, FACTOR_TYPE, FACTOR, CODE_NAME, LANDCOVER = scientific_name,
         value)

write_csv(ripdat_classify, 'data_orig/riparian_sensitivity.csv')

# prop_riptype = baseline_ripdetail %>% mask(delta) %>% freq() %>%
#   as_tibble() %>%
#   mutate(area_ha = count * 30 * 30 /10000,
#          prop = area_ha/sum(area_ha)) %>%
#   arrange(desc(prop))
#
# # calculated weighted sensitivity scores
# rip_sensitivity = left_join(riptype_sensitivity, prop_riptype %>% select(label, prop),
#                             by = c('riptype' = 'label')) %>%
#   pivot_longer(salt_sensitivity:flood_sensitivity, names_to = 'FACTOR',
#                values_to = 'SCORE') %>%
#   group_by(FACTOR) %>%
#   summarize(SCORE = sum(SCORE * prop),
#             LANDCOVER = 'riparian',
#             FACTOR_TYPE = 'S',
#             SCORER = 'Fremier et al. 2008')
#
# ccv_means %>% filter(LANDCOVER == 'riparian')
# # drought & flood scores match fairly well survey data



ccv_compiled = bind_rows(ccv_raw, da_raw, ripdat_classify)

# classify--------
ccv_classify = ccv_compiled %>%
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
    LANDCOVER %in% c('wetl', 'wetland') ~ 'WETLAND',
    TRUE ~ CODE_NAME)) %>%
  filter(LANDCOVER != 'cotton')

# fill in missing------
# treat missing land cover classes as equivalent to others
# field crops equivalent to corn; wheat equivalent to grain&hay;
# temp sensitivity scores for RIPARIAN subclasses equivalent to overall RIPARIAN

ccv_fill = ccv_classify %>%
  bind_rows(
    ccv_classify %>%
      filter((CODE_NAME == 'FIELD_CORN' & FACTOR != 'salinity') |
               (CODE_NAME == 'GRAIN&HAY')) %>%
      mutate(CODE_NAME = recode(CODE_NAME,
                                'FIELD_CORN' = 'FIELD',
                                'GRAIN&HAY' = 'GRAIN&HAY_WHEAT'))
  ) %>%
  # temp sensitivity scores for riparian subclasses
  bind_rows(
    expand_grid(
      ccv_classify %>%
        filter(CODE_NAME == 'RIPARIAN' & FACTOR == 'temp') %>%
        select(-CODE_NAME),
      ripdat_classify %>% select(CODE_NAME) %>% distinct()
    )
  ) %>%
  # overall salinity sensitivity scores for riparian
  bind_rows(
    ripdat_classify %>%
      filter(
        FACTOR == 'salinity sensitivity' &
          # average over most relevant subclasses to the Delta
        CODE_NAME %in% c('RIPARIAN_FOREST_POFR', 'RIPARIAN_FOREST_QULO',
                         'RIPARIAN_SCRUB_SALIX')) %>%
      group_by(scorer, FACTOR_TYPE, FACTOR) %>%
      summarize(value = mean(value), .groups = 'drop') %>%
      mutate(CODE_NAME = 'RIPARIAN')
  )

# estimate average---------
# across scorers, average score for each factor and landcover

ccv_sum = ccv_fill %>%
  filter(FACTOR != 'mgmt') %>%
  group_by(CODE_NAME, FACTOR_TYPE, FACTOR) %>%
  summarize(SCORE = mean(value, na.rm = TRUE),
            SCORE_MIN = min(value, na.rm = TRUE),
            SCORE_MAX = max(value, na.rm = TRUE),
            .groups = 'drop') %>%
  bind_rows(
    ccv_fill %>%
      filter(FACTOR != 'mgmt') %>%
      group_by(CODE_NAME, FACTOR_TYPE, FACTOR) %>%
      summarize(SCORE = mean(value, na.rm = TRUE),
                SCORE_MIN = min(value, na.rm = TRUE),
                SCORE_MAX = max(value, na.rm = TRUE),
                .groups = 'drop') %>%
      group_by(CODE_NAME, FACTOR_TYPE) %>%
      summarize(SCORE_MIN = min(SCORE),
                SCORE_MAX = max(SCORE),
                SCORE = mean(SCORE),
                .groups = 'drop') %>%
      ungroup() %>%
      mutate(FACTOR = if_else(FACTOR_TYPE == 'EXPOSURE', 'exposure_all',
                              'sensitivity_all'))
  ) %>%
  arrange(CODE_NAME, FACTOR_TYPE, FACTOR)

# explore plot
ccv_sum %>%
  filter(FACTOR_TYPE == 'EXPOSURE') %>%
  filter(!grepl('_FOREST|_SCRUB', CODE_NAME)) %>%
  mutate(CODE_NAME = factor(CODE_NAME,
                            levels = ccv_sum %>%
                              filter(FACTOR == 'sensitivity_all') %>%
                              arrange(SCORE) %>% pull(CODE_NAME))) %>%
  ggplot(aes(SCORE, CODE_NAME)) +
  geom_col(aes(fill = SCORE)) +
  facet_wrap(~FACTOR)

# FINALIZE--------
ccv_final = ccv_sum %>%
  filter(FACTOR_TYPE == 'SENSITIVITY') %>%
  mutate(METRIC_CATEGORY = 'climate',
         METRIC_SUBTYPE = 'climate change sensitivity',
         UNIT = 'ranking') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC = FACTOR, CODE_NAME,
         SCORE, SCORE_MIN, SCORE_MAX, UNIT)
write_csv(ccv_final,
          'data/multiplebenefits/climate_change_sensitivity.csv')
