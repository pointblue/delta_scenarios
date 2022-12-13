# CLIMATE CHANGE RESILIENCE
# qualitative indices for sensitivity/resilience to flood, drought, heat
# also considering salinity

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')
key = readxl::read_excel('GIS/VEG_key.xlsx')

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
         # rescale 1-3 to 1-10
         value_rescale = case_when(value == 3 ~ 10,
                                   value == 2 ~ 5,
                                   value == 1 ~ 1),
         FACTOR_TYPE = 'RESILIENCE',
         FACTOR = gsub(' sensitivity| intolerance', '', FACTOR))

## estimate average------
# across scorers, average score for each factor and landcover

ccv_avg = ccv_invert %>%
  group_by(LANDCOVER, FACTOR_TYPE, FACTOR) %>%
  summarize(SCORE_MEAN = mean(value_rescale, na.rm = TRUE),
            # calculate SE on original scale
            SCORE_SE = sd(value, na.rm = TRUE)/sqrt(length(!is.na(value))),
            SCORE_MIN = min(value_rescale, na.rm = TRUE),
            SCORE_MAX = max(value_rescale, na.rm = TRUE),
            .groups = 'drop') %>%
  arrange(LANDCOVER, FACTOR_TYPE, FACTOR)

ccv_avg %>% select(LANDCOVER:SCORE_MEAN) %>%
  pivot_wider(names_from = 'FACTOR', values_from = SCORE_MEAN)

## classify--------
ccv_classify = codify_ccv(ccv_avg) %>%
  filter(LANDCOVER != 'cotton') #is cotton relevant to Delta?

ccv_classify %>% select(CODE_NAME, FACTOR, SCORE_MEAN) %>%
  pivot_wider(names_from = 'FACTOR', values_from = SCORE_MEAN)

write_csv(ccv_classify, 'data_orig/cc_resilience_Peterson.csv')

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
         SCORE_MEAN = c(3, 3, 3, #mod sensitive
                   1, 1,  #sensitive
                   8, 8, 8, 8)), #mod tolerant (assume "grains" includes rice?)
  tibble(scorer = 'medellin',
         FACTOR = 'salinity',
         FACTOR_TYPE = 'RESILIENCE',
         LANDCOVER = c('irrigated pasture', 'dryland pasture'),
         SCORE_MEAN = c(3, 3) #mod sensitive
  ),
  tibble(scorer = 'CVSALTS',
         FACTOR = 'salinity',
         FACTOR_TYPE = 'RESILIENCE',
         LANDCOVER = c('wetland'),
         SCORE_MEAN = 9) #common wetland plants less sensitive than most crops
)

da_classify = da_raw %>%
  mutate(CODE_NAME = case_when(
    LANDCOVER == 'orchard' ~ 'ORCHARD_DECIDUOUS',
    LANDCOVER == 'olives' ~ 'ORCHARD_CITRUS&SUBTROPICAL',
    LANDCOVER == 'vineyard' ~ 'VINEYARD',
    LANDCOVER == 'grain' ~ 'GRAIN&HAY', #assume this includes wheat
    LANDCOVER == 'corn' ~ 'FIELD_CORN',
    LANDCOVER == 'field crops' ~ 'FIELD_OTHER', #is cotton relevant to Delta?
    LANDCOVER == 'truck crops' ~ 'ROW',
    LANDCOVER == 'rice' ~ 'RICE',
    LANDCOVER == 'alfalfa' ~ 'PASTURE_ALFALFA',
    LANDCOVER == 'irrigated pasture' ~ 'PASTURE_OTHER',
    LANDCOVER == 'dryland pasture' ~ 'GRASSLAND',
    LANDCOVER == 'wetland' ~ 'WETLAND_MANAGED'))

# --> missing data for riparian; wetland data filled in from a search -
# references to wetland plants (swamp timothy, watergrass) being less sensitive
# than many crops

write_csv(da_classify, 'data_orig/cc_resilience_supplemental.csv')


## DRERIP conceptual model-----
# sensitivity data for specific riparian species

# riptable = tabulizer::locate_areas('data_orig/DRERIP_Rip_Veg_Model_ Oct08-EMG_FINAL_fmtd3.pdf',
#                                    pages = 37)

# ripdat_raw = tabulizer::extract_tables(
#  'data_orig/DRERIP_Rip_Veg_Model_ Oct08-EMG_FINAL_fmtd3.pdf',
#  pages = 37, area = list(c(100, 24, 1122, 1122)))[[1]] %>%
#  as_tibble() %>%
# rlang::set_names(
#   c('scientific_name', 'common_name', 'growth_form', 'evergreen',
#     'foliage_porosity_summer', 'foliage_porosity_winter', 'shade_tolerance',
#     'root_depth_min_in', 'height_20yrs_ft', 'height_max_ft',
#     'precip_min_in', 'precip_max_in', 'temp_min_F', 'CA_wetl_ind',
#     'flood_tolerance', 'drought_tolerance', 'moisture_use', 'growth_rate',
#     'herbivory_tolerance', 'fruit_seed_period',
#     'repro_spread_rate', 'veg_spread_rate', 'adapted_fines', 'adapted_medium',
#     'adapted_coarse', 'n_fixation', 'fertility_req', 'CN_ratio',
#     'CaCO3_tolerance', 'pH_min', 'pH_max', 'salinity_tolerance', 'impact_IPC',
#     'invasiveness_IPC', 'protection_status', 'comments'))

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
                           value %in% c('Low', 'Low: prefers well\rdrained soils') ~ 4,
                           value == 'Medium' ~ 6,
                           value == 'High/Moderate' ~ 8,
                           value == 'High' ~ 10,
                           value == '' ~ NA_real_),
         scorer = 'ripmod') %>%
  select(scorer, FACTOR_TYPE, FACTOR, CODE_NAME, LANDCOVER = scientific_name,
         SCORE)

write_csv(ripdat_classify, 'data_orig/cc_resilience_riparian_raw.csv')

ripdat_avg = ripdat_classify %>%
  group_by(CODE_NAME, FACTOR_TYPE, FACTOR, scorer) %>%
  summarize(SCORE_MEAN = mean(SCORE, na.rm = TRUE),
            SCORE_SE = sd(SCORE, na.rm = TRUE)/sqrt(length(!is.na(SCORE))),
            SCORE_MIN = min(SCORE, na.rm = TRUE),
            SCORE_MAX = max(SCORE, na.rm = TRUE), .groups = 'drop')

write_csv(ripdat_avg, 'data_orig/cc_resilience_riparian.csv')

# COMPILED-------
ccv_compiled = bind_rows(ccv_classify %>% mutate(scorer = 'Peterson'),
                         da_classify, ripdat_avg) %>%
  select(-LANDCOVER)
write_csv(ccv_compiled, 'data_orig/cc_resilience_all.csv')

# check for missing landcovers
ccv_compiled %>% select(FACTOR, SCORE_MEAN, CODE_NAME) %>%
  pivot_wider(names_from = FACTOR, values_from = SCORE_MEAN) %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)

# fill in missing
# - wheat & grain&hay_other: assume equivalent to grain&hay overall (salinity),
#    and wheat is equivalent to grain&hay_other for all other factors
# - field_other: assume equivalent to row? (except for salinity, which has its own value)
# - wetland_other: assume equivalent to grassland

# - urban: resilient to salinity, mod to drought/heat, low to flood?
# - idle & barren: assume low sensitivity to all/high resilience
# - water, woodland, scrub: IGNORE
# - wetland_tidal: resilient to salinity, flood, mod to drought/heat?

# - riparian: missing general salinity (equivalent to average of subclasses?)
# - riparian subclasses: missing heat (equivalent to overall riparian)



ccv_fill = ccv_compiled %>%
  bind_rows(
    # for those assumed to be equivalent to others
    ccv_compiled %>% filter(CODE_NAME == 'GRAIN&HAY_OTHER') %>%
      mutate(CODE_NAME = 'GRAIN&HAY_WHEAT', scorer = 'assumed'),

    ccv_compiled %>% filter(CODE_NAME == 'GRAIN&HAY') %>% select(-CODE_NAME) %>%
      expand_grid(tibble(CODE_NAME = c('GRAIN&HAY_WHEAT', 'GRAIN&HAY_OTHER'))),

    ccv_compiled %>% filter(CODE_NAME == 'ROW' & FACTOR != 'salinity') %>%
      mutate(CODE_NAME = 'FIELD_OTHER', scorer = 'assumed'),

    ccv_compiled %>% filter(CODE_NAME == 'WETLAND_MANAGED') %>%
      select(-CODE_NAME, -scorer) %>%
      expand_grid(
        tibble(CODE_NAME = c('WETLAND_OTHER', 'WETLAND_MANAGED_PERENNIAL',
                             'WETLAND_MANAGED_SEASONAL'),
               scorer = 'assumed')),

    ccv_compiled %>% filter(CODE_NAME == 'ROW') %>%
      select(FACTOR_TYPE:FACTOR) %>%
      mutate(SCORE_MEAN = 10, SCORE_SE = NA, SCORE_MIN = 10, SCORE_MAX = 10,
             scorer = 'assumed') %>%
      expand_grid(tibble(CODE_NAME = c('IDLE', 'BARREN'))),

    ccv_compiled %>% filter(CODE_NAME == 'ROW') %>%
      select(FACTOR_TYPE:FACTOR) %>%
      mutate(SCORE_MEAN = case_when(FACTOR == 'drought' ~ 5,
                                    FACTOR == 'heat' ~ 5,
                                    FACTOR == 'flood' ~ 1,
                                    FACTOR == 'salinity' ~ 1),
             SCORE_SE = NA, SCORE_MIN = NA, SCORE_MAX = NA,
             scorer = 'assumed') %>%
      expand_grid(tibble(CODE_NAME = c('WOODLAND', 'SCRUB'))),

    ccv_compiled %>% filter(CODE_NAME == 'ROW') %>%
      select(FACTOR_TYPE:SCORE_MEAN) %>%
      mutate(CODE_NAME = 'URBAN', scorer = 'assumed',
             SCORE_MEAN = case_when(FACTOR == 'drought' ~ 5,
                                    FACTOR == 'heat' ~ 5,
                                    FACTOR == 'flood' ~ 1,
                                    FACTOR == 'salinity' ~ 10),
             SCORE_SE = NA,
             SCORE_MIN = SCORE_MEAN,
             SCORE_MAX = SCORE_MEAN),

    ccv_compiled %>% filter(CODE_NAME == 'ROW') %>%
      select(FACTOR_TYPE:SCORE_MEAN) %>%
      mutate(CODE_NAME = 'WETLAND_TIDAL', scorer = 'assumed',
             SCORE_MEAN = case_when(FACTOR == 'drought' ~ 5,
                                    FACTOR == 'heat' ~ 5,
                                    FACTOR == 'flood' ~ 10,
                                    FACTOR == 'salinity' ~ 10),
             SCORE_SE = NA,
             SCORE_MIN = SCORE_MEAN,
             SCORE_MAX = SCORE_MEAN),
    # heat tolerance scores for riparian subclasses
    expand_grid(
      ccv_compiled %>%
        filter(CODE_NAME == 'RIPARIAN' & FACTOR == 'heat') %>%
        select(-CODE_NAME),
      ripdat_avg %>% select(CODE_NAME) %>% distinct()
      ),
    # roll up overall salinity tolerance scores for riparian?
    tibble(CODE_NAME = 'RIPARIAN',
           FACTOR_TYPE = 'RESILIENCE',
           FACTOR = 'salinity',
           SCORE_MEAN = 4,
           scorer = 'ripmod')
    ) %>%
  filter(CODE_NAME != 'GRAIN&HAY')

# check again for missing:
ccv_fill %>% select(FACTOR, SCORE_MEAN, CODE_NAME) %>%
  pivot_wider(names_from = FACTOR, values_from = SCORE_MEAN) %>%
  mutate(CODE_NAME = factor(CODE_NAME, levels = key %>% pull(CODE_NAME))) %>%
  complete(CODE_NAME) %>% print(n = 40)

# explore plot
ccv_fill %>%
  filter(!grepl('_FOREST|_SCRUB', CODE_NAME)) %>%
  ggplot(aes(SCORE_MEAN, CODE_NAME, xmin = SCORE_MEAN-SCORE_SE, xmax = SCORE_MEAN + SCORE_SE)) +
  geom_col(aes(fill = SCORE_MEAN)) + geom_errorbar() +
  facet_wrap(~FACTOR)
ccv_fill %>%
  filter(grepl('RIPARIAN', CODE_NAME)) %>%
  ggplot(aes(SCORE_MEAN, CODE_NAME, xmin = SCORE_MEAN-SCORE_SE, xmax = SCORE_MEAN + SCORE_SE)) +
  geom_col(aes(fill = SCORE_MEAN)) + geom_errorbar() +
  facet_wrap(~FACTOR)

# FINALIZE--------
# add metadata

ccv_final = ccv_fill %>%
  # bind_rows(ccv_fill %>%
  #             # just to make the following calculations simpler
  #             mutate(SCORE_SE = replace_na(SCORE_SE, replace = 0),
  #                    SCORE_SE_SQ = SCORE_SE^2) %>%
  #             group_by(CODE_NAME, FACTOR_TYPE) %>%
  #             summarize(SCORE_MEAN = mean(SCORE_MEAN),
  #                       n = length(SCORE_SE),
  #                       SCORE_SE = sqrt(sum(SCORE_SE_SQ))/(n - 1), #don't count salinity in the mean SE
  #                       .groups = 'drop') %>%
  #             mutate(FACTOR = 'Overall') %>%
  #             select(-n)) %>%
  mutate(METRIC_CATEGORY = 'climate',
         METRIC_SUBTYPE = 'climate change resilience',
         UNIT = 'ranking') %>%
  select(METRIC_CATEGORY, METRIC_SUBTYPE, METRIC = FACTOR, CODE_NAME,
         SCORE_MEAN, SCORE_SE, SCORE_MIN, SCORE_MAX, UNIT) %>%
  mutate(METRIC = recode(METRIC, drought = 'Drought', flood = 'Flood',
                         heat = 'Heat', salinity = 'Salinity'))
write_csv(ccv_final,
          'data/climate_change_resilience.csv')
