# README--------
# From spatial metrics compiled for riparian landbird and waterbird distribution
# models, generate predictions of probability of presence across the Delta

# PACKAGES & FUNCTIONS
source('R/packages.R')
source('R/functions.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
delta_buff10k = read_sf('GIS/boundaries/Legal_Delta_boundary_buff10k.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
delta_buff10k = rasterize(vect(delta_buff10k), extend(delta, delta_buff10k))
baseline = rast('GIS/landscape_rasters/veg_baseline.tif')
watermask = baseline
watermask[watermask == 90] = NA
county_raster = rast('GIS/landscape_rasters/counties.tif') #see code below to create

palette = c("#2b83ba", "#80bfab", "#c7e8ad", "#ffffbf", "#fdc980", "#f07c4a",
            "#d7191c")
memory.limit(size = 48000)

# COUNTIES & CENSUS TRACTS---------
ct = read_sf('GIS/original_source_data/tl_2020_06_tract/tl_2020_06_tract.shp') %>%
  st_transform(crs = crs(delta))
ct_delta = st_intersection(ct,
                           st_read('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
                             st_transform(crs = crs(delta))) %>%
  mutate(COUNTY_NAME = case_when(COUNTYFP == '001' ~ 'Alameda',
                                 COUNTYFP == '077' ~ 'San Joaquin',
                                 COUNTYFP == '067' ~ 'Sacramento',
                                 COUNTYFP == '113' ~ 'Yolo',
                                 COUNTYFP == '095' ~ 'Solano',
                                 COUNTYFP == '013' ~ 'Contra Costa')) %>%
  filter(COUNTYFP != '099') #remove sliver of Stanislaus
ct_delta %>% select(COUNTY_NAME) %>% plot()
county_delta = ct_delta %>% group_by(COUNTYFP, COUNTY_NAME) %>%
  summarize(.groups = 'drop')
write_sf(county_delta, 'GIS/original_source_data/county_boundaries_simplified.shp')
# county_simple = county_delta %>%
#   mutate(COUNTY_NAME = if_else(COUNTY_NAME %in% c('Alameda', 'Contra Costa'),
#                                'Contra Costa & Alameda', COUNTY_NAME)) %>%
#   group_by(COUNTY_NAME) %>% summarize(.groups = 'drop')
# county_raster = rasterize(vect(county_simple), delta, field = 'COUNTY_NAME')
# writeRaster(county_raster, 'GIS/landscape_rasters/counties.tif')

# RIPARIAN---------
# apply top species model to each set of predictors for each scenario
# --> requires gbm package loaded
load('data/riparian_landbirds/BRT_models_final_v2_simplified.RData')
thresholds = read_csv('data/riparian_landbirds/model_thresholds_v2_delta_simplified.csv')
thresholds_cv = read_csv('data/riparian_landbirds/model_thresholds_v2_simplified.csv')

## compile predictor stacks-------
# for each scenario, stack of rasters corresponding to model predictors (names
# must match)
# - all scenarios include the two scenario-independent predictors (bio_1,
# streamdist) plus the scenario-specific landcover rasters

stacklist = list(
  'baseline' = c(
    list.files('GIS/landscape_rasters/riparian_predstack', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/riparian_predstack/baseline', '.tif$',
               full.names = TRUE) %>% rast()),
  'scenario1' = c(
    list.files('GIS/landscape_rasters/riparian_predstack', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/riparian_predstack/scenario1', '.tif$',
               full.names = TRUE) %>% rast()),
  'scenario2' = c(
    list.files('GIS/landscape_rasters/riparian_predstack', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/riparian_predstack/scenario2', '.tif$',
               full.names = TRUE) %>% rast()))

# --> double-check that there aren't any missing data in these layers; e.g. all
# landcover data should be filled with zero if there isn't any (rather than NA)
c(stacklist$baseline$POFR_2000,
  stacklist$scenario1$POFR_2000,
  stacklist$scenario2$POFR_2000) %>% plot(colNA = 'black')
c(stacklist$baseline$shape,
  stacklist$scenario1$shape,
  stacklist$scenario2$shape) %>% plot(colNA = 'black')

## fit distribution models---------
load('data/riparian_landbirds/BRT_models_final_v2_simplified.RData')
# for each scenario and species, input scenario-specific predictors to original
# distribution models to generate predicted distributions

combos = expand_grid(scenario = c('baseline', 'scenario1', 'scenario2'),
                     species = names(BRT_topmodels_simplified))

# NOTE: This step takes hours!
purrr::map2(
  combos$species,
  combos$scenario,
  ~terra::predict(
    model = BRT_topmodels_simplified[[.x]],
    object = stacklist[[.y]],
    n.trees = BRT_topmodels_simplified[[.x]]$n.trees,
    const = data.frame(region = 1,
                       area.ha = 3.141593),
    type = 'response',
    filename = paste0('GIS/prediction_rasters/riparian/', .y, '/raw/', .x, '.tif')
    )
  )

## refine results--------
preds$baseline = mask(preds$baseline, delta)
plot(preds$baseline, col = palette, colNA = 'black')

preds$scenario1 = mask(preds$scenario1, delta)
plot(preds$scenario1, col = palette, colNA = 'black')

preds$scenario2 = mask(preds$scenario2, delta)
plot(preds$scenario2, col = palette, colNA = 'black')

plot(c(preds$baseline$SPTO, preds$scenario2$SPTO), col = palette)

writeRaster(preds$baseline,
            paste0('GIS/prediction_rasters/riparian/baseline/',
                   names(preds$baseline), '.tif'))
writeRaster(preds$scenario1,
            paste0('GIS/prediction_rasters/riparian/scenario1/',
                   names(preds$scenario1), '.tif'))
writeRaster(preds$scenario2,
            paste0('GIS/prediction_rasters/riparian/scenario2/',
                   names(preds$scenario2), '.tif'))


# WATERBIRDS FALL---------

## compile predictor stacks-----------
# TRY: one stacklist for all scales? (models should pull the correct ones, since
# the scale is in the variable name)

stacklist = list(
  'baseline' = c(
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/baseline', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/baseline/buffer2k', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/baseline/buffer5k', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/baseline/buffer10k', '.tif$',
               full.names = TRUE) %>% rast()),
  'scenario1' = c(
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/scenario1', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/scenario1/buffer2k', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/scenario1/buffer5k', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/scenario1/buffer10k', '.tif$',
               full.names = TRUE) %>% rast()),
  'scenario2' = c(
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/scenario2', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/scenario2/buffer2k', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/scenario2/buffer5k', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_fall_predstack/scenario2/buffer10k', '.tif$',
               full.names = TRUE) %>% rast()))

# --> double-check that there aren't any missing data in these layers; e.g. all
# landcover data should be filled with zero if there isn't any (rather than NA)
c(stacklist$baseline$orch_area_2k,
  stacklist$scenario1$orch_area_2k,
  stacklist$scenario2$orch_area_2k) %>% plot(colNA = 'black')
c(stacklist$baseline$covertype,
  stacklist$scenario1$covertype,
  stacklist$scenario2$covertype) %>% plot(colNA = 'black')

## fit distribution models---------
# for each scenario and species, input scenario-specific predictors to original
# distribution models to generate predicted distributions
load('data/waterbirds/BRT_models_final.RData')

combos = tibble(species = names(waterbird_mods_fall),
                scenario = 'baseline') %>%
  mutate(scenario = factor(scenario, levels = c('baseline', 'scenario1', 'scenario2'))) %>%
  expand(species, scenario) %>%
  mutate(offset = case_when(species %in% c('crane', 'geese') ~ 3.709,
                            TRUE ~ 4.435))

combos = combos %>% filter(scenario != 'baseline') %>%
  arrange(scenario)
# NOTE: This step takes hours!

purrr::map(
  c(1:nrow(combos)),
  ~terra::predict(
    model = waterbird_mods_fall[[combos$species[.x]]],
    object = stacklist[[combos$scenario[.x]]],
    n.trees = waterbird_mods_fall[[combos$species[.x]]]$n.trees,
    const = data.frame(offset = combos$offset[.x]),
    factors = list(list('covertype' = c('Alfalfa',
                                        'Irrigated pasture',
                                        'Rice',
                                        'Wetland'))),
    type = 'response',
    filename = paste0('GIS/prediction_rasters/waterbirds_fall/',
                      combos$scenario[.x], '/raw/', combos$species[.x], '.tif'),
    wopt = list(names = combos$species[.x]),
    overwrite = TRUE))



## refine results-------

# mask out unpredictable/unsuitable cover types
covertype = c(stacklist$baseline$covertype,
              stacklist$scenario1$covertype,
              stacklist$scenario2$covertype)
names(covertype) = c('baseline', 'scenario1', 'scenario2')
include = subst(covertype, from = c(98:99), to = NA)
dev = subst(covertype, from = c(1:98), to = NA)

preds = list(
  'baseline' = list.files('GIS/prediction_rasters/waterbirds_fall/baseline/raw', '.tif$',
                          full.names = TRUE) %>% rast(),
  'scenario1' = list.files('GIS/prediction_rasters/waterbirds_fall/scenario1/raw', '.tif$',
                           full.names = TRUE) %>% rast(),
  'scenario2' = list.files('GIS/prediction_rasters/waterbirds_fall/scenario2/raw', '.tif$',
                           full.names = TRUE) %>% rast()
)
names(preds$baseline) = list.files('GIS/prediction_rasters/waterbirds_fall/baseline/raw', '.tif$') %>% gsub('.tif', '', .)
names(preds$scenario1) = list.files('GIS/prediction_rasters/waterbirds_fall/scenario1/raw', '.tif$') %>% gsub('.tif', '', .)
names(preds$scenario2) = list.files('GIS/prediction_rasters/waterbirds_fall/scenario2/raw', '.tif$') %>% gsub('.tif', '', .)

preds$baseline = mask(preds$baseline, include$baseline) %>%
  mask(dev$baseline, updatevalue = 0, inverse = TRUE) %>%
  mask(delta)
plot(preds$baseline, col = palette, colNA = 'black')

preds$scenario1 = mask(preds$scenario1, include$scenario1) %>%
  mask(dev$scenario1, updatevalue = 0, inverse = TRUE) %>%
  mask(delta)
plot(preds$scenario1, col = palette, colNA = 'black')

preds$scenario2 = mask(preds$scenario2, include$scenario2) %>%
  mask(dev$scenario2, updatevalue = 0, inverse = TRUE) %>%
  mask(delta)
plot(preds$scenario2, col = palette, colNA = 'black')

# # compare to earliest predictions: (baseline only, from Task 3 report)
# report = list(
#   'baseline' = list.files(
#     'Z:/Terrestrial/programs_and_projects/delta_multiple_benefits/delta-waterbirds/output/predmaps_fall',
#     '.tif$', full.names = TRUE) %>% rast()
# )

writeRaster(preds$baseline,
            paste0('GIS/prediction_rasters/waterbirds_fall/baseline/',
                   names(preds$baseline), '.tif'))
writeRaster(preds$scenario1,
            paste0('GIS/prediction_rasters/waterbirds_fall/scenario1/',
                   names(preds$scenario1), '.tif'))
writeRaster(preds$scenario2,
            paste0('GIS/prediction_rasters/waterbirds_fall/scenario2/',
                   names(preds$scenario2), '.tif'))




# WATERBIRDS WINTER-------

## compile predictor stacks-----------
# TRY: one stacklist for all scales? (models should pull the correct ones, since
# the scale is in the variable name)

stacklist = list(
  'baseline' = c(
    list.files('GIS/landscape_rasters/waterbirds_win_predstack/baseline', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_win_predstack/baseline/buffer5k', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_win_predstack/baseline/buffer10k', '.tif$',
               full.names = TRUE) %>% rast()),
  'scenario1' = c(
    list.files('GIS/landscape_rasters/waterbirds_win_predstack/scenario1', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_win_predstack/scenario1/buffer5k', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_win_predstack/scenario1/buffer10k', '.tif$',
               full.names = TRUE) %>% rast()),
  'scenario2' = c(
    list.files('GIS/landscape_rasters/waterbirds_win_predstack/scenario2', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_win_predstack/scenario2/buffer5k', '.tif$',
               full.names = TRUE) %>% rast(),
    list.files('GIS/landscape_rasters/waterbirds_win_predstack/scenario2/buffer10k', '.tif$',
               full.names = TRUE) %>% rast()))

# --> double-check that there aren't any missing data in these layers; e.g. all
# landcover data should be filled with zero if there isn't any (rather than NA)
c(stacklist$baseline$orch_area_5k,
  stacklist$scenario1$orch_area_5k,
  stacklist$scenario2$orch_area_5k) %>% plot(colNA = 'black')

## fit distribution models---------
# for each scenario and species, input scenario-specific predictors to original
# distribution models to generate predicted distributions
load('data/waterbirds/BRT_models_final.RData')

combos = tibble(species = names(waterbird_mods_win),
                scenario = 'baseline') %>%
  mutate(scenario = factor(scenario, levels = c('baseline', 'scenario1', 'scenario2'))) %>%
  expand(species, scenario)
combos = combos %>% filter(scenario != 'baseline') %>% arrange(scenario)

# NOTE: This step takes hours!
purrr::map2(
  combos$species,
  combos$scenario,
  ~terra::predict(
    model = waterbird_mods_win[[.x]],
    object = stacklist[[.y]],
    n.trees = waterbird_mods_win[[.x]]$n.trees,
    const = data.frame(offset = 3.617),
    factors = list(list('covertype' = c('Alfalfa',
                                        'Corn',
                                        'Irrigated pasture',
                                        'Rice',
                                        'Wetland',
                                        'Winter wheat'))),
    type = 'response',
    filename = paste0('GIS/prediction_rasters/waterbirds_win/', .y, '/raw/', .x, '.tif'),
    overwrite = TRUE))

## refine results-------

# mask out unpredictable/unsuitable cover types
covertype = c(stacklist$baseline$covertype,
              stacklist$scenario1$covertype,
              stacklist$scenario2$covertype)
names(covertype) = c('baseline', 'scenario1', 'scenario2')
include = subst(covertype, from = c(98:99), to = NA)
dev = subst(covertype, from = c(1:98), to = NA)

preds = list(
  'baseline' = list.files('GIS/prediction_rasters/waterbirds_win/baseline/raw', '.tif$',
                          full.names = TRUE) %>% rast(),
  'scenario1' = list.files('GIS/prediction_rasters/waterbirds_win/scenario1/raw', '.tif$',
                           full.names = TRUE) %>% rast(),
  'scenario2' = list.files('GIS/prediction_rasters/waterbirds_win/scenario2/raw', '.tif$',
                           full.names = TRUE) %>% rast()
)
names(preds$baseline) = list.files('GIS/prediction_rasters/waterbirds_win/baseline/raw', '.tif$') %>% gsub('.tif', '', .)
names(preds$scenario1) = list.files('GIS/prediction_rasters/waterbirds_win/scenario1/raw', '.tif$') %>% gsub('.tif', '', .)
names(preds$scenario2) = list.files('GIS/prediction_rasters/waterbirds_win/scenario2/raw', '.tif$') %>% gsub('.tif', '', .)

preds$baseline = mask(preds$baseline, include$baseline) %>%
  mask(dev$baseline, updatevalue = 0, inverse = TRUE) %>%
  mask(delta)
plot(preds$baseline, col = palette, colNA = 'black')

preds$scenario1 = mask(preds$scenario1, include$scenario1) %>%
  mask(dev$scenario1, updatevalue = 0, inverse = TRUE) %>%
  mask(delta)
plot(preds$scenario1, col = palette, colNA = 'black')

preds$scenario2 = mask(preds$scenario2, include$scenario2) %>%
  mask(dev$scenario2, updatevalue = 0, inverse = TRUE) %>%
  mask(delta)
plot(preds$scenario2, col = palette, colNA = 'black')

writeRaster(preds$baseline,
            paste0('GIS/prediction_rasters/waterbirds_win/baseline/',
                   names(preds$baseline), '.tif'))
writeRaster(preds$scenario1,
            paste0('GIS/prediction_rasters/waterbirds_win/scenario1/',
                   names(preds$scenario1), '.tif'))
writeRaster(preds$scenario2,
            paste0('GIS/prediction_rasters/waterbirds_win/scenario2/',
                   names(preds$scenario2), '.tif'))

