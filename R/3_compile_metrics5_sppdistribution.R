# All distribution models related the probability of species presence to the
# extent and configuration of land covers in the surrounding landscape, as well
# as additional taxon- or guild-specific predictors. To apply these models to
# the scenarios of landscape change, we first analyzed each landscape to
# generate the necessary predictors. All models required summaries of the
# proportion or total area of land cover classes within a range of distances
# from each pixel in the Delta: proportion cover within 50m and 2km for riparian
# landbirds, and total area within 2km, 5km, and 10km for waterbirds. Thus, we
# generated estimates of the total area of each land cover class within all 4
# buffer distances for all six landscapes, then aggregated land cover types
# according to the different classification schemes used by each set of models
# (Dybala et al. 2021) and converted to proportion cover for the riparian
# landbird models.

# PYTHON focal stats: calculate total area within 50m, 2km, 5km, 10km
# aggregate to classification schemes
# riparian: convert to prop cover


# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

# reference data:
key = readxl::read_excel('GIS/VEG_key.xlsx')
scenarios = list.files('GIS/scenario_rasters', '.tif$', full.names = TRUE) %>%
  terra::rast()

# BASIC PREDICTORS-----------
# predictors not requiring focal stats with a moving window
#
# riparian: bio1, bio12, streamdist, region, area.ha --> all are unchanged from
#   original
# waterbird: covertype, pwater, droost_km, offset --> covertype, pwater, and
#   droost need to be updated for each scenario; have to update pwater first
#   because it will influence pfld predictors

## covertype--------
# categorical var representing the covertype at the waterbird survey point/pixel

# waterbird_fall:
purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::update_covertype(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             SDM = 'waterbird_fall',
             maskpath = 'GIS/boundaries/delta.tif',
             pathout = 'GIS/landscape_rasters/predictors_waterbird_fall',
             overwrite = TRUE))

# waterbird_win:
purrr::map(names(scenarios)[which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::update_covertype(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             SDM = 'waterbird_win',
             maskpath = 'GIS/boundaries/delta.tif',
             pathout = 'GIS/landscape_rasters/predictors_waterbird_win',
             overwrite = TRUE))

## pwater----------

# waterbird_fall:
purrr::pmap(
  list(
    scenario_landscape = list(NULL, scenarios$scenario1_restoration,
                              scenarios$scenario2_perennialexpand),
    scenario_name = names(scenarios)[-which(grepl('win', names(scenarios)))],
    floor = c(FALSE, TRUE, FALSE)),
  DeltaMultipleBenefits::update_pwater,
  landscape = scenarios$baseline,
  waterdatpath = 'GIS/landscape_rasters/pwater/water_fall_mean.tif',
  maskpath = 'GIS/boundaries/delta.tif',
  pathout = c('GIS/landscape_rasters/pwater',
              'GIS/landscape_rasters/predictors_waterbird_fall'),
  overwrite = TRUE)

# waterbird_win:
purrr::pmap(
  list(scenario_landscape = list(NULL, scenarios$scenario1_restoration_win,
                                 scenarios$scenario2_perennialexpand_win),
       scenario_name = names(scenarios)[which(grepl('win', names(scenarios)))],
       floor = c(FALSE, TRUE, FALSE)),
  DeltaMultipleBenefits::update_pwater,
  landscape = scenarios$baseline_win,
  waterdatpath = 'GIS/landscape_rasters/pwater/water_win_mean.tif',
  maskpath = 'GIS/boundaries/delta.tif',
  pathout = c('GIS/landscape_rasters/pwater',
              'GIS/landscape_rasters/predictors_waterbird_WIN'),
  overwrite = TRUE)


## droost----------
# unnecessary to repeat for each season since "unsuitable" land cover classes
# aren't dynamic and changing seasonally
purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::update_roosts(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             unsuitable = c(11:19, 60, 70:79, 100:120),
             proportion = 0.2,
             roostpath = 'GIS/original_source_data/Ivey/Select_recent_roosts_Ivey_utm.shp',
             pathout = 'GIS/landscape_rasters/crane_roosts',
             overwrite = TRUE
           ))

# use python to calculate distance to roost from every pixel in each landscape
purrr::pmap(
  list(landscape_name = names(scenarios)[-which(grepl('win', names(scenarios)))],
       copyto = names(scenarios)[which(grepl('win', names(scenarios)))]),
  DeltaMultipleBenefits::python_dist,
  pathin = 'GIS/landscape_rasters/crane_roosts',
  pathout = c('GIS/landscape_rasters/predictors_waterbird_fall',
              'GIS/landscape_rasters/predictors_waterbird_win'),
  maskpath = 'GIS/boundaries/delta.tif',
  overwrite = TRUE)


# FOCAL STATS---------
# summarizing landcover stats within a moving window surrounding each focal
# pixel; multiple window sizes needed for riparian and waterbird SDMs

## prep rasters----------
# first prep for python focal statistics calculations by splitting raster stacks
# for each scenario, with one layer for each land cover class
# -for waterbirds:
# -- to support area calculations: fill pixel with 0.09 (area of each pixel) if
# land cover is present, and 0 otherwise (python will later sum # of pixels by
# buffer size)
# -- to support pfld calculations: further mask land cover presence above by
# pwater layer (python will calculate mean of pixels by buffer size)

# riparian
purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::python_focal_prep(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             SDM = 'riparian',
             pathout = 'GIS/landscape_rasters/cover',
             overwrite = TRUE))

# waterbird_fall
pwater_list = list.files('GIS/landscape_rasters/pwater', 'pwater.tif',
                         recursive = TRUE, full.names = TRUE)
pwater_fall =  pwater_list[-which(grepl('_win', pwater_list))] %>% rast()
names(pwater_fall) = pwater_list[-which(grepl('_win', pwater_list))] %>%
  gsub('GIS/landscape_rasters/pwater/|/pwater.tif', '', .)

purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::python_focal_prep(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             SDM = 'waterbird_fall',
             mask = pwater_fall[[.x]],
             pixel_value = 0.09,
             pathout = 'GIS/landscape_rasters/cover',
             suffix = c('_area', '_pfld'),
             overwrite = TRUE))

# waterbird_win
pwater_win =  pwater_list[which(grepl('_win', pwater_list))] %>% rast()
names(pwater_win) = pwater_list[which(grepl('_win', pwater_list))] %>%
  gsub('GIS/landscape_rasters/pwater/|/pwater.tif', '', .)

purrr::map(names(scenarios)[which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::python_focal_prep(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             SDM = 'waterbird_win',
             mask = pwater_win[[.x]],
             pixel_value = 0.09,
             pathout = 'GIS/landscape_rasters/cover',
             suffix = c('_area', '_pfld'),
             overwrite = TRUE))


## run Python focal stats------------
# for faster compilation of focal stats, use python and arcpy (requires python
# installation, ArcGIS license, and Spatial Analyst extension)
# --> may function better if running RStudio as administrator?

# library(reticulate)
# arcpy <- reticulate::import('arcpy')
# arcpy$CheckOutExtension("Spatial")
# reticulate::source_python('R/0_focal_stats.py')

combos_python = bind_rows(
  expand_grid(SDM = 'riparian',
              landscape_name = names(scenarios)[-which(grepl('win', names(scenarios)))],
              scale = c('50', '2000')),
  expand_grid(SDM = 'waterbird_fall',
              landscape_name = names(scenarios)[-which(grepl('win', names(scenarios)))],
              scale = c('2000', '5000', '10000')),
  expand_grid(SDM = 'waterbird_win',
              landscape_name = names(scenarios)[which(grepl('win', names(scenarios)))],
              scale = c('5000', '10000')))

# --> NOTE: this may hours to run, slower for larger buffer sizes, and will
# generate a lot of raster files on disk; try smaller chunks by scenario
# --> ALSO NOTE: this will not overwrite existing files, so need to move old
# versions to a new location or change 'pathout'

# riparian: total pixels within buffer
combos_python %>% filter(SDM == 'riparian') %>%
  purrr::pmap(DeltaMultipleBenefits::python_focal_run,
              pathin = 'GIS/landscape_rasters/cover',
              pathout = 'GIS/landscape_rasters/focal_stats',
              fun = 'SUM')

# waterbirds: fall and winter, total area within buffer, requires regex
combos_python %>% filter(SDM != 'riparian') %>%
  purrr::pmap(DeltaMultipleBenefits::python_focal_run,
              pathin = 'GIS/landscape_rasters/cover',
              pathout = 'GIS/landscape_rasters/focal_stats',
              regex = '*_area.tif', fun = 'SUM')

# waterbirds: fall and winter, mean pfld within buffer, requires regex
combos_python %>% filter(SDM != 'riparian') %>%
  purrr::pmap(DeltaMultipleBenefits::python_focal_run,
              pathin = 'GIS/landscape_rasters/cover',
              pathout = 'GIS/landscape_rasters/focal_stats',
              regex = '*_pfld.tif', fun = 'MEAN')

## finalize predictors----------
# convert riparian pixel counts to proportion area; fix predictor names to match
# inputs expected by SDMs; mask by study area boundary

combos_finalize = combos_python %>%
  mutate(
    pathout = case_when(
      SDM == 'riparian' ~ 'GIS/landscape_rasters/predictors_riparian',
      SDM == 'waterbird_fall' ~ 'GIS/landscape_rasters/predictors_waterbird_fall',
      SDM == 'waterbird_win' ~ 'GIS/landscape_rasters/predictors_waterbird_win'))

combos_finalize %>% filter(SDM == 'riparian') %>% #filter/subset as needed
  purrr::pmap(DeltaMultipleBenefits::python_focal_finalize,
              pathin = 'GIS/landscape_rasters/focal_stats',
              maskpath = 'GIS/boundaries/delta.tif',
              cover = TRUE,
              overwrite = TRUE)

# FIT MODELS--------
# --> fit_SDMs function is designed to read in all predictors from the directory
# of the same scenario_name, select those included in the SDM, generate
# predictions, and then optionally mask out land cover types designated as
# unsuitable a priori before writing the results raster for each spp

load('data/riparian_landbirds/BRT_models_riparian_final.RData')
load('data/waterbirds/BRT_models_final.RData')

# riparian
purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::fit_SDM(
             pathin = 'GIS/landscape_rasters/predictors_riparian',
             landscape_name = .x,
             modlist = BRT_riparian,
             constants = data.frame(region = 1,
                                    area.ha = 3.141593),
             unsuitable = 90, #open water
             landscape = scenarios[[.x]] %>%
               mask(rast('GIS/boundaries/delta.tif')),
             pathout = 'GIS/prediction_rasters/riparian',
             overwrite = TRUE
           ))

# waterbird_fall -- note different offset values for cranes & geese vs. dblr,
# shore, cicon

# crane, geese:
purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::fit_SDM(
             pathin = 'GIS/landscape_rasters/predictors_waterbird_fall',
             landscape_name = .x,
             modlist = waterbird_mods_fall[c('crane', 'geese')],
             constants = data.frame(offset = 3.709),
             factors = list(list('covertype' = c('Alfalfa',
                                                 'Irrigated pasture',
                                                 'Rice',
                                                 'Wetland'))),
             unsuitable = c(10:19, 60, 130), #perennial crops, urban, barren
             landscape = scenarios[[.x]] %>%
               mask(rast('GIS/boundaries/delta.tif')),
             pathout = 'GIS/prediction_rasters/waterbird_fall',
             overwrite = TRUE
           ))

# dblr, shore, cicon:
purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::fit_SDM(
             pathin = 'GIS/landscape_rasters/predictors_waterbird_fall',
             landscape_name = .x,
             modlist = waterbird_mods_fall[c('dblr', 'cicon', 'shore')],
             constants = data.frame(offset = 4.435),
             factors = list(list('covertype' = c('Alfalfa',
                                                 'Irrigated pasture',
                                                 'Rice',
                                                 'Wetland'))),
             unsuitable = c(10:19, 60, 130), #perennial crops, urban, barren
             landscape = scenarios[[.x]] %>%
               mask(rast('GIS/boundaries/delta.tif')),
             pathout = 'GIS/prediction_rasters/waterbird_fall',
             overwrite = TRUE
           ))
# beepr::beep(1)

# waterbird_win: one offset for all, so can do together
purrr::map(names(scenarios)[which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::fit_SDM(
             pathin = 'GIS/landscape_rasters/predictors_waterbird_win',
             landscape_name = .x,
             modlist = waterbird_mods_win,
             constants = data.frame(offset = 3.617),
             factors = list(list('covertype' = c('Alfalfa',
                                                 'Corn',
                                                 'Irrigated pasture',
                                                 'Rice',
                                                 'Wetland',
                                                 'Winter wheat'))),
             unsuitable = c(10:19, 60, 130), #perennial crops, urban, barren
             landscape = scenarios[[.x]] %>%
               mask(rast('GIS/boundaries/delta.tif')),
             pathout = 'GIS/prediction_rasters/waterbird_win',
             overwrite = TRUE))

## thresholds---------
# create versions of prediction maps that use model-specific thresholds to
# convert probabilities into binary presence/absence
purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::transform_SDM(
             modlist = BRT_riparian,
             pathin = 'GIS/prediction_rasters/riparian',
             landscape_name = .x,
             stat = 'equal_sens_spec',
             pathout = 'GIS/prediction_rasters_threshold/riparian'))

purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::transform_SDM(
             modlist = waterbird_mods_fall,
             pathin = 'GIS/prediction_rasters/waterbird_fall',
             landscape_name = .x,
             stat = 'equal_sens_spec',
             pathout = 'GIS/prediction_rasters_threshold/waterbird_fall'))

purrr::map(names(scenarios)[which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::transform_SDM(
             modlist = waterbird_mods_win,
             pathin = 'GIS/prediction_rasters/waterbird_win',
             landscape_name = .x,
             stat = 'equal_sens_spec',
             pathout = 'GIS/prediction_rasters_threshold/waterbird_win'))
