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
scenarios = c(rast('GIS/landscape_rasters/veg_baseline.tif'),
              rast('GIS/scenario_rasters/scenario1_restoration.tif'),
              rast('GIS/scenario_rasters/scenario2_perennialexpand.tif'),
              rast('GIS/landscape_rasters/veg_baseline_winter.tif'),
              rast('GIS/scenario_rasters/scenario1_restoration_win.tif'),
              rast('GIS/scenario_rasters/scenario2_perennialexpand_win.tif'))

delta = rast('GIS/boundaries/delta.tif')
# delta_buff10k = read_sf('GIS/boundaries/Legal_Delta_boundary_buff10k.shp') %>%
#   st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
# delta_buff10k = rasterize(vect(delta_buff10k), extend(delta, delta_buff10k))
# delta0 = subst(delta_buff10k, 1, 0)

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
purrr::map2(.x = scenarios[[1:3]] %>% as.list(),
            .y = file.path('GIS/landscape_rasters/predictors_waterbird_fall',
                           names(scenarios[[1:3]])),
            generate_covertype,
            type = 'waterbird_fall',
            maskpath = 'GIS/boundaries/delta.tif')

# waterbird_win:
purrr::map2(.x = scenarios[[4:6]] %>% as.list(),
            .y = file.path('GIS/landscape_rasters/predictors_waterbird_win',
                           names(scenarios[[4:6]])),
            generate_covertype,
            type = 'waterbird_win',
            maskpath = 'GIS/boundaries/delta.tif')

# # compare covertype to original predictors used in report results:
# pred = list.files('GIS/landscape_rasters/predictors_waterbird_win',
#                         'covertype.tif$', full.names = TRUE, recursive = TRUE) %>% rast()
# pred_orig = list.files('GIS/landscape_rasters/archive/waterbirds_win_predstack',
#                        'covertype.tif$', full.names = TRUE, recursive = TRUE) %>% rast()
# scenario = 2
# c(pred[[scenario]],
#   pred_orig[[scenario]] %>% mask(delta)) %>%
#   plot(colNA = 'gray50')
# c(pred[[scenario]],
#   pred_orig[[scenario]] %>% mask(delta) %>% subst(from = c(98:99), to = NA)) %>%
#   diff() %>% plot(colNA = 'gray50')
#
# --> all are identical to original

## pwater----------
# baseline mean pwater by season:
pwater_fall = rast('GIS/landscape_rasters/pwater/water_fall_mean.tif')
mwater_fall = zonal(pwater_fall, scenarios$baseline, fun = mean, na.rm = TRUE) %>%
  setNames(c('label', 'pwater')) %>% drop_na() %>%
  left_join(freq(scenarios$baseline), by = 'label')
pwater_win = rast('GIS/landscape_rasters/pwater/water_win_mean.tif')
mwater_win = zonal(pwater_win, scenarios$baseline_win, fun = mean, na.rm = TRUE) %>%
  setNames(c('label', 'pwater')) %>% drop_na() %>%
  left_join(freq(scenarios$baseline), by = 'label')
# use baseline mean pwater by season to inform pixels that change class in
# each scenario

# waterbird_fall:
purrr::pmap(list(baseline_landscape = list(scenarios[[1]], scenarios[[1]], scenarios[[1]]),
                 scenario_landscape = list(NULL, scenarios[[2]], scenarios[[3]]),
                 landscape_name = names(scenarios[[1:3]]),
                 floor = c(FALSE, TRUE, FALSE)),
            generate_pwater,
            waterdatpath = 'GIS/landscape_rasters/pwater/water_fall_mean.tif',
            maskpath = 'GIS/boundaries/delta.tif',
            pathout = c('GIS/landscape_rasters/pwater',
                        'GIS/landscape_rasters/predictors_waterbird_fall'),
            overwrite = TRUE)

# waterbird_win:
purrr::pmap(list(baseline_landscape = list(scenarios[[4]], scenarios[[4]], scenarios[[4]]),
                 scenario_landscape = list(NULL, scenarios[[5]], scenarios[[6]]),
                 landscape_name = names(scenarios[[4:6]]),
                 floor = c(FALSE, TRUE, FALSE)),
            generate_pwater,
            waterdatpath = 'GIS/landscape_rasters/pwater/water_win_mean.tif',
            maskpath = 'GIS/boundaries/delta.tif',
            pathout = c('GIS/landscape_rasters/pwater',
                        'GIS/landscape_rasters/predictors_waterbird_win'),
            overwrite = TRUE)

# # compare pwater estimates to original predictors used in report results:
# pred = list.files('GIS/landscape_rasters/pwater', 'pwater.tif',
#                   full.names = TRUE, recursive = TRUE) %>% rast()
# names(pred) = c('baseline', 'baseline_win', 'scenario1', 'scenario1_win',
#                 'scenario2', 'scenario2_win')
# pred_orig = list.files('GIS/landscape_rasters/archive', 'pwater.tif',
#                        full.names = TRUE, recursive = TRUE) %>% rast()
# names(pred_orig) = c('baseline', 'scenario1', 'scenario2', 'baseline_win',
#                      'scenario1_win', 'scenario2_win')
# scenario = 'scenario1_win'
# c(pred[[scenario]], pred_orig[[scenario]]) %>% mask(delta) %>% diff() %>%
#   plot(colNA = 'gray50')
# c(pred[[scenario]], pred_orig[[scenario]] %>% mask(delta)) %>%
#   plot(colNA = 'gray50')
# # small differences in pwater for each scenario and season (but not for baseline)


## droost----------

purrr::pmap(list(landscape = scenarios[[1:3]] %>% as.list(),
                 scenario_name = names(scenarios)[1:3]),
            update_roosts,
            roostpath = 'GIS/original_source_data/Ivey/Select_recent_roosts_Ivey_utm.shp',
            pathout = 'GIS/landscape_rasters/crane_roosts')

# RERUN - TEST AUTO CONVERSION TO KM (BUT NEED TO FIX FILENAME OUTPUT)
reticulate::source_python('R/0_dist_stats.py')
purrr::pmap(list(scenario_name = names(scenarios)[1:3],
                 copyto = names(scenarios)[4:6]),
            python_dist,
            pathin = 'GIS/landscape_rasters/crane_roosts',
            pathout = c('GIS/landscape_rasters/predictors_waterbird_fall',
                        'GIS/landscape_rasters/predictors_waterbird_win'),
            maskpath = 'GIS/boundaries/delta.tif',
            overwrite = TRUE)

# # compare droost estimates to original predictors used in report results:
# pred = list.files('GIS/landscape_rasters/predictors_waterbird_win',
#                   'droost_km.tif', recursive = TRUE, full.names = TRUE) %>% rast()
# pred_orig = list.files('GIS/landscape_rasters/archive/waterbirds_win_predstack',
#                        'droost_km.tif', recursive = TRUE, full.names = TRUE) %>% rast()
# scenario = 1
# c(pred[[scenario]], pred_orig[[scenario]] %>% mask(delta)) %>% diff() %>%
#   plot(colNA = 'gray50')
# # all are identical to original droost!


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
purrr::pmap(list(landscape = scenarios[[1:3]] %>% as.list(),
                 scenario_name = names(scenarios[[1:3]])),
            python_prep,
            SDM = 'riparian',
            pathout = 'GIS/landscape_rasters/cover',
            overwrite = TRUE)


# waterbird_fall
pwater_list = list.files('GIS/landscape_rasters/pwater', 'pwater.tif',
                         recursive = TRUE, full.names = TRUE)
pwater_fall =  pwater_list[-which(grepl('_win', pwater_list))] %>% rast()
purrr::pmap(list(landscape = scenarios[[1:3]] %>% as.list(),
                 mask = pwater_fall %>% as.list(), #same number of layers as landscape
                 scenario_name = names(scenarios[[1:3]])),
            python_prep,
            SDM = 'waterbird_fall',
            pixel_value = 0.09,
            pathout = 'GIS/landscape_rasters/cover', overwrite = TRUE,
            suffix = c('_area', '_pfld'))

# waterbird_win
pwater_win =  pwater_list[which(grepl('_win', pwater_list))] %>% rast()
purrr::pmap(list(landscape = scenarios[[4:6]] %>% as.list(),
                 mask = pwater_win %>% as.list(),
                 scenario_name = names(scenarios[[4:6]])),
            python_prep,
            SDM = 'waterbird_win',
            pixel_value = 0.09,
            pathout = 'GIS/landscape_rasters/cover', overwrite = TRUE,
            suffix = c('_area', '_pfld'))

## run Python focal stats------------
# for faster compilation of focal stats, use python and arcpy (requires python
# installation, ArcGIS license, and Spatial Analyst extension)
# --> may function better if running RStudio as administrator?

library(reticulate)
arcpy <- reticulate::import('arcpy')
arcpy$CheckOutExtension("Spatial")
reticulate::source_python('R/0_focal_stats.py')

combos_python = bind_rows(
  expand_grid(type = 'riparian',
              scenario = names(scenarios[[1:3]]),
              scale = c('50', '2000')),
  expand_grid(type = 'waterbird_fall',
              scenario = names(scenarios[[1:3]]),
              scale = c('2000', '5000', '10000')),
  expand_grid(type = 'waterbird_win',
              scenario = names(scenarios[[4:6]]), #changed!
              scale = c('5000', '10000')))

# --> NOTE: this may hours to run, slower for larger buffer sizes, and will
# generate a lot of raster files on disk; try smaller chunks by scenario
# --> ALSO NOTE: this will not overwrite existing files, so need to move old
# versions to a new location or change 'pathout'

# riparian: total pixels within buffer
combos_python %>% filter(type == 'riparian') %>%
  purrr::pmap(python_run,
              pathin = 'GIS/landscape_rasters/cover',
              pathout = 'GIS/landscape_rasters/focal_stats',
              fun = 'SUM')

# waterbirds: fall and winter, total area within buffer, requires regex
combos_python %>% filter(type != 'riparian') %>%
  purrr::pmap(python_run,
              pathin = 'GIS/landscape_rasters/cover',
              pathout = 'GIS/landscape_rasters/focal_stats',
              regex = '*_area.tif', fun = 'SUM')

# waterbirds: fall and winter, mean pfld within buffer, requires regex
combos_python %>% filter(type != 'riparian') %>%
  purrr::pmap(python_run,
              pathin = 'GIS/landscape_rasters/cover',
              pathout = 'GIS/landscape_rasters/focal_stats',
              regex = '*_pfld.tif', fun = 'MEAN')

## finalize predictors----------
# convert riparian pixel counts to proportion area; fix predictor names to match
# inputs expected by SDMs; mask by study area boundary

combos_finalize = combos_python %>%
  rename(SDM = type) %>%
  mutate(
    pathout = case_when(SDM == 'riparian' ~ 'GIS/landscape_rasters/predictors_riparian',
                        SDM == 'waterbird_fall' ~ 'GIS/landscape_rasters/predictors_waterbird_fall',
                        SDM == 'waterbird_win' ~ 'GIS/landscape_rasters/predictors_waterbird_win'))

combos_finalize %>%
  filter(SDM != 'riparian') %>% #filter/subset as needed
  purrr::pmap(finalize_SDM_predictors,
              pathin = 'GIS/landscape_rasters/focal_stats',
              maskpath = 'GIS/boundaries/delta.tif',
              cover = TRUE,
              overwrite = TRUE)

# # compare focal stats to original predictors used in report results:
pred = list.files('GIS/landscape_rasters/predictors_waterbird_fall/scenario1_restoration',
                  'pfld_2k', full.names = TRUE) %>% rast()
pred_orig = list.files('GIS/landscape_rasters/archive/waterbirds_fall_predstack/scenario1',
                       'pfld_2k.tif$', full.names = TRUE, recursive = TRUE) %>%
  rast() %>% mask(delta)
purrr::map(names(pred) %>% set_names(names(pred)),
           function(x) c(pred[[x]], pred_orig[[x]]) %>% diff())

# fall baseline area & pfld 2k identical: ALL (alf, barren, corn, dev, dryp,
#    duwet, fal, field, for, grain, ip, orch, rice, row, water, wet, woodw)
# fall scenario 1 area 2k identical: ALL; pfld: duwet & woodw are quite different!
# fall scenario 2 area 2k identical: ALL; pfld: all identical but orch
plot(c(pred$woodw_pfld_2k, pred_orig$woodw_pfld_2k) %>% diff())

# FIT MODELS--------
# --> fit_SDMs function is designed to read in all predictors from the directory
# of the same scenario_name, select those included in the SDM, generate
# predictions, and then optionally mask out land cover types designated as
# unsuitable a priori before writing the results raster for each spp

load('data/riparian_landbirds/BRT_models_riparian_final.RData')
load('data/waterbirds/BRT_models_final.RData')


# riparian
purrr::pmap(list(scenario_name = names(scenarios)[1:3],
                 landscape = scenarios[[1:3]] %>% mask(delta) %>% as.list()),
            fit_SDMs,
            modlist = BRT_riparian,
            constants = data.frame(region = 1,
                                   area.ha = 3.141593),
            unsuitable = 90, #open water
            pathin = 'GIS/landscape_rasters/predictors_riparian',
            pathout = 'GIS/prediction_rasters/riparian',
            overwrite = TRUE)

# waterbird_fall -- note different offset values for cranes & geese vs. dblr,
# shore, cicon

# crane, geese:
purrr::pmap(list(scenario_name = names(scenarios)[1:3],
                 landscape = scenarios[[1:3]] %>% mask(delta) %>% as.list()),
            fit_SDMs,
            modlist = waterbird_mods_fall[c('crane', 'geese')],
            constants = data.frame(offset = 3.709),
            factors = list(list('covertype' = c('Alfalfa',
                                                'Irrigated pasture',
                                                'Rice',
                                                'Wetland'))),
            unsuitable = c(10:19, 60, 130), #perennial crops, urban, barren
            pathin = 'GIS/landscape_rasters/predictors_waterbird_fall',
            pathout = 'GIS/prediction_rasters/waterbird_fall',
            overwrite = TRUE)

# dblr, shore, cicon:
purrr::pmap(list(scenario_name = names(scenarios)[1:3],
                 landscape = scenarios[[1:3]] %>% mask(delta) %>% as.list()),
            fit_SDMs,
            modlist = waterbird_mods_fall[c('dblr', 'cicon', 'shore')],
            constants = data.frame(offset = 4.435),
            factors = list(list('covertype' = c('Alfalfa',
                                                'Irrigated pasture',
                                                'Rice',
                                                'Wetland'))),
            unsuitable = c(10:19, 60), #perennial crops and urban
            pathin = 'GIS/landscape_rasters/predictors_waterbird_fall',
            pathout = 'GIS/prediction_rasters/waterbird_fall',
            overwrite = TRUE)
# beepr::beep(1)

# # compare predictions to original report results:
# pred = list.files('GIS/prediction_rasters/waterbird_fall/scenario2_perennialexpand/', '.tif',
#                   full.names = TRUE) %>% rast()
# pred_orig = list.files('GIS/prediction_rasters/waterbirds_fall_orig/scenario2', '.tif',
#                        full.names = TRUE) %>% rast()
# spp = 'cicon'
# c(pred[[spp]], pred_orig[[spp]]) %>% diff() %>% plot(colNA = 'gray50')
#
# # baseline identical to original for geese, dblr, shore, cicon
# # --> [[slight variation for crane]]
# # scenario1 is fairly different from original for all
# # scenario2 is slightly different from original for all

# waterbird_win: one offset for all, so can do together
purrr::pmap(list(scenario_name = names(scenarios)[4:6],
                 landscape = scenarios[[4:6]] %>% mask(delta) %>% as.list()),
            fit_SDMs,
            modlist = waterbird_mods_win,
            const = data.frame(offset = 3.617),
            factors = list(list('covertype' = c('Alfalfa',
                                                'Corn',
                                                'Irrigated pasture',
                                                'Rice',
                                                'Wetland',
                                                'Winter wheat'))),
            unsuitable = c(10:19, 60, 130), #perennial crops, urban, barren
            pathin = 'GIS/landscape_rasters/predictors_waterbird_win',
            pathout = 'GIS/prediction_rasters/waterbird_win',
            overwrite = TRUE)
beepr::beep(1)

# compare predictions to original report results:
pred = list.files('GIS/prediction_rasters/waterbird_fall/scenario1_restoration/', '.tif',
                  full.names = TRUE) %>% rast()
pred_orig = list.files('GIS/prediction_rasters/waterbirds_fall_orig/scenario1/', '.tif',
                       full.names = TRUE) %>% rast()
spp = 'geese'
c(pred[[spp]], pred_orig[[spp]]) %>% diff() %>% plot(colNA = 'gray50')
#--> winter predictions have changed due to fixing pwater

# baseline identical to original for geese, dblr, shore, cicon
# --> [[slight variation for crane]]
# scenario1 is fairly different from original for all (including divduck)
# scenario2 is fairly different from original for all (except divduck) due to fixing pwater
