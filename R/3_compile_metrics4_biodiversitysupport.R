# BIODIVERSITY SUPPORT
# species distribution models developed in prior analysis for riparian landbirds
# and waterbird groups

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
    scenario_landscape = list(NULL,
                              scenarios$scenario1_restoration,
                              scenarios$scenario2_perennialexpand,
                              scenarios$scenario3_combo),
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
  list(scenario_landscape = list(NULL,
                                 scenarios$scenario1_restoration_win,
                                 scenarios$scenario2_perennialexpand_win,
                                 scenarios$scenario3_combo_win),
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
  filename = 'droost_km.tif',
  scale = 'km',
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

## compare predictors---------
# compare scenarios to baseline in case of any substantial deviation

### riparian----
riparian = list(
  baseline = list.files('GIS/landscape_rasters/predictors_riparian/baseline/',
                        '.tif$', full.names = TRUE) %>% rast(),
  scenario1 = list.files('GIS/landscape_rasters/predictors_riparian/scenario1_restoration/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario2 = list.files('GIS/landscape_rasters/predictors_riparian/scenario2_perennialexpand/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario3 = list.files('GIS/landscape_rasters/predictors_riparian/scenario3_combo/',
                         '.tif$', full.names = TRUE) %>% rast())

rip_stats = purrr::map_df(
  riparian,
  ~full_join(
    global(., 'mean', na.rm = TRUE) %>% as_tibble(rownames = 'predictor'),
    global(., 'range', na.rm = TRUE) %>% as_tibble(rownames = 'predictor') %>%
      purrr::set_names(c('predictor', 'min', 'max')),
    by = 'predictor'),
  .id = 'landscape')
write_csv(rip_stats, 'output/predictor_summary_riparian.csv')

rip_stats %>%
  filter(grepl('RIPARIAN|WETLAND|ORCHVIN', predictor)) %>%
  # filter(grepl('_2000', predictor)) %>%
  ggplot(aes(mean, predictor, xmin = min, xmax = max)) +
  geom_pointrange(aes(color = landscape), position = position_dodge(width = 0.25))

rip_stats %>%
  filter(grepl('RIPARIAN|WETLAND|ORCHVIN', predictor)) %>%
  mutate(max = round(max, digits = 2),
         mean = round(mean, digits = 2)) %>%
  unite('range', min:max, sep = '-') %>%
  pivot_wider(names_from = landscape, values_from = c('mean', 'range')) %>%
  select(predictor, ends_with('baseline'), ends_with('scenario1'), ends_with('scenario2'))

### waterbird_fall----
waterbird_fall = list(
  baseline = list.files('GIS/landscape_rasters/predictors_waterbird_fall/baseline/',
                        '.tif$', full.names = TRUE) %>% rast(),
  scenario1 = list.files('GIS/landscape_rasters/predictors_waterbird_fall/scenario1_restoration/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario2 = list.files('GIS/landscape_rasters/predictors_waterbird_fall/scenario2_perennialexpand/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario3 = list.files('GIS/landscape_rasters/predictors_waterbird_fall/scenario3_combo/',
                         '.tif$', full.names = TRUE) %>% rast())

fall_stats = purrr::map_df(
  waterbird_fall,
  ~full_join(
    global(., 'mean', na.rm = TRUE) %>% as_tibble(rownames = 'predictor'),
    global(., 'range', na.rm = TRUE) %>% as_tibble(rownames = 'predictor') %>%
      purrr::set_names(c('predictor', 'min', 'max')),
    by = 'predictor'),
  .id = 'landscape')
write_csv(fall_stats, 'output/predictor_summary_waterbird_fall.csv')

# fall_stats_plotformat = fall_stats %>%
#   filter(grepl('woodw|duwet|orch|droost', predictor)) %>%
#   filter(!grepl('pfld', predictor)) %>%
#   separate(predictor, into = c('predictor', 'unit', 'scale')) %>%
#   replace_na(replace = list(scale = ' ')) %>%
#   mutate(landscape = factor(landscape,
#                             levels = c('scenario3', 'scenario2', 'scenario1', 'baseline'),
#                             labels = c('Scenario 3', 'Scenario 2', 'Scenario 1', 'Baseline')),
#          predictor = factor(predictor,
#                             levels = c('duwet', 'woodw', 'orch', 'droost'),
#                             labels = c("Managed Wetlands (sq km)",
#                                        "Riparian (sq km)",
#                                        "Perennial Crops (sq km)",
#                                        "Distance to crane roost (km)")),
#          scale = factor(scale, levels = rev(c('2k', '5k', '10k', ' ')),
#                         labels = rev(c('2 km', '5 km', '10 km', ' '))),
#          across(c(mean, min, max),
#                 ~if_else(scale == ' ', ., ./100)))
#
# library(showtext)
# font_add_google('Source Sans Pro', 'sourcesans')
# showtext_auto()
# showtext_opts(dpi = 300) #default for ggsave
#
# fall_stats_plotformat %>%
#   ggplot(aes(mean, scale, xmin = min, xmax = max)) +
#   ggforce::facet_col(~predictor, scales = 'free_y', space = 'free') +
#   # facet_wrap(~predictor, ncol = 1, scales = 'free', labeller = label_parsed) +
#   geom_pointrange(aes(color = landscape), fatten = 2,
#                   position = position_dodge(width = 0.5)) +
#   labs(x = NULL, y = NULL) +
#   xlim(0, NA) +
#   scale_color_manual(values = pointblue.palette[c(4:1)]) +
#   theme_minimal() +
#   theme(
#     # axis.line.x = element_line(color = 'gray30'),
#         axis.text = element_text(family = 'sourcesans', size = 9),
#         panel.grid.major.y = element_blank(),
#         plot.title = element_text(family = 'sourcesans', size = 10),
#         strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
#         strip.background = element_blank(),
#         # legend.position = 'none'
#         )
# ggsave('fig/predictor_stats.jpg',
#        height = 6, width = 6.5, units = 'in', dpi = 300)
# showtext_auto(FALSE)

fall_stats %>%
  filter(grepl('woodw|duwet|orch|droost', predictor) & !(grepl('pfld', predictor))) %>%
  mutate(min = round(min, digits = 0),
         max = round(max, digits = 0),
         mean = round(mean, digits = 0)) %>%
  unite('range', min:max, sep = '-') %>%
  pivot_wider(names_from = landscape, values_from = c('mean', 'range')) %>%
  select(predictor, ends_with('baseline'), ends_with('scenario1'), ends_with('scenario2'))



### waterbird_win-----
waterbird_win = list(
  baseline = list.files('GIS/landscape_rasters/predictors_waterbird_win/baseline_win/',
                        '.tif$', full.names = TRUE) %>% rast(),
  scenario1 = list.files('GIS/landscape_rasters/predictors_waterbird_win/scenario1_restoration_win/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario2 = list.files('GIS/landscape_rasters/predictors_waterbird_win/scenario2_perennialexpand_win/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario3 = list.files('GIS/landscape_rasters/predictors_waterbird_win/scenario3_combo_win/',
                         '.tif$', full.names = TRUE) %>% rast())

win_stats = purrr::map_df(
  waterbird_win,
  ~full_join(
    global(., 'mean', na.rm = TRUE) %>% as_tibble(rownames = 'predictor'),
    global(., 'range', na.rm = TRUE) %>% as_tibble(rownames = 'predictor') %>%
      purrr::set_names(c('predictor', 'min', 'max')),
    by = 'predictor'),
  .id = 'landscape')
write_csv(win_stats, 'output/predictor_summary_waterbird_win.csv')

rm(riparian, waterbird_fall, waterbird_win)
rm(rip_stats, fall_stats, win_stats)


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

# CALCULATE CHANGE MAPS--------

## RIPARIAN-----
pred_rip_binary = list(
  'baseline' = list.files('GIS/prediction_rasters_threshold/riparian/baseline', '.tif',
                          full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario1' = list.files('GIS/prediction_rasters_threshold/riparian/scenario1_restoration/', '.tif',
                           full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario2' = list.files('GIS/prediction_rasters_threshold/riparian/scenario2_perennialexpand/', '.tif',
                           full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario3' = list.files('GIS/prediction_rasters_threshold/riparian/scenario3_combo/', '.tif',
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

change3_rip = purrr::map(
  names(pred_rip_binary$baseline),
  ~diff(c(pred_rip_binary$baseline[[.x]],
          pred_rip_binary$scenario3[[.x]]))) %>%
  setNames(names(pred_rip_binary$baseline)) %>%
  rast()
writeRaster(change3_rip,
            file.path('GIS/change_rasters/riparian/scenario3_combo',
                      paste0(names(change3_rip), '.tif')),
            overwrite = TRUE)

## WATERBIRDS FALL---------
pred_fall_binary = list(
  'baseline' = list.files('GIS/prediction_rasters_threshold/waterbird_fall/baseline', '.tif',
                          full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario1' = list.files('GIS/prediction_rasters_threshold/waterbird_fall/scenario1_restoration/', '.tif',
                           full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario2' = list.files('GIS/prediction_rasters_threshold/waterbird_fall/scenario2_perennialexpand/', '.tif',
                           full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario3' = list.files('GIS/prediction_rasters_threshold/waterbird_fall/scenario3_combo/', '.tif',
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

change3_fall = purrr::map(
  names(pred_fall_binary$baseline),
  ~diff(c(pred_fall_binary$baseline[[.x]],
          pred_fall_binary$scenario3[[.x]]))) %>%
  setNames(names(pred_fall_binary$baseline)) %>%
  rast()
writeRaster(change3_fall,
            file.path('GIS/change_rasters/waterbird_fall/scenario3_combo',
                      paste0(names(change3_fall), '.tif')),
            overwrite = TRUE)


## WATERBIRDS WINTER-------
pred_win_binary = list(
  'baseline' = list.files('GIS/prediction_rasters_threshold/waterbird_win/baseline_win', '.tif',
                          full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario1' = list.files('GIS/prediction_rasters_threshold/waterbird_win/scenario1_restoration_win', '.tif',
                           full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario2' = list.files('GIS/prediction_rasters_threshold/waterbird_win/scenario2_perennialexpand_win', '.tif',
                           full.names = TRUE, recursive = TRUE) %>% rast(),
  'scenario3' = list.files('GIS/prediction_rasters_threshold/waterbird_win/scenario3_combo_win', '.tif',
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

change3_win = purrr::map(
  names(pred_win_binary$baseline),
  ~diff(c(pred_win_binary$baseline[[.x]],
          pred_win_binary$scenario3[[.x]]))) %>%
  setNames(names(pred_win_binary$baseline)) %>%
  rast()
writeRaster(change3_win,
            file.path('GIS/change_rasters/waterbird_win/scenario3_combo',
                      paste0(names(change3_win), '.tif')),
            overwrite = TRUE)

rm(pred_rip_binary, pred_fall_binary, pred_win_binary)
rm(change3_rip, change3_fall, change3_win)

# PLOT CHANGE MAPS---------
spp_key = read_csv('output/TABLE_species_key.csv')

delta_shp = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(crs = st_crs(32610))
palette = c("#2b83ba", "#80bfab", "#c7e8ad", "#ffffbf", "#fdc980", "#f07c4a",
            "#d7191c")

library(showtext)
font_add_google('Source Sans Pro', 'sourcesans')

## riparian-------
change1_rip = list.files('GIS/change_rasters/riparian/scenario1_restoration/',
                         '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change1_rip) + facet_wrap(~label, ncol = 5) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario1_riparian.png', height = 7, width = 10)
showtext_auto(F)

change2_rip = list.files('GIS/change_rasters/riparian/scenario2_perennialexpand/',
                         '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change2_rip) + facet_wrap(~label, ncol = 5) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario2_riparian.png', height = 7, width = 10)
showtext_auto(F)

change3_rip = list.files('GIS/change_rasters/riparian/scenario3_combo/',
                         '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change3_rip) + facet_wrap(~label, ncol = 5) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario3_riparian.png', height = 7, width = 10)
showtext_auto(F)

## waterbird_fall-----------

change1_fall = list.files('GIS/change_rasters/waterbird_fall/scenario1_restoration/',
                          '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change1_fall) + facet_wrap(~label, ncol = 3) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = 'right',
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario1_waterbirds_fall.png', height = 7, width = 7)
showtext_auto(F)

change2_fall = list.files('GIS/change_rasters/waterbird_fall/scenario2_perennialexpand/',
                          '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change2_fall) + facet_wrap(~label, ncol = 3) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = 'right',
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario2_waterbirds_fall.png', height = 7, width = 7)
showtext_auto(F)

change3_fall = list.files('GIS/change_rasters/waterbird_fall/scenario3_combo/',
                          '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change3_fall) + facet_wrap(~label, ncol = 3) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = 'right',
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario3_waterbirds_fall.png', height = 7, width = 7)
showtext_auto(F)

## waterbird_winter-----------

change1_win = list.files('GIS/change_rasters/waterbird_win/scenario1_restoration/',
                         '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change1_win) + facet_wrap(~label, ncol = 3) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = 'right',
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario1_waterbirds_winter.png', height = 7, width = 7)
showtext_auto(F)

change2_win = list.files('GIS/change_rasters/waterbird_win/scenario2_perennialexpand/',
                         '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change2_win) + facet_wrap(~label, ncol = 3) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = 'right',
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario2_waterbirds_winter.png', height = 7, width = 7)
showtext_auto(F)

change3_win = list.files('GIS/change_rasters/waterbird_win/scenario3_combo/',
                         '.tif$', full.names = TRUE) %>% rast() %>%
  as.data.frame(xy = TRUE) %>%
  pivot_longer(!x:y, names_to = 'spp') %>%
  filter(value != 0) %>%
  left_join(spp_key, by = 'spp') %>%
  mutate(value = factor(value, levels = c(-1, 1),
                        labels = c('loss', 'gain')),
         label = factor(label, levels = spp_key$label))

showtext_auto()
showtext_opts(dpi = 300)
ggplot(change3_win) + facet_wrap(~label, ncol = 3) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_manual(values = c('gain' = palette[1], 'loss' = palette[7])) +
  labs(x = NULL, y = NULL, fill = 'Change in\npredicted\npresence') +
  geom_sf(data = delta_shp, fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = 'sourcesans', size = 10, hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 10),
        legend.text = element_text(family = 'sourcesans', size = 9),
        legend.position = 'right',
        plot.margin = margin(2, 2, 2, 2, unit = 'pt'),
        panel.spacing = unit(3, 'pt'))
ggsave('fig/changemap_scenario3_waterbirds_winter.png', height = 7, width = 7)
showtext_auto(F)

# SUMMARIZE TOTAL HABITAT---------
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
