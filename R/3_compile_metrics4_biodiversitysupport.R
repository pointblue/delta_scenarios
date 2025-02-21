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
#reticulate::use_python('C:/Python27/ArcGISx6410.8/python.exe', required = TRUE)
reticulate::use_python('C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe', required = TRUE)

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
             key = readxl::read_excel('GIS/VEG_key.xlsx'),
             SDM = 'waterbird_fall',
             maskpath = 'GIS/boundaries/delta.tif',
             pathout = 'GIS/SDM_predictors',
             overwrite = TRUE))

# waterbird_win:
purrr::map(names(scenarios)[which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::update_covertype(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             key = readxl::read_excel('GIS/VEG_key.xlsx'),
             SDM = 'waterbird_win',
             maskpath = 'GIS/boundaries/delta.tif',
             pathout = 'GIS/SDM_predictors',
             overwrite = TRUE))

# alt fall:
purrr::map(names(scenarios)[which(!grepl('win', names(scenarios)) & grepl('_alt', names(scenarios)))],
           ~DeltaMultipleBenefits::update_covertype(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             key = readxl::read_excel('GIS/VEG_key.xlsx'),
             SDM = 'waterbird_fall',
             mask = 'GIS/boundaries/delta.tif',
             pathout = 'GIS/SDM_predictors',
             overwrite = TRUE))

# alt win:
purrr::map(names(scenarios)[which(grepl('win', names(scenarios)) & grepl('_alt', names(scenarios)))],
           ~DeltaMultipleBenefits::update_covertype(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             key = readxl::read_excel('GIS/VEG_key.xlsx'),
             SDM = 'waterbird_win',
             mask = 'GIS/boundaries/delta.tif',
             pathout = 'GIS/SDM_predictors',
             overwrite = TRUE))

## pwater----------

# waterbird_fall:
purrr::pmap(
  list(
    scenario_landscape = list(NULL,
                              scenarios$scenario1_restoration,
                              scenarios$scenario2_perennialexpand,
                              scenarios$scenario3_combo),
    landscape_name = names(scenarios)[-which(grepl('win', names(scenarios)))],
    floor = c(FALSE, TRUE, FALSE, FALSE)),
  DeltaMultipleBenefits::update_pwater,
  waterdat = 'GIS/landscape_rasters/water_fall_mean.tif',
  mask = 'GIS/boundaries/delta.tif',
  pathout = 'GIS/SDM_predictors',
  SDM = 'waterbird_fall',
  overwrite = TRUE,
  baseline_landscape = scenarios$baseline)

# waterbird_win:
purrr::pmap(
  list(scenario_landscape = list(NULL,
                                 scenarios$scenario1_restoration_win,
                                 scenarios$scenario2_perennialexpand_win,
                                 scenarios$scenario3_combo_win),
       landscape_name = names(scenarios)[which(grepl('win', names(scenarios)))],
       floor = c(FALSE, TRUE, FALSE, FALSE)),
  DeltaMultipleBenefits::update_pwater,
  waterdat = 'GIS/landscape_rasters/water_win_mean.tif',
  mask = 'GIS/boundaries/delta.tif',
  pathout = 'GIS/SDM_predictors',
  SDM = 'waterbird_win',
  overwrite = TRUE,
  baseline_landscape = scenarios$baseline_win)

# alt fall:
purrr::pmap(
  list(
    scenario_landscape = list(scenarios$scenario2_perennialexpand_alt,
                              scenarios$scenario3_combo_alt),
    landscape_name = c(names(scenarios)[which(!grepl('win', names(scenarios)) &
                                                grepl('_alt', names(scenarios)))])),
  DeltaMultipleBenefits::update_pwater,
  waterdat = 'GIS/landscape_rasters/water_fall_mean.tif',
  mask = 'GIS/boundaries/delta.tif',
  pathout = 'GIS/SDM_predictors',
  SDM = 'waterbird_fall',
  overwrite = TRUE,
  baseline_landscape = scenarios$baseline,
  floor = FALSE)

# alt win:
purrr::pmap(
  list(
    scenario_landscape = list(scenarios$scenario2_perennialexpand_alt_win,
                              scenarios$scenario3_combo_alt_win),
    landscape_name = c(names(scenarios)[which(grepl('win', names(scenarios)) &
                                                grepl('_alt', names(scenarios)))])),
  DeltaMultipleBenefits::update_pwater,
  waterdat = 'GIS/landscape_rasters/water_win_mean.tif',
  mask = 'GIS/boundaries/delta.tif',
  pathout = 'GIS/SDM_predictors',
  SDM = 'waterbird_win',
  overwrite = TRUE,
  baseline_landscape = scenarios$baseline_win,
  floor = FALSE)

## droost----------
# not specific to a seasonal SDM and unnecessary to repeat for each season since
# "unsuitable" land cover classes aren't dynamic and changing seasonally; output
# to a general crane_roosts folder
data(roosts_original)

i = which(grepl('baseline|scenario1|_alt', names(scenarios)) & !grepl('win', names(scenarios)))
purrr::map(names(scenarios)[i],
           ~DeltaMultipleBenefits::update_roosts(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             unsuitable = c(11:19, 60, 70:79, 100:120),
             proportion = 0.2,
             roosts = terra::vect(roosts_original),
             pathout = 'GIS/SDM_predictors/crane_roosts',
             overwrite = TRUE
           ))
# then use python to calculate distance to roost from every pixel in each landscape
purrr::map(
  names(scenarios)[i],
  ~DeltaMultipleBenefits::python_dist(
    pathin = 'GIS/SDM_predictors/crane_roosts',
    landscape_name = .x,
    pathout = 'GIS/SDM_predictors',
    SDM = 'waterbird_fall',
    filename = 'droost_km.tif',
    scale = 'km',
    mask = 'GIS/boundaries/delta.tif',
    overwrite = TRUE))
# copy to winter directories as well:
writeRaster(
  rast('GIS/SDM_predictors/waterbird_fall/baseline/droost_km.tif'),
  filename = 'GIS/SDM_predictors/waterbird_win/baseline_win/droost_km.tif')
writeRaster(
  rast('GIS/SDM_predictors/waterbird_fall/scenario1_restoration/droost_km.tif'),
  filename = 'GIS/SDM_predictors/waterbird_win/scenario1_restoration_win/droost_km.tif')
writeRaster(
  rast('GIS/SDM_predictors/waterbird_fall/scenario2_perennialexpand/droost_km.tif'),
  filename = 'GIS/SDM_predictors/waterbird_win/scenario2_perennialexpand_win/droost_km.tif')
writeRaster(
  rast('GIS/SDM_predictors/waterbird_fall/scenario3_combo/droost_km.tif'),
  filename = 'GIS/SDM_predictors/waterbird_win/scenario3_combo_win/droost_km.tif')


# alt:
purrr::map(names(scenarios)[which(!grepl('win', names(scenarios)) & grepl('_alt', names(scenarios)))],
           ~DeltaMultipleBenefits::update_roosts(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             unsuitable = c(11:19, 60, 70:79, 100:120),
             proportion = 0.2,
             roosts = 'GIS/original_source_data/Ivey/Select_recent_roosts_Ivey_utm.shp',
             pathout = 'GIS/SDM_predictors/crane_roosts',
             overwrite = TRUE
           ))
purrr::map(names(scenarios)[which(!grepl('win', names(scenarios)) &
                                    grepl('_alt', names(scenarios)))],
  ~DeltaMultipleBenefits::python_dist(
    pathin = 'GIS/SDM_predictors/crane_roosts',
    landscape_name = .x,
    pathout = 'GIS/SDM_predictors',
    SDM = 'waterbird_fall',
    filename = 'droost_km.tif',
    scale = 'km',
    mask = 'GIS/boundaries/delta.tif',
    overwrite = TRUE
  ))
# copy to winter directories as well:
writeRaster(
  rast('GIS/SDM_predictors/waterbird_fall/scenario2_perennialexpand_alt/droost_km.tif'),
  filename = 'GIS/SDM_predictors/waterbird_win/scenario2_perennialexpand_alt_win/droost_km.tif')
writeRaster(
  rast('GIS/SDM_predictors/waterbird_fall/scenario3_combo_alt/droost_km.tif'),
  filename = 'GIS/SDM_predictors/waterbird_win/scenario3_combo_alt_win/droost_km.tif')

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
             pathout = 'GIS/SDM_predictors/cover',
             overwrite = TRUE))

# waterbird_fall
pwater_list = list.files('GIS/SDM_predictors/pwater', 'pwater.tif',
                         recursive = TRUE, full.names = TRUE)
pwater_fall =  pwater_list[-which(grepl('_win', pwater_list))] %>% rast()
names(pwater_fall) = pwater_list[-which(grepl('_win', pwater_list))] %>%
  gsub('GIS/SDM_predictors/pwater/|/pwater.tif', '', .)

purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::python_focal_prep(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             SDM = 'waterbird_fall',
             mask = pwater_fall[[.x]],
             pixel_value = 0.09,
             pathout = 'GIS/SDM_predictors/cover',
             suffix = c('_area', '_pfld'),
             overwrite = TRUE))

# waterbird_win
pwater_win =  pwater_list[which(grepl('_win', pwater_list))] %>% rast()
names(pwater_win) = pwater_list[which(grepl('_win', pwater_list))] %>%
  gsub('GIS/SDM_predictors/pwater/|/pwater.tif', '', .)

purrr::map(names(scenarios)[which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::python_focal_prep(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             SDM = 'waterbird_win',
             mask = pwater_win[[.x]],
             pixel_value = 0.09,
             pathout = 'GIS/SDM_predictors/cover',
             suffix = c('_area', '_pfld'),
             overwrite = TRUE))

### alt---------
# riparian
purrr::map(names(scenarios)[which(!grepl('win', names(scenarios)) & grepl('_alt', names(scenarios)))],
           ~DeltaMultipleBenefits::python_focal_prep(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             SDM = 'riparian',
             pathout = 'GIS/SDM_predictors/cover',
             overwrite = TRUE))

# waterdat for waterbirds
pwater_list = list.files('GIS/SDM_predictors/pwater', 'pwater.tif',
                         recursive = TRUE, full.names = TRUE)
fall = which(!grepl('_win', pwater_list) & grepl('_alt', pwater_list))
pwater_fall_alt =  pwater_list[fall] %>% rast()
names(pwater_fall_alt) = pwater_list[fall] %>%
  gsub('GIS/SDM_predictors/pwater/|/pwater.tif', '', .)

win = which(grepl('_win', pwater_list) & grepl('_alt', pwater_list))
pwater_win_alt =  pwater_list[win] %>% rast()
names(pwater_win_alt) = pwater_list[win] %>%
  gsub('GIS/SDM_predictors/pwater/|/pwater.tif', '', .)

# waterbird_fall
purrr::map(names(scenarios)[fall],
           ~DeltaMultipleBenefits::python_focal_prep(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             SDM = 'waterbird_fall',
             mask = pwater_fall_alt[[.x]],
             pixel_value = 0.09,
             pathout = 'GIS/SDM_predictors/cover',
             suffix = c('_area', '_pfld')))

# waterbird_win
purrr::map(names(scenarios)[win],
           ~DeltaMultipleBenefits::python_focal_prep(
             landscape = scenarios[[.x]],
             landscape_name = .x,
             SDM = 'waterbird_win',
             mask = pwater_win_alt[[.x]],
             pixel_value = 0.09,
             pathout = 'GIS/SDM_predictors/cover',
             suffix = c('_area', '_pfld')))

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
              pathin = 'GIS/SDM_predictors/cover',
              pathout = 'GIS/SDM_predictors/focal_stats',
              fun = 'SUM')

# waterbirds: fall and winter, total area within buffer, requires regex
combos_python %>% filter(SDM != 'riparian') %>%
  purrr::pmap(DeltaMultipleBenefits::python_focal_run,
              pathin = 'GIS/SDM_predictors/cover',
              pathout = 'GIS/SDM_predictors/focal_stats',
              regex = '*_area.tif', fun = 'SUM')

# waterbirds: fall and winter, mean pfld within buffer, requires regex
combos_python %>% filter(SDM != 'riparian') %>%
  purrr::pmap(DeltaMultipleBenefits::python_focal_run,
              pathin = 'GIS/SDM_predictors/cover',
              pathout = 'GIS/SDM_predictors/focal_stats',
              regex = '*_pfld.tif', fun = 'MEAN')

### alt-------
combos_python_alt = bind_rows(
  expand_grid(SDM = 'riparian',
              landscape_name = names(scenarios)[which(!grepl('win', names(scenarios)) &
                                                        grepl('_alt', names(scenarios)))],
              scale = c('50', '2000')),
  expand_grid(SDM = 'waterbird_fall',
              landscape_name = names(scenarios)[which(!grepl('win', names(scenarios)) &
                                                         grepl('_alt', names(scenarios)))],
              scale = c('2000', '5000', '10000')),
  expand_grid(SDM = 'waterbird_win',
              landscape_name = names(scenarios)[which(grepl('win', names(scenarios)) &
                                                        grepl('_alt', names(scenarios)))],
              scale = c('5000', '10000')))

# riparian: total pixels within buffer
combos_python_alt %>% filter(SDM == 'riparian') %>%
  purrr::pmap(DeltaMultipleBenefits::python_focal_run,
              pathin = 'GIS/SDM_predictors/cover',
              pathout = 'GIS/SDM_predictors/focal_stats',
              fun = 'SUM')

# waterbirds: fall and winter, total area within buffer, requires regex
combos_python_alt %>% filter(SDM != 'riparian') %>%
  purrr::pmap(DeltaMultipleBenefits::python_focal_run,
              pathin = 'GIS/SDM_predictors/cover',
              pathout = 'GIS/SDM_predictors/focal_stats',
              regex = '*_area.tif', fun = 'SUM')

# waterbirds: fall and winter, mean pfld within buffer, requires regex
combos_python_alt %>% filter(SDM != 'riparian') %>%
  purrr::pmap(DeltaMultipleBenefits::python_focal_run,
              pathin = 'GIS/SDM_predictors/cover',
              pathout = 'GIS/SDM_predictors/focal_stats',
              regex = '*_pfld.tif', fun = 'MEAN')

## finalize predictors----------
# convert riparian pixel counts to proportion area; fix predictor names to match
# inputs expected by SDMs; mask by study area boundary
purrr::pmap(combos_python, # can filter/subset as needed
            DeltaMultipleBenefits::python_focal_finalize,
            pathin = 'GIS/SDM_predictors/focal_stats',
            pathout = 'GIS/SDM_predictors',
            mask = 'GIS/boundaries/delta.tif',
            cover = TRUE,
            overwrite = TRUE)

### alt------
purrr::pmap(combos_python_alt %>% filter(SDM != 'riparian'),
            DeltaMultipleBenefits::python_focal_finalize,
            pathin = 'GIS/SDM_predictors/focal_stats',
            pathout = 'GIS/SDM_predictors',
            mask = 'GIS/boundaries/delta.tif',
            cover = TRUE,
            overwrite = TRUE)

## compare predictors---------
# compare scenarios to baseline in case of any substantial deviation

### riparian----
riparian = list(
  baseline = list.files('GIS/SDM_predictors/riparian/baseline/',
                        '.tif$', full.names = TRUE) %>% rast(),
  scenario1 = list.files('GIS/SDM_predictors/riparian/scenario1_restoration/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario2 = list.files('GIS/SDM_predictors/riparian/scenario2_perennialexpand/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario3 = list.files('GIS/SDM_predictors/riparian/scenario3_combo/',
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
  baseline = list.files('GIS/SDM_predictors/waterbird_fall/baseline/',
                        '.tif$', full.names = TRUE) %>% rast(),
  scenario1 = list.files('GIS/SDM_predictors/waterbird_fall/scenario1_restoration/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario2 = list.files('GIS/SDM_predictors/waterbird_fall/scenario2_perennialexpand/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario3 = list.files('GIS/SDM_predictors/waterbird_fall/scenario3_combo/',
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
  baseline = list.files('GIS/SDM_predictors/waterbird_win/baseline_win/',
                        '.tif$', full.names = TRUE) %>% rast(),
  scenario1 = list.files('GIS/SDM_predictors/waterbird_win/scenario1_restoration_win/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario2 = list.files('GIS/SDM_predictors/waterbird_win/scenario2_perennialexpand_win/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario3 = list.files('GIS/SDM_predictors/waterbird_win/scenario3_combo_win/',
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
             pathin = 'GIS/SDM_predictors/riparian',
             landscape_name = .x,
             modlist = BRT_riparian,
             constants = data.frame(region = 1,
                                    area.ha = 3.141593),
             unsuitable = 90, #open water
             landscape = scenarios[[.x]] %>%
               mask(rast('GIS/boundaries/delta.tif')),
             pathout = 'GIS/SDM_results/riparian',
             overwrite = TRUE
           ))

# waterbird_fall -- note different offset values for cranes & geese vs. dblr,
# shore, cicon

# crane, geese:
purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::fit_SDM(
             pathin = 'GIS/SDM_predictors/waterbird_fall',
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
             pathout = 'GIS/SDM_results/waterbird_fall',
             overwrite = TRUE
           ))

# dblr, shore, cicon:
purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::fit_SDM(
             pathin = 'GIS/SDM_predictors/waterbird_fall',
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
             pathout = 'GIS/SDM_results/waterbird_fall',
             overwrite = TRUE
           ))
# beepr::beep(1)

# waterbird_win: one offset for all, so can do together
purrr::map(names(scenarios)[which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::fit_SDM(
             pathin = 'GIS/SDM_predictors/waterbird_win',
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
             pathout = 'GIS/SDM_results/waterbird_win',
             overwrite = TRUE))

## thresholds---------
# create versions of prediction maps that use model-specific thresholds to
# convert probabilities into binary presence/absence
purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::transform_SDM(
             modlist = BRT_riparian,
             pathin = 'GIS/SDM_results',
             SDM = 'riparian',
             landscape_name = .x,
             stat = 'equal_sens_spec',
             pathout = 'GIS/SDM_results_threshold'))

purrr::map(names(scenarios)[-which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::transform_SDM(
             modlist = waterbird_mods_fall,
             pathin = 'GIS/SDM_results',
             SDM = 'waterbird_fall',
             landscape_name = .x,
             stat = 'equal_sens_spec',
             pathout = 'GIS/SDM_results_threshold'))

purrr::map(names(scenarios)[which(grepl('win', names(scenarios)))],
           ~DeltaMultipleBenefits::transform_SDM(
             modlist = waterbird_mods_win,
             pathin = 'GIS/SDM_results',
             SDM = 'waterbird_win',
             landscape_name = .x,
             stat = 'equal_sens_spec',
             pathout = 'GIS/SDM_results_threshold'))

## alt-------
# riparian
purrr::map(names(scenarios)[which(!grepl('win', names(scenarios)) &
                                    grepl('_alt', names(scenarios)))],
           ~DeltaMultipleBenefits::fit_SDM(
             pathin = 'GIS/SDM_predictors',
             SDM = 'riparian',
             landscape_name = .x,
             modlist = BRT_riparian,
             constants = data.frame(region = 1,
                                    area.ha = 3.141593),
             unsuitable = 90, #open water
             landscape = scenarios[[.x]] %>%
               mask(rast('GIS/boundaries/delta.tif')),
             pathout = 'GIS/SDM_results',
             overwrite = TRUE
           ))

# waterbird_fall -- note different offset values for cranes & geese vs. others
# crane, geese:
purrr::map(names(scenarios)[which(!grepl('win', names(scenarios)) &
                                    grepl('_alt', names(scenarios)))],
           ~DeltaMultipleBenefits::fit_SDM(
             pathin = 'GIS/SDM_predictors',
             SDM = 'waterbird_fall',
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
             pathout = 'GIS/SDM_results',
             overwrite = TRUE))

# dblr, shore, cicon:
purrr::map(names(scenarios)[which(!grepl('win', names(scenarios)) & grepl('_alt', names(scenarios)))],
           ~DeltaMultipleBenefits::fit_SDM(
             pathin = 'GIS/SDM_predictors',
             SDM = 'waterbird_fall',
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
             pathout = 'GIS/SDM_results',
             overwrite = TRUE))
# beepr::beep(1)

# waterbird_win: one offset for all, so can do together
purrr::map(names(scenarios)[which(grepl('win', names(scenarios)) & grepl('_alt', names(scenarios)))],
           ~DeltaMultipleBenefits::fit_SDM(
             pathin = 'GIS/SDM_predictors',
             SDM = 'waterbird_win',
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
             pathout = 'GIS/SDM_results',
             overwrite = TRUE))

## thresholds:

# purrr::pmap(sets,
#             DeltaMultipleBenefits::transform_SDM,
#             stat = 'equal_sens_spec',
#             pathin = 'GIS/prediction_rasters',
#             pathout = 'GIS/prediction_rasters_threshold')


purrr::map(names(scenarios)[which(!grepl('win', names(scenarios)) & grepl('_alt', names(scenarios)))],
           ~DeltaMultipleBenefits::transform_SDM(
             modlist = BRT_riparian,
             pathin = 'GIS/SDM_results',
             SDM = 'riparian',
             landscape_name = .x,
             stat = 'equal_sens_spec',
             pathout = 'GIS/SDM_results_threshold'))

purrr::map(names(scenarios)[which(!grepl('win', names(scenarios)) & grepl('_alt', names(scenarios)))],
           ~DeltaMultipleBenefits::transform_SDM(
             modlist = waterbird_mods_fall,
             pathin = 'GIS/SDM_results',
             SDM = 'waterbird_fall',
             landscape_name = .x,
             stat = 'equal_sens_spec',
             pathout = 'GIS/SDM_results_threshold'))

purrr::map(names(scenarios)[which(grepl('win', names(scenarios)) & grepl('_alt', names(scenarios)))],
           ~DeltaMultipleBenefits::transform_SDM(
             modlist = waterbird_mods_win,
             pathin = 'GIS/SDM_results',
             SDM = 'waterbird_win',
             landscape_name = .x,
             stat = 'equal_sens_spec',
             pathout = 'GIS/SDM_results_threshold'))

# SUMMARIZE TOTAL HABITAT---------
# total suitable habitat for each landscape, predicted from SDMs

## probability of presence:
habitat = DeltaMultipleBenefits::sum_habitat(
  pathin = 'GIS/SDM_results',
  subtype = 'distributions',
  rollup = TRUE,
  key = 'output/TABLE_species_key.csv',
  scale = 0.09) %>%
  mutate(UNIT = 'ha')
write_csv(habitat, 'output/scenario_habitat.csv')

## binary presence/absence (using thresholds):
habitat_binary = DeltaMultipleBenefits::sum_habitat(
  pathin = 'GIS/SDM_results_threshold',
  subtype = 'habitat',
  rollup = TRUE,
  key = 'output/TABLE_species_key.csv',
  scale = 0.09) %>%
  mutate(UNIT = 'ha')
write_csv(habitat_binary, 'output/scenario_habitat_binary.csv')


# CALCULATE CHANGE MAPS--------
# ideally distinguish between areas that never were habitat and still aren't
# (diff = 0), and areas that already were habitat and still are (diff also = 0)

## RIPARIAN
purrr::map(c('scenario1_restoration',
             'scenario2_perennialexpand',
             'scenario3_combo'),
           ~calc_change_SDM(pathin = 'GIS/SDM_results_threshold',
                            SDM = 'riparian',
                            baseline_name = 'baseline',
                            scenario_name = .x,
                            pathout = 'GIS/SDM_results_diff',
                            differentiate = TRUE,
                            overwrite = TRUE))

## WATERBIRDS FALL
purrr::map(c('scenario1_restoration',
             'scenario2_perennialexpand',
             'scenario3_combo'),
           ~calc_change_SDM(pathin = 'GIS/SDM_results_threshold',
                            SDM = 'waterbird_fall',
                            baseline_name = 'baseline',
                            scenario_name = .x,
                            pathout = 'GIS/SDM_results_diff',
                            differentiate = TRUE,
                            overwrite = TRUE))

## WATERBIRDS WINTER
purrr::map(c('scenario1_restoration_win',
             'scenario2_perennialexpand_win',
             'scenario3_combo_win'),
           ~calc_change_SDM(pathin = 'GIS/SDM_results_threshold',
                            SDM = 'waterbird_win',
                            baseline_name = 'baseline_win',
                            scenario_name = .x,
                            pathout = 'GIS/SDM_results_diff',
                            differentiate = TRUE,
                            overwrite = TRUE))

## alt-----
combos = bind_rows(
  expand_grid(scenario_name = names(scenarios)[which(!grepl('win', names(scenarios)) &
                                                       grepl('_alt', names(scenarios)))],
              SDM = c('riparian', 'waterbird_fall'),
              baseline_name = 'baseline'),
  expand_grid(scenario_name = names(scenarios)[which(grepl('win', names(scenarios)) &
                                                       grepl('_alt', names(scenarios)))],
              SDM = 'waterbird_win',
              baseline_name = 'baseline_win')
)

purrr::pmap(combos,
            calc_change_SDM,
            pathin = 'GIS/SDM_results_threshold',
            pathout = 'GIS/SDM_results_diff',
            differentiate = TRUE,
            overwrite = TRUE)

# BOOTSTRAP----------
# resample original survey data, refit BRT, predict to all landscapes, and
# calculate total area of suitable habitat
scenarios = list.files('GIS/scenario_rasters', '.tif$', full.names = TRUE) %>%
  terra::rast()
source("R/0_bootstrap_SDM.R")

## riparian-------
load('data/riparian_landbirds/BRT_models_riparian_final.RData')
# set up mask (if needed)
watermask = terra::classify(scenarios$baseline %>%
                         terra::mask(terra::rast('GIS/boundaries/delta.tif')),
                       rcl = data.frame(from = 90,
                                        to = 0) %>% as.matrix(),
                       others = NA)
# read in riparian predictors
predictors = list(
  baseline = c(list.files('GIS/SDM_predictors/riparian', pattern = '.tif$', full.names = TRUE),
               list.files('GIS/SDM_predictors/riparian/baseline', pattern = '.tif$',
                          full.names = TRUE)) %>% terra::rast(),
  scenario1 = c(list.files('GIS/SDM_predictors/riparian', pattern = '.tif$', full.names = TRUE),
                list.files('GIS/SDM_predictors/riparian/scenario1_restoration/', pattern = '.tif$',
                           full.names = TRUE)) %>% terra::rast(),
  scenario2_alt = c(list.files('GIS/SDM_predictors/riparian', pattern = '.tif$', full.names = TRUE),
                    list.files('GIS/SDM_predictors/riparian/scenario2_perennialexpand_alt/', pattern = '.tif$',
                               full.names = TRUE)) %>% terra::rast(),
  scenario3_alt = c(list.files('GIS/SDM_predictors/riparian', pattern = '.tif$', full.names = TRUE),
                    list.files('GIS/SDM_predictors/riparian/scenario3_combo_alt/', pattern = '.tif$',
                               full.names = TRUE)) %>% terra::rast())

bootrip50 = bootstrap_SDM(
  modlist = BRT_riparian,
  n = c(1:50), #number of the bootstrap sample to run for each model
  predictors = predictors,
  constants = data.frame(region = 1,
                         area.ha = 3.141593),
  factors = NULL,
  unsuitable = watermask,
  stat = 'equal_sens_spec',
  rollup = TRUE)
write_csv(bootrip50, 'output/bootstrap_riparian50.csv')

## boot win------
load('data/waterbirds/BRT_models_final.RData')
waterbirdmask = terra::classify(scenarios$baseline %>%
                                  terra::mask(terra::rast('GIS/boundaries/delta.tif')),
                                rcl = data.frame(from = c(10:19, 60, 130),
                                                 to = 0) %>% as.matrix(),
                                others = NA)

waterbird_win = list(
  baseline = list.files('GIS/SDM_predictors/waterbird_win/baseline_win/',
                        '.tif$', full.names = TRUE) %>% rast(),
  scenario1 = list.files('GIS/SDM_predictors/waterbird_win/scenario1_restoration_win/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario2_alt = list.files('GIS/SDM_predictors/waterbird_win/scenario2_perennialexpand_alt_win/',
                             '.tif$', full.names = TRUE) %>% rast(),
  scenario3_alt = list.files('GIS/SDM_predictors/waterbird_win/scenario3_combo_alt_win/',
                             '.tif$', full.names = TRUE) %>% rast())

bootwin50 = bootstrap_SDM(
  modlist = waterbird_mods_win,
  n = c(1:50), #number of the bootstrap sample to run for each model
  predictors = waterbird_win,
  constants = data.frame(offset = 3.617),
  factors = list(list('covertype' = c('Alfalfa',
                                      'Corn',
                                      'Irrigated pasture',
                                      'Rice',
                                      'Wetland',
                                      'Winter wheat'))),
  unsuitable = waterbirdmask,
  stat = 'equal_sens_spec',
  rollup = TRUE)
write_csv(bootwin50, 'output/bootstrap_waterbirds_win_50.csv')


## boot fall-------

waterbird_fall = list(
  baseline = list.files('GIS/SDM_predictors/waterbird_fall/baseline/',
                        '.tif$', full.names = TRUE) %>% rast(),
  scenario1 = list.files('GIS/SDM_predictors/waterbird_fall/scenario1_restoration/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario2_alt = list.files('GIS/SDM_predictors/waterbird_fall/scenario2_perennialexpand_alt/',
                         '.tif$', full.names = TRUE) %>% rast(),
  scenario3_alt = list.files('GIS/SDM_predictors/waterbird_fall/scenario3_combo_alt/',
                         '.tif$', full.names = TRUE) %>% rast())

bootfall50 = bootstrap_SDM2(
  modlist = waterbird_mods_fall,
  n = c(1:50), #number of the bootstrap sample to run for each model
  predictors = waterbird_fall,
  constants = list('crane' = data.frame(offset = 3.709),
                   'geese' = data.frame(offset = 3.709),
                   'dblr' = data.frame(offset = 4.435),
                   'cicon' = data.frame(offset = 4.435),
                   'shore' = data.frame(offset = 4.435)),
  factors = list(list('covertype' = c('Alfalfa',
                                      'Irrigated pasture',
                                      'Rice',
                                      'Wetland'))),
  unsuitable = waterbirdmask,
  stat = 'equal_sens_spec',
  rollup = TRUE)
write_csv(bootfall50, 'output/bootstrap_waterbirds_fall_50.csv')


## compile--------
# add habitat totals to habitat change estimates from above
sppkey = read_csv('output/TABLE_species_key.csv')

bootrip50 = read_csv('output/bootstrap_riparian50.csv')
bootfall50 = read_csv('output/bootstrap_waterbirds_fall_50.csv')
bootwin50 = read_csv('output/bootstrap_waterbirds_win_50.csv')

boot_totals = bind_rows(bootrip50 %>% mutate(group = 'riparian'),
                        bootfall50 %>% mutate(group = 'fall'),
                        bootwin50 %>% mutate(group = 'win')) %>%
  left_join(sppkey %>% select(spp, METRIC = label), by = 'spp') %>%
  mutate(METRIC = case_when(group == 'fall' ~ paste0(METRIC, ' (fall)'),
                            group == 'win' ~ paste0(METRIC, ' (winter)'),
                            TRUE ~ METRIC),
         METRIC_SUBTYPE = if_else(group == 'riparian',
                                  'Riparian landbird habitat',
                                  'Waterbird habitat'),
         scenario = case_when(scenario == 'scenario1' ~ 'scenario1_restoration',
                              scenario == 'scenario2_alt' ~ 'scenario2_perennialexpand_alt',
                              scenario == 'scenario3_alt' ~ 'scenario3_combo_alt',
                              TRUE ~ scenario),
         scenario = if_else(group == 'win', paste0(scenario, '_win'), scenario)) %>%
  select(scenario, spp, METRIC_SUBTYPE, METRIC, n, value)
write_csv(boot_totals, 'output/scenario_habitat_binary_bootstrap.csv')

boot_totals_sum = boot_totals %>%
  group_by(scenario, spp, METRIC_SUBTYPE, METRIC) %>%
  summarize(median = median(value) * 0.09, #convert to ha
            se = sd(value)/sqrt(50) * 0.09,
            lcl = quantile(value, 0.025) * 0.09,
            ucl = quantile(value, 0.975) * 0.09,
            .groups = 'drop')

habitat_binary_ci = read_csv('output/scenario_habitat_binary.csv') %>%
  right_join(boot_totals_sum %>% select(scenario, METRIC, se, lcl, ucl),
            by = c('scenario', 'METRIC'))
write_csv(habitat_binary_ci, 'output/scenario_habitat_binary_ci.csv')
# >> SPTO CI does not include original predicted values for any scenario!
