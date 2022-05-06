# README---------
# From baseline land cover layers, compile landcover stats for use with
# waterbird spatial models for the fall season (July 15 - Nov 17) and the winter
# season

# PACKAGES & FUNCTIONS
source('R/packages.R')
source('R/functions.R')

# reference data:
baseline = rast('GIS/landscape_rasters/veg_baseline.tif')
scenario1 = rast('GIS/scenario_rasters/scenario1_restoration.tif')
scenario2 = rast('GIS/scenario_rasters/scenario2_perennialexpand.tif')

baseline_win = rast('GIS/landscape_rasters/veg_baseline_winter.tif')
scenario1_win = rast('GIS/scenario_rasters/scenario1_restoration_win.tif')
scenario2_win = rast('GIS/scenario_rasters/scenario2_perennialexpand_win.tif')

key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')
wkey = key %>% select(CODE_WATERBIRD, LABEL_WATERBIRD) %>% drop_na() %>%
  arrange(CODE_WATERBIRD)

delta = rast('GIS/boundaries/delta.tif')
delta_buff10k = read_sf('GIS/boundaries/Legal_Delta_boundary_buff10k.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
delta_buff10k = rasterize(vect(delta_buff10k), extend(delta, delta_buff10k))
delta0 = subst(delta_buff10k, 1, 0)

palette = c("#2b83ba", "#80bfab", "#c7e8ad", "#ffffbf",
            "#fdc980", "#f07c4a", "#d7191c")

# library(terra)
# library(elevatr)
# delta_longlat = delta %>% project('epsg:4326', method = 'near')
# test = get_elev_raster(raster(delta_longlat), z = 12)
# elev = rast(test) %>% project(delta)

# RECLASSIFY-------
# reclassify landcover rasters to match encodings used by distribution models

## fall----
fallrasters = c(baseline, scenario1, scenario2) %>%
  classify(rcl = key %>% select(from = CODE_BASELINE, to = CODE_WATERBIRD) %>%
             drop_na() %>%
             # for fall, combine wheat and grain
             mutate(to = case_when(from == 22 ~ 10,
                                   TRUE ~ to)) %>%
             as.matrix())
levels(fallrasters) <- list(
  wkey %>% filter(LABEL_WATERBIRD != 'ww') %>%
    arrange(CODE_WATERBIRD) %>% as.data.frame(),
  wkey %>% filter(LABEL_WATERBIRD != 'ww') %>%
    arrange(CODE_WATERBIRD) %>% as.data.frame(),
  wkey %>% filter(LABEL_WATERBIRD != 'ww') %>%
    arrange(CODE_WATERBIRD) %>% as.data.frame())
names(fallrasters) = c('baseline', 'scenario1', 'scenario2')
plot(fallrasters)

# # check conversions
# crosstab(c(baseline_win, fallrasters$baseline), useNA = TRUE, long = TRUE)

## winter----
winrasters = c(baseline_win, scenario1_win, scenario2_win) %>%
  classify(rcl = key %>% select(from = CODE_BASELINE, to = CODE_WATERBIRD) %>%
             drop_na() %>%
             # mutate(to = case_when(from == 130 ~ NA_real_,
             #                       TRUE ~ to)) %>%
             as.matrix())
levels(winrasters) <- list(
  wkey %>% arrange(CODE_WATERBIRD) %>% as.data.frame(),
  wkey %>% arrange(CODE_WATERBIRD) %>% as.data.frame(),
  wkey %>% arrange(CODE_WATERBIRD) %>% as.data.frame())
names(winrasters) = c('baseline', 'scenario1', 'scenario2')
plot(winrasters)

# # check conversions
# crosstab(c(baseline_win, winrasters$baseline), useNA = TRUE, long = TRUE)


# COMPILE MODEL METRICS------
# for baseline and each scenario

## droost_km-----------
# for cranes only, calculate distance to roost

# first rasterize original 94 polygons
roost_shp = read_sf('GIS/original_source_data/Ivey/Select_recent_roosts_Ivey_utm.shp')
roost = roost_shp %>% vect() %>% rasterize(., baseline)
writeRaster(roost, 'GIS/landscape_rasters/crane_roosts.tif')

### check landcover-------
# check what baseline land cover types now overlay crane roosts
tab = crosstab(c(baseline, roost), useNA = TRUE, long = TRUE)
tab %>% rlang::set_names(c('baseline', 'roost', 'n')) %>%
  left_join(key %>% select(baseline = CODE_BASELINE, from = CODE_NAME)) %>%
  arrange(roost, desc(n))
# incompatible types: orch, vin, some riparian forest (pofr, salix, qulo, mixed) and scrub (salix, intro, mixed)

# check how much overlap with new baseline:
roost_overlay = baseline %>% subst(from = c(20:69, 80:130), to = 0) %>%
  subst(from = c(11:19, 70:77), to = 1) %>%
  terra::extract(vect(roost_shp))
exclude = roost_overlay %>%
  group_by(ID, lyr1) %>% count() %>% ungroup() %>%
  group_by(ID) %>% mutate(ncell = sum(n), prop = n/ncell) %>% ungroup() %>%
  filter(lyr1 == 1) %>% arrange(desc(prop))
# 45 roost polygons now overlap somewhat with rip/orch; 10 with considerable overlap
# (>20%), 3 with substantial overlap (>70%) --2 of these >90%
# c('79', '78', '64', '39', '60', '71', '62', '26', '44', '48')

# REPEAT FOR SCENARIO 1 AND 2
roost_overlay_scenario1 = scenario1 %>% subst(from = c(20:69, 80:130), to = 0) %>%
  subst(from = c(11:19, 70:77), to = 1) %>%
  terra::extract(vect(roost_shp))
exclude_scenario1 = roost_overlay_scenario1 %>%
  group_by(ID, scenario_restoration) %>% count() %>% ungroup() %>%
  group_by(ID) %>% mutate(ncell = sum(n), prop = n/ncell) %>% ungroup() %>%
  filter(scenario_restoration == 1) %>% arrange(desc(prop))
# again, 45 roosts now overlap somewhat with rip/orch; the same 10 with
# considerable overlap (>20%)

roost_overlay_scenario2 = scenario2 %>% subst(from = c(20:69, 80:130), to = 0) %>%
  subst(from = c(11:19, 70:77), to = 1) %>%
  terra::extract(vect(roost_shp))
exclude_scenario2 = roost_overlay_scenario2 %>%
  group_by(ID, scenario_perennial_expansion) %>% count() %>% ungroup() %>%
  group_by(ID) %>% mutate(ncell = sum(n), prop = n/ncell) %>% ungroup() %>%
  filter(scenario_perennial_expansion == 1) %>% arrange(desc(prop))
# now 56 roosts with some overlap; 14 with considerable overlap (>20%)
# including the original 10 plus: 42, 80, 93, 65 [so essentially only removed 4 more]

### exclude incompatible land cover-----
# update baseline roost layer: (also use for scenario 1)
roost_update = roost_shp %>%
  filter(!Roost_ID %in%
           c('79', '78', '64', '39', '60', '71', '62', '26', '44', '48'))
# now, 84 polygons
roost_update = roost_update %>% vect() %>% rasterize(., baseline)
writeRaster(roost_update, 'GIS/landscape_rasters/crane_roosts_update.tif',
            overwrite = TRUE)

# and update scenario 2 roost layer
roost_update_scenario2 = roost_shp %>%
  filter(!Roost_ID %in%
           c('79', '78', '64', '39', '60', '71', '62', '26', '44', '48',
             '42', '80', '93', '65'))
# now, 80 polygons
roost_update_scenario2 = roost_update_scenario2 %>% vect() %>% rasterize(., baseline)
writeRaster(roost_update_scenario2, 'GIS/landscape_rasters/crane_roosts_update_scenario2.tif',
            overwrite = TRUE)

c(roost, roost_update, roost_update_scenario2) %>% plot()
# map the polygons removed
roost %>% mask(roost_update, inverse = TRUE) %>% plot()
roost_update %>% mask(roost_update_scenario2, inverse = TRUE) %>% plot()

### calculate new distance to roost-------
# # PYTHON APPROACH: (not currently working -- 'No raster store is configurated')
# library(reticulate)
# arcpy <- reticulate::import('arcpy')
# arcpy$CheckOutExtension('spatial')
# roost_py = arcpy$sa$Raster('GIS/landscape_rasters/crane_roosts_update.tif')
# roostdist = arcpy$sa$EucDistance(roost_py, 30000, 30, 'GIS/landscape_rasters/crane_roosts_update_dist.tif')
#
# reticulate::source_python('R/focal_stats.py')
# dist_stats(pathin = 'GIS/landscape_rasters',
#            regex = 'crane_roosts_update.tif',
#            pathout = 'GIS/landscape_rasters')

# OR RUN IN R (SLOW)
# roostdist = distance(roost) #distance to roost in m
# roostdist_fill = roostdist %>% subst(from = NA, to = 0) #why fill with zeroes?
# roostdist_km = roostdist_fill / 1000
# roostdist_mask = roostdist_km %>% mask(delta)
# writeRaster(roostdist_mask,
#             filename = 'GIS/landscape_rasters/waterbirds_fall_predstack/droost_km.tif',
#             wopt = list(names = 'droost_km'))
# # copy to winter stack as well:
# writeRaster(roostdist_mask,
#             filename = 'GIS/landscape_rasters/waterbirds_win_predstack/droost_km.tif',
#             wopt = list(names = 'droost_km'))

# OR RUN IN ARCGIS SEPARATELY (Euclidean Distance tool), THEN BRING BACK INTO R:

# process to mask and convert to km:
roostdist = rast('GIS/landscape_rasters/crane_roosts_update_EucDist.tif') %>%
  mask(delta_buff10k)
roostdist_km = roostdist / 1000

roostdist_scenario2 = rast('GIS/landscape_rasters/crane_roosts_update_scenario2_EucDist.tif') %>%
  mask(delta_buff10k)
roostdist_scenario2_km = roostdist_scenario2 / 1000

c(roostdist_km, roostdist_scenario2_km) %>% plot(col = palette)
# most difference in the south/central portion of the Delta

# write to stacks for baseline & scenario 1, plus winter versions:
writeRaster(roostdist_km,
            filename = 'GIS/landscape_rasters/waterbirds_fall_predstack/baseline/droost_km.tif',
            wopt = list(names = 'droost_km'),
            overwrite = TRUE)
writeRaster(roostdist_km,
            filename = 'GIS/landscape_rasters/waterbirds_win_predstack/baseline/droost_km.tif',
            wopt = list(names = 'droost_km'),
            overwrite = TRUE)
writeRaster(roostdist_km,
            filename = 'GIS/landscape_rasters/waterbirds_fall_predstack/scenario1/droost_km.tif',
            wopt = list(names = 'droost_km'),
            overwrite = TRUE)
writeRaster(roostdist_km,
            filename = 'GIS/landscape_rasters/waterbirds_win_predstack/scenario1/droost_km.tif',
            wopt = list(names = 'droost_km'),
            overwrite = TRUE)
# scenario2:
writeRaster(roostdist_scenario2_km,
            filename = 'GIS/landscape_rasters/waterbirds_fall_predstack/scenario2/droost_km.tif',
            wopt = list(names = 'droost_km'),
            overwrite = TRUE)
writeRaster(roostdist_scenario2_km,
            filename = 'GIS/landscape_rasters/waterbirds_win_predstack/scenario2/droost_km.tif',
            wopt = list(names = 'droost_km'),
            overwrite = TRUE)



## covertype--------
# categorical var representing the covertype at the survey point/pixel

### fall------
# predict only to: Alfalfa, Irrigated pasture, Rice, and Wetland
covertype_key_fall = wkey %>%
  mutate(covertype = recode(LABEL_WATERBIRD,
                            'rice' = 'Rice',
                            'ip' = 'Irrigated pasture',
                            'alf' = 'Alfalfa',
                            'duwet' = 'Wetland',
                            # unsuitable by definition
                            'barren' = 'Z',
                            'orch' = 'Z',
                            'dev' = 'Z',
                            # all others
                            .default = 'X')) %>%
  arrange(covertype) %>%
  mutate(covertype_code = c(1:4, rep(98, 11), rep(99, 3)))

# reclassify to these factor levels; all others NA
covertype_fall = fallrasters %>%
  classify(
    rcl = covertype_key_fall %>%
      select(from = CODE_WATERBIRD, to = covertype_code),
    othersNA = TRUE)
levels(covertype_fall) <- list(
  covertype_key_fall %>% select(covertype_code, covertype) %>% distinct() %>%
    as.data.frame(),
  covertype_key_fall %>% select(covertype_code, covertype) %>% distinct() %>%
    as.data.frame(),
  covertype_key_fall %>% select(covertype_code, covertype) %>% distinct() %>%
    as.data.frame())
names(covertype_fall) = c('baseline', 'scenario1', 'scenario2')

covertype_fall %>% mask(delta) %>% plot(col = rev(palette))

writeRaster(
  covertype_fall$baseline,
  filename = 'GIS/landscape_rasters/waterbirds_fall_predstack/baseline/covertype.tif',
  wopt = list(names = 'covertype'), overwrite = TRUE)
writeRaster(
  covertype_fall$scenario1,
  filename = 'GIS/landscape_rasters/waterbirds_fall_predstack/scenario1/covertype.tif',
  wopt = list(names = 'covertype'), overwrite = TRUE)
writeRaster(
  covertype_fall$scenario2,
  filename = 'GIS/landscape_rasters/waterbirds_fall_predstack/scenario2/covertype.tif',
  wopt = list(names = 'covertype'), overwrite = TRUE)

### winter-----
# predict only to: Alfalfa, Corn, Irrigated pasture, Rice, Wetland, Winter wheat
covertype_key_win = wkey %>%
  mutate(covertype = recode(LABEL_WATERBIRD,
                            'rice' = 'Rice',
                            'ip' = 'Irrigated pasture',
                            'alf' = 'Alfalfa',
                            'duwet' = 'Wetland',
                            'ww' = 'Winter wheat',
                            'corn' = 'Corn',
                            # unsuitable by definition
                            'barren' = 'Z',
                            'orch' = 'Z',
                            'dev' = 'Z',
                            # all others (unmodeled)
                            .default = 'X')) %>%
  arrange(covertype) %>%
  mutate(covertype_code = c(1:6, rep(98, 9), rep(99, 3)))

# reclassify to these factor levels; all others NA
covertype_win = winrasters %>%
  classify(
    rcl = covertype_key_win %>%
      select(from = CODE_WATERBIRD, to = covertype_code),
    othersNA = TRUE)
levels(covertype_win) <- list(
  covertype_key_win %>% select(covertype_code, covertype) %>% distinct() %>%
    as.data.frame(),
  covertype_key_win %>% select(covertype_code, covertype) %>% distinct() %>%
    as.data.frame(),
  covertype_key_win %>% select(covertype_code, covertype) %>% distinct() %>%
    as.data.frame())
names(covertype_win) = c('baseline', 'scenario1', 'scenario2')

covertype_win %>% mask(delta) %>% plot(col = rev(palette))

writeRaster(
  covertype_win$baseline,
  filename = 'GIS/landscape_rasters/waterbirds_win_predstack/baseline/covertype.tif',
  wopt = list(names = 'covertype'), overwrite = TRUE)
writeRaster(
  covertype_win$scenario1,
  filename = 'GIS/landscape_rasters/waterbirds_win_predstack/scenario1/covertype.tif',
  wopt = list(names = 'covertype'), overwrite = TRUE)
writeRaster(
  covertype_win$scenario2,
  filename = 'GIS/landscape_rasters/waterbirds_win_predstack/scenario2/covertype.tif',
  wopt = list(names = 'covertype'), overwrite = TRUE)


## pwater------
# originally the proportion of survey area with open water, but for predictions,
# use the mean probability of open water for each pixel across all available
# surface water mosaics 2013-2019, during each season

### diff_scenario-------
# for pixels that changed land cover categories in each scenario, assume new
# pwater equals baseline mean pwater for that class
# --> first find pixels that have changed in each scenario to use as a mask (can
# use either fallrasters or winrasters for this)
diff_scenario1 = c(scenario1, baseline) %>%
  diff() %>% subst(from = 0, to = NA) %>% #no change = NA
  classify(rcl = matrix(c(-Inf, Inf, 1), nrow = 1)) # all others = 1

diff_scenario2 = c(scenario2, baseline) %>%
  diff() %>% subst(from = 0, to = NA) %>% #no change = NA
  classify(rcl = matrix(c(-Inf, Inf, 1), nrow = 1))

plot(c(diff_scenario1, diff_scenario2))


### fall---------

# start with baseline mean probability of open water:
pwater_fall = rast('GIS/landscape_rasters/water_fall_mean.tif')

# calculate mean baseline pwater by land cover class
# mwater_fall = zonal(pwater_fall,
#                     fallrasters$baseline, fun = mean, na.rm = TRUE) %>%
#   select(value = baseline, pwater = water_fall_mean) %>%
#   full_join(wkey %>%
#               filter(!LABEL_WATERBIRD %in% c('ww')) %>%
#               select(level = CODE_WATERBIRD, value = LABEL_WATERBIRD) %>%
#               drop_na(), by = 'value')

# calculate mean baseline pwater by land cover class (detailed)
mwater_fall = zonal(pwater_fall %>% mask(delta),
                    baseline, fun = mean, na.rm = TRUE) %>%
  select(value = lyr1, pwater = water_fall_mean) %>%
  full_join(key %>%
              # filter(!LABEL_WATERBIRD %in% c('ww')) %>%
              select(value = CODE_BASELINE, CODE_NAME) %>%
              drop_na(), by = 'value')

mwater_fall
# orch_deciduous = 1.3%
# orch_citrus = 0.01%
# vineyard = 1.8%
# wetland_perennial = 19.95%
# wetland_seasonal = 20.9%
# riparian: ranges 5.4-15.9%

# # assign mean pwater values by new land cover classs
# fallpwater_scenario1_new = fallrasters$scenario1 %>% mask(diff_scenario1) %>%
#   classify(rcl = mwater_fall %>% select(level, pwater) %>% as.matrix(),
#            othersNA = TRUE)
# fallpwater_scenario2_new = fallrasters$scenario2 %>% mask(diff_scenario2) %>%
#   classify(rcl = mwater_fall %>% select(level, pwater) %>% as.matrix(),
#            othersNA = TRUE)
# plot(c(fallpwater_scenario1_new, fallpwater_scenario2_new))

# assign mean pwater values by new land cover classs
fallpwater_scenario1_new = scenario1 %>% mask(diff_scenario1) %>%
  classify(rcl = mwater_fall %>% select(value, pwater) %>% drop_na() %>% as.matrix(),
           othersNA = TRUE)
fallpwater_scenario2_new = scenario2 %>% mask(diff_scenario2) %>%
  classify(rcl = mwater_fall %>% select(value, pwater) %>% drop_na() %>% as.matrix(),
           othersNA = TRUE)
plot(c(fallpwater_scenario1_new, fallpwater_scenario2_new))

# overlay new seasonal pwater values on baseline values

# check for difference from baseline
tmp = c(fallpwater_scenario1_new, pwater_fall$water_fall_mean) %>% diff() %>%
  classify(rcl = matrix(c(-Inf, 0, 99), nrow = 1), othersNA= TRUE)
plot(tmp)
c(fallpwater_scenario1_new, pwater_fall$water_fall_mean) %>% diff() %>% mask(tmp, inverse = TRUE) %>% plot()
# lots of new values would be lower than baseline

# for restoration scenario, don't replace if baseline value is
# already greater than the mean value
pwater_fall = c(pwater_fall,
                lapp(c(fallpwater_scenario1_new,
                       pwater_fall$water_fall_mean),
                     function(x, y) {
                       ifelse(x > 0 & !is.na(x) & x > y, x, y)
                     }),
                cover(fallpwater_scenario2_new,
                      pwater_fall$water_fall_mean))
names(pwater_fall) = c('baseline', 'scenario1', 'scenario2')
plot(pwater_fall)
c(diff(c(pwater_fall$baseline, pwater_fall$scenario1)),
  diff(c(pwater_fall$baseline, pwater_fall$scenario2))) %>%
  plot(range = c(-1, 1))

writeRaster(pwater_fall$baseline,
            'GIS/landscape_rasters/waterbirds_fall_predstack/baseline/pwater.tif',
            wopt = list(names = 'pwater'), overwrite = TRUE)
writeRaster(pwater_fall$scenario1,
            'GIS/landscape_rasters/waterbirds_fall_predstack/scenario1/pwater.tif',
            wopt = list(names = 'pwater'), overwrite = TRUE)
writeRaster(pwater_fall$scenario2,
            'GIS/landscape_rasters/waterbirds_fall_predstack/scenario2/pwater.tif',
            wopt = list(names = 'pwater'), overwrite = TRUE)

rm(fallpwater_scenario1_new, fallpwater_scenario2_new, mwater_fall)

### winter-------

# start with baseline:
pwater_win = rast('GIS/landscape_rasters/water_win_mean.tif')

# calculate mean baseline pwater by land cover class
mwater_win = zonal(pwater_win %>% mask(delta),
                   baseline_win, fun = mean, na.rm = TRUE) %>%
  select(value = lyr1, pwater = water_win_mean) %>%
  full_join(key %>%
              select(value = CODE_BASELINE, label = CODE_NAME) %>%
              drop_na(), by = 'value')
mwater_win
# orch_deciduous: 6.3%
# orch_citrus: 5.8%
# vineyard: 5.6%
# riparian: 14.9-26%
# wetland_perennial: 28.9%
# wetland_seasonal: 66.5%

# assign mean pwater values by new land cover classs
winpwater_scenario1_new = scenario1_win %>% mask(diff_scenario1) %>%
  classify(rcl = mwater_win %>% select(value, pwater) %>% drop_na() %>% as.matrix(),
           othersNA = TRUE)
winpwater_scenario2_new = scenario2_win %>% mask(diff_scenario2) %>%
  classify(rcl = mwater_win %>% select(value, pwater) %>% drop_na() %>% as.matrix(),
           othersNA = TRUE)
plot(c(winpwater_scenario1_new, winpwater_scenario2_new))

# check for difference from baseline
tmp = c(winpwater_scenario1_new, pwater_win$water_win_mean) %>% diff() %>%
  classify(rcl = matrix(c(-Inf, 0, 99), nrow = 1), othersNA= TRUE)
plot(tmp)
c(winpwater_scenario1_new, pwater_win$water_win_mean) %>% diff() %>% mask(tmp, inverse = TRUE) %>% plot()
# lots of new values would be lower than baseline

pwater_win = c(pwater_win,
                # for restoration scenario, don't replace if baseline value is
                # already greater than the mean value
                lapp(c(winpwater_scenario1_new,
                       pwater_win$water_win_mean),
                     function(x, y) {
                       ifelse(x > 0 & !is.na(x) & x > y, x, y)
                     }),
                cover(winpwater_scenario2_new,
                      pwater_win$water_win_mean))
names(pwater_win) = c('baseline', 'scenario1', 'scenario2')
plot(pwater_win)
c(diff(c(pwater_win$baseline, pwater_win$scenario1)),
  diff(c(pwater_win$baseline, pwater_win$scenario2))) %>%
  plot(range = c(-1, 1))

writeRaster(pwater_win$baseline,
            'GIS/landscape_rasters/waterbirds_win_predstack/baseline/pwater.tif',
            wopt = list(names = 'pwater'), overwrite = TRUE)
writeRaster(pwater_win$scenario1,
            'GIS/landscape_rasters/waterbirds_win_predstack/scenario1/pwater.tif',
            wopt = list(names = 'pwater'), overwrite = TRUE)
writeRaster(pwater_win$scenario2,
            'GIS/landscape_rasters/waterbirds_win_predstack/scenario2/pwater.tif',
            wopt = list(names = 'pwater'), overwrite = TRUE)


# _area, _pfld (FOCAL STATS)---------

# Goals:
# - area: total area of each land cover class within specified buffer distance
# (sum of cells), within buffer, by season
# - pfld: mean probability (0-1) of open water for all cells of a land cover
# class within buffer, by season

### Python prep---------
# first prep for python focal statistics calculations by splitting raster stacks
# for each scenario and season, with one layer for each land cover class
# - to support area calculations: fill pixel with 1 if land cover is present,
# and 0 otherwise (python will sum # of pixels by buffer size)
# - to support pfld calculations: further mask land cover presence above by
# pwater layer (python will calculate mean of pixels by buffer size)

# Note: masklayers, if present, should be same number of layers as stack

python_prep(stack = fallrasters,
            masklayers = pwater_fall,
            pixel_value = 0.09,
            path = 'GIS/landscape_rasters/waterbirds_fall',
            layernames = levels(fallrasters)[[1]] %>% na.omit() %>% as.vector(),
            suffix = c('_area', '_pfld'))

python_prep(stack = winrasters,
            masklayers = pwater_win,
            pixel_value = 0.09,
            path = 'GIS/landscape_rasters/waterbirds_win',
            layernames = levels(winrasters)[[1]] %>% na.omit() %>% as.vector(),
            suffix = c('_area', '_pfld'))


### PYTHON------------
# for faster compilation of focal stats, use python and arcpy (requires python
# installation, ArcGIS license, and Spatial Analyst extension)
# --> may function better if running RStudio as administrator!

library(reticulate)
arcpy <- reticulate::import('arcpy')
arcpy$CheckOutExtension('spatial')
reticulate::source_python('R/focal_stats.py')

memory.limit(size = 48000)

# goal: for each season (fall or winter), scenario (baseline, scenario1,
# scenario2), and spatial scale (2k, 5k, 10k), calculate cover area as the total
# area of each land cover class within the buffer, and proportion flooded as the
# mean probability of open water for all cells of a particular land cover class
# within the buffer


#### fall-----
# fall models included all 3 spatial scales
combos = tibble(scenarios = c('baseline', 'scenario1', 'scenario2'),
                scales = c('2k', '5k', '10k')) %>%
  expand(scenarios, scales)

# NOTE: this may take 2-3 hours to run, and will generate a lot of raster files
# on disk
combos = combos %>% filter(scenarios != 'baseline')

purrr::map2(.x = combos$scenarios,
            .y = combos$scales,
            python_run,
            type = 'waterbird',
            pathin = 'GIS/landscape_rasters/waterbirds_fall/',
            pathout = 'GIS/landscape_rasters/waterbirds_fall_predstack/',
            zero_ras = delta0, mask = delta_buff10k)

#### winter-------
# winter models were all either 5k or 10k; 2k scale not needed -- but each
# column in tibble has to have the same number; "expand" will fix it to find all
# unique combos
combos = tibble(scenarios = c('baseline', 'scenario1', 'scenario2'),
                scales = c('5k', '10k', '5k')) %>%
  expand(scenarios, scales)

# NOTE: this may take 2-3 hours to run, and will generate a lot of raster files
# on disk
purrr::map2(.x = combos$scenarios,
            .y = combos$scales,
            python_run,
            type = 'waterbird',
            pathin = 'GIS/landscape_rasters/waterbirds_win/',
            pathout = 'GIS/landscape_rasters/waterbirds_win_predstack/',
            zero_ras = delta0, mask = delta_buff10k)

