# Compile and prep supporting GIS data for use in building scenarios of
# landscape change in the Delta


library(tidyverse)
library(sf)
library(raster)
# library(terra)

# reference raster:
gisdir = 'V:/Project/Terrestrial/cv_riparian/distribution_modeling/'
veg_baseline_riparian = raster::raster(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_riparian.tif'))
rkey = read_csv(paste0(gisdir, 'GIS/landscape_rasters/key.csv'),
                col_types = cols())

veg_baseline_waterbird_fall = raster::raster(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_waterbirds_fall.tif'))

wkey = read_csv('V:/Project/wetland/Delta/landscape_models/predrasters_baseline/baseline/VegCAMP_crosswalk.csv',
                col_types = cols()) %>%
  dplyr::select(group = WATERBIRD, code) %>%
  distinct() %>%
  mutate(label = case_when(group == 'Irrigated pasture' ~ 'ip',
                           group == 'Dryland pasture' ~ 'dryp',
                           group == 'wetland' ~ 'wet',
                           group == 'orchard' ~ 'orch',
                           group == 'alfalfa' ~ 'alf',
                           group == 'woody wetland' ~ 'woodw',
                           group == 'developed' ~ 'dev',
                           group == 'fallow' ~ 'fal',
                           TRUE ~ group)) %>%
  bind_rows(tibble(group = 'DUwetland',
                   code = 18,
                   label = 'duwet'))

# DELTA BOUNDARY
delta = read_sf('V:/Data/geopolitical/california/Legal_Delta_Boundary/Legal_Delta_Boundary.shp') %>%
  st_transform(crs = crs(veg_baseline_riparian)) %>%
  fasterize::fasterize(veg_baseline_riparian)
writeRaster(delta, 'GIS/delta', format = 'GTiff')

# RESTORATION OPPORTUNITIES ANALYSIS--------
# first combine separate layers representing restoration opportunities
filelist = paste0('GIS/DSLPT/',
                  list.files('GIS/DSLPT', 'potential.*shp$'))

#ignore potential tidal marsh for now, and focus only riparian, wetland:
tmp = do.call(rbind,
              lapply(filelist[1:2],
                     function(x) st_read(x) %>% dplyr::select(Habitat_Ty))) %>%
  filter(Habitat_Ty %in% c('valley foothill riparian',
                           'willow riparian scrub/shrub',
                           'wet meadow/seasonal wetland')) %>%
  mutate(code = case_when(Habitat_Ty == 'wet meadow/seasonal wetland' ~ 8000,
                          Habitat_Ty == 'valley foothill riparian' ~ 7100,
                          Habitat_Ty == 'willow riparian scrub/shrub' ~ 7600)) %>%
  st_transform(crs = proj4string(veg_baseline_riparian))
# Note: coding "valley foothill riparian" as POFR, but should be ~22% QULO, 21%
# POFR, 12% SALIX, 7% MIXEDFOREST; also, SALIXSHRUB_2000 left out of main models
# due to CV-wide correlation with total RIPARIAN_2000, but Delta baseline is
# ~25% SALIXSHRUB [was SALIXSHRUB_50 important to any individual spp?]


habpotential = fasterize::fasterize(tmp, veg_baseline_riparian, field = 'code') %>%
  mask(veg_baseline_riparian)
plot(habpotential)

writeRaster(habpotential, 'GIS/habpotential', format = 'GTiff')
# riparian = 7000; wetland = 8000

# # what kinds of riparian are most common?
# veg_riparian_details = raster::raster(paste0(gisdir, 'GIS/landscape_rasters/veg_riparian.tif')) %>%
#   crop(veg_baseline_riparian) %>%
#   mask(veg_baseline_riparian)
# plot(veg_riparian_details, col = viridis::plasma(n = 7))
#
# getValues(veg_riparian_details) %>% as.factor() %>% summary()
# # lots of QULO & POFR, SALIXSHRUB;
# # then SALIX, INTROSCRUB, MIXEDFOREST, AND mixedshrub a distant last; no "other"

# PRIORITY RESTORATION AREAS---------
priority = st_read('GIS/ER_P3/ER_P3.shp') %>%
  mutate(code = 100) %>%
  fasterize::fasterize(veg_baseline_riparian, field = 'code')
writeRaster(priority, 'GIS/restorationpriority', format = 'GTiff')
# priority = 100


# ZONING-----------
zoning = st_read('GIS/FINAL_City_County_LU_merged/FINAL_City_County_LU_merged.shp') %>%
  filter(GIN_CLASS %in% c('Areas Designated for Development',
                          'Open Space/Recreation',
                          'Public/Quasi-Public')) %>%
  mutate(code = case_when(GIN_CLASS == 'Areas Designated for Development' ~ 90,
                          GIN_CLASS == 'Open Space/Recreation' ~ 10,
                          GIN_CLASS == 'Public/Quasi-Public' ~ 20)) %>%
  st_transform(crs = proj4string(veg_baseline_riparian)) %>%
  fasterize::fasterize(veg_baseline_riparian, field = 'code') %>%
  mask(veg_baseline_riparian)
writeRaster(zoning, 'GIS/zoning', format = 'GTiff')
# 10 = open space/recreation; 20 = public/quasi=public; 90 = designated for development


# PROTECTED AREAS-------------
filelist = paste0('GIS/DSLPT/', list.files('GIS/DSLPT', 'protected.*shp$'))
protected = rbind(st_read(filelist[1]) %>% st_zm() %>% mutate(code = 2) %>%
                    dplyr::select(code),
                  st_read(filelist[2]) %>% mutate(code = 1) %>%
                    dplyr::select(code)
                  ) %>%
  st_transform(crs = proj4string(veg_baseline_riparian)) %>%
  fasterize::fasterize(veg_baseline_riparian, field = 'code') %>%
  mask(veg_baseline_riparian)
writeRaster(protected, 'GIS/protectedareas', format = 'GTiff')
#  1 = protected; 2 = easement




# riparian:
target_area %>% filter(code >= 7000 & code < 8000)
# 967 ha of potential riparian within area zoned for development (7090)
# --> plus 54.3 ha zoned for development that is also in protected land? (7190)

# first priority: protected land/easement and/or public land/open space + priority area
# -- includes 7011, 7101, 7111, 7201, 7211 = 621 + 81.3 + 23.1 + 15.8 + 8.01 = 749.21
# second priority: same as above but not in "priority area"?
# -- includes 7010, 7100, 7110, 7200, 7210 = 561 + 121 + 26 + 165 + 7.11 = 880.11
# third: other priority areas not zoned for development (not public/protected land)
# -- includes 7001 = 594
# total: 749.21 + 880.11 + 594 = 2223.32 ha out of 6596 in objectives (33.7%)
# --> plus 5599 of potential riparian not on public land, not in priority area,
# and not zoned for development (need 4372.68 ha of this)

# wetland:
target_area %>% filter(code >= 8000)
# 502 ha of potential wetland within area zoned for development (8090)
# --> plus 69.3 ha zoned for development that is also in protected land? (8190)

# first priority: 8011, 8101, 8111, 8201, 8211 = 11.3 + 320 + 1504 + 1683 + .36 = 3518.3
# second priority: 8010, 8020, 8100, 8110, 8120, 8200, 8210 = 110 + 20.9 + 1248 + .72 + .63 + 892 = 2272.25
# third: 8001 = 7033 (only need)
# total: 3518.3 + 2272.25 = 5790.55 out of 7689 goal (75.3%)
# --> plus 7033 ha of potential riparian in priority area but not on public land
# and not zoned for development (need 1898.45 ha of this)
# (or do you prioritize the "priority area" over the protected land/easements)

restoration_target_wetlands = reclassify(
  restoration_target,
  rcl = matrix(c(0, 8000, NA), byrow = TRUE, nrow = 1),
  include.lowest = TRUE, right = FALSE
)
restoration_target_wetlands = reclassify(
  restoration_target_wetlands,
  rcl = matrix(c(8011, 1,
                 8101, 1,
                 8111, 1,
                 8201, 1,
                 8211, 1,
                 8010, 2,
                 8020, 2,
                 8100, 2,
                 8110, 2,
                 8120, 2,
                 8200, 2,
                 8210, 2,
                 8001, 3,
                 8090, 4,
                 8190, 5),
               byrow = TRUE, ncol = 2))
writeRaster(restoration_target_wetlands, 'GIS/restoration_target_wetlands.tif')

# potential wetland within priority area & on public land/easements:
# 1824 + 1683 = 3507 ha
# plus protected land/easements not in priority area: 1319 + 892 = 2211
# = 5718 ha out of 7689 goal (74%)
# plus potential wetland within priority area not on public land: 7044
# --> this area is more than enough to meet target (would need 1326 ha)

# PROJECTED LAND USE------
skey = read_csv('GIS/State Class Rasters/key.csv')
ncell(veg_baseline_waterbird_fall[!is.na(veg_baseline_waterbird_fall)]) #7257416

scenario_stack = stack(
  paste0('GIS/State Class Rasters/',
         list.files('GIS/State Class Rasters/', 'tif$'))
  )
names(scenario_stack) = c('DUST', 'DREAM', 'HBAU', 'BBAU', 'EEM')
# in a different projection, and with a different resolution than our baseline landscape

scenario_stack_proj = scenario_stack %>%
  # first crop in the original projection to make the file smaller
  crop(veg_baseline_riparian %>%
         projectExtent(crs = proj4string(scenario_stack))) %>%
         # projectExtent(crs = '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'))
  # disaggregate to 30m pixels (staying in original projection)
  disaggregate(fact = 270/30) %>%
  # project to baseline crs; use "ngb" so we keep integer values
  projectRaster(to = veg_baseline_riparian, method = 'ngb') %>%
  mask(veg_baseline_riparian)

writeRaster(scenario_stack_proj, bylayer = TRUE, format = 'GTiff',
            paste0('GIS/projected_landuse_2100_', names(scenario_stack_proj)))

plot(scenario_stack, col = viridis::inferno(32)) #differences appear subtle
ncell(scenario_stack$DUST[!is.na(scenario_stack$DUST)]) #5771648

# scenario_stack = purrr::map(
#   paste0('GIS/', list.files('GIS', 'projected_landuse_2100')),
#   raster) %>% stack()

gv = getValues(scenario_stack$BBAU)
hist(gv)

# compare to each other and to new riparian/waterbird baseline:
tab1 = crosstab(scenario_stack$DUST, scenario_stack$DREAM)
tab_extremes = tab1 %>% as_tibble() %>%
  mutate_at(vars(DUST:DREAM), as.numeric) %>%
  left_join(skey %>% dplyr::select(code, dream_landuse = label),
            by = c('DREAM' = 'code')) %>%
  left_join(skey %>% dplyr::select(code, dust_landuse = label),
            by = c('DUST' = 'code')) %>%
  mutate(perc = n / 5771648 * 100) %>%
  arrange(desc(n))

tab_extremes %>%
  filter(!(dream_landuse == dust_landuse)) %>%
  ggplot(aes(x = dream_landuse, y = dust_landuse, fill = n)) +
  geom_tile() +
  scale_fill_gradientn(name = '', colors = topo.colors(10))

tab_extremes %>%
  filter(!(dream_landuse == dust_landuse)) %>%
  arrange(desc(n))
# largest differences are: (DREAM vs DUST)
# - dev vs. corn;  corn vs. dev
# - dev vs. orch; orch vs. dev
# - dev vs. grass/shrub; grass/shrub vs. dev
# - dev vs. seas
# --> but all differences relatively small: < 1%

tab2 = crosstab(scenario_stack$BBAU, scenario_stack$DREAM)
tab_extremes2 = tab2 %>% as_tibble() %>%
  mutate_at(vars(BBAU:DREAM), as.numeric) %>%
  left_join(skey %>% dplyr::select(code, dream_landuse = label),
            by = c('DREAM' = 'code')) %>%
  left_join(skey %>% dplyr::select(code, bbau_landuse = label),
            by = c('BBAU' = 'code')) %>%
  mutate(perc = n / 5793303 * 100) %>%
  arrange(desc(n))

tab_extremes2 %>%
  filter(!(dream_landuse == bbau_landuse)) %>%
  ggplot(aes(x = dream_landuse, y = bbau_landuse, fill = n)) +
  geom_tile() +
  scale_fill_gradientn(name = '', colors = topo.colors(10))

tab_extremes2 %>%
  filter(!(dream_landuse == bbau_landuse)) %>%
  arrange(desc(n))
# largest differences are (DREAM vs BBAU)
# - dev vs. corn; corn vs. dev (equal amts though)
# - dev vs. orchard > orch vs. dev
# - grass/shrub vs. dev > dev vs. grass/shrub
# --> but all differences relatively small: < 1%


# baseline vs. BBAU: (no restoration)
btab = crosstab(veg_baseline_waterbird_fall, scenario_stack$BBAU) %>% as_tibble()
tab_bbau = btab %>% set_names(c('baseline', 'bbau', 'n')) %>%
  mutate_at(vars(baseline:bbau), as.numeric) %>%
  left_join(skey %>% dplyr::select(code, bbau_landuse = label),
            by = c('bbau' = 'code')) %>%
  left_join(wkey %>% dplyr::select(code, baseline_landuse = label),
            by = c('baseline' = 'code')) %>%
  mutate(perc = n / 5793303 * 100) %>%
  arrange(desc(n))

tab_bbau %>%
  filter(!(bbau_landuse == 'dev' & baseline_landuse == 'dev') &
           !(bbau_landuse == 'water' & baseline_landuse == 'water') &
           !(bbau_landuse == 'grass/shrub' & baseline_landuse == 'dryp')) %>%
  # filter(!is.na(hbau_landuse)) %>% #area not included by the scenario
  ggplot(aes(x = bbau_landuse, y = baseline_landuse, fill = n)) +
  geom_tile() +
  scale_fill_gradientn(name = '', colors = topo.colors(10))

tab_bbau %>% filter(!is.na(bbau_landuse) & bbau_landuse != baseline_landuse) %>%
  arrange(desc(n))
# largest changes (>1%) are: (BBAU vs baseline)
# - seas vs. corn (3.8%) (?)
# - dev vs. orch (2.5%)
# - rice vs. ip (2.4%)
# - corn vs. orch (1.9%)
# - orch vs. alf (1.9%)
# - dev vs. dryp (1.5%)
# - corn vs. row (1.5%)
# - corn vs. alf (1.2%)

# total acres? but remember that scenarios exclude southwest corner
veg_baseline_mask = mask(veg_baseline_waterbird_fall, scenario_stack$BBAU)

btot = getValues(veg_baseline_mask) %>% as.factor() %>% summary() %>%
  as_tibble(rownames = 'code') %>%
  mutate(code = as.numeric(code),
         area.ha = value * 30 * 30 / 10000) %>%
  filter(!is.na(code))

bbautot = getValues(scenario_stack$BBAU) %>% as.factor() %>% summary() %>%
  as_tibble(rownames = 'code') %>%
  mutate(code = as.numeric(code),
         area.ha = value * 30 * 30 / 10000) %>%
  filter(!is.na(code))

dusttot = getValues(scenario_stack$DUST) %>% as.factor() %>% summary() %>%
  as_tibble(rownames = 'code') %>%
  mutate(code = as.numeric(code),
         area.ha = value * 30 * 30 / 10000) %>%
  filter(!is.na(code))

dreamtot = getValues(scenario_stack$DREAM) %>% as.factor() %>% summary() %>%
  as_tibble(rownames = 'code') %>%
  mutate(code = as.numeric(code),
         area.ha = value * 30 * 30 / 10000) %>%
  filter(!is.na(code))

bind_rows(
  btot %>%
    left_join(wkey) %>%
    mutate(label = case_when(label %in% c('dryp', 'woodw') ~ 'dryp & woodw',
                             label %in% c('fal', 'row', 'field') ~ 'fal & row & field',
                             label %in% c('grain', 'wheat') ~ 'grain & wheat',
                             TRUE ~ label)) %>%
    group_by(label) %>%
    summarize(area.ha = sum(area.ha)) %>%
    mutate(source = 'baseline'),
  bbautot %>%
    left_join(skey) %>%
    mutate(label = case_when(label %in% c('semiperm', 'seas') ~ 'duwet',
                             label == 'wetother' ~ 'wet',
                             label == 'grass/shrub' ~ 'dryp & woodw',
                             label == 'row' ~ 'fal & row & field',
                             label == 'pasture' ~ 'ip',
                             label == 'grain' ~ 'grain & wheat',
                             TRUE ~ label)) %>%
    group_by(label) %>%
    summarize(area.ha = sum(area.ha)) %>%
    mutate(source = 'BBAU'),
  dusttot %>%
    left_join(skey) %>%
    mutate(label = case_when(label %in% c('semiperm', 'seas') ~ 'duwet',
                             label == 'wetother' ~ 'wet',
                             label == 'grass/shrub' ~ 'dryp & woodw',
                             label == 'row' ~ 'fal & row & field',
                             label == 'pasture' ~ 'ip',
                             label == 'grain' ~ 'grain & wheat',
                             TRUE ~ label)) %>%
    group_by(label) %>%
    summarize(area.ha = sum(area.ha)) %>%
    mutate(source = 'DUST'),
  dreamtot %>%
    left_join(skey) %>%
    mutate(label = case_when(label %in% c('semiperm', 'seas') ~ 'duwet',
                             label == 'wetother' ~ 'wet',
                             label == 'grass/shrub' ~ 'dryp & woodw',
                             label == 'row' ~ 'fal & row & field',
                             label == 'pasture' ~ 'ip',
                             label == 'grain' ~ 'grain & wheat',
                             TRUE ~ label)) %>%
    group_by(label) %>%
    summarize(area.ha = sum(area.ha)) %>%
    mutate(source = 'DREAM')
) %>%
  ggplot(aes(label, area.ha/1000, fill = source)) +
  geom_col(position = position_dodge())

# biggest changes from baseline:
# ALL SCENARIOS: more dev, less dryp & woodw, less ip, less water, more wet
# BBAU: more orch; less alf, corn, fal/row/field, grain/wheat, rice
# DREAM: more orch (but less of an increase), duwet; less alf, corn,
#    fal/row/field, grain/wheat, rice
# DUST: more alf, corn; less orch; less reduction in dryp/woodw, grain/wheat, ip, rice

#--> note that riparian is lost completely at this resolution, and/or lumped in
# with grass/shrub; propose to overlay baseline riparian, and possibly
# dev, orchard, du wetlands, water just to make sure these don't change.

#--> other issue: missing southwest corner and 10km buffer. A lot of it is
# developed and then grasslands in the hills; can just fill in and assume no
# change?

#--> plus: field/row crops combined; grain/winter wheat combined and no way to
# pull out separate winter map

scenario_bbau_waterbirds <- reclassify(scenario_stack_proj$BBAU,
                                       rcl = matrix(
                                         c(1, 14, #water
                                           2, 12, #dev
                                           3, 99, #barren
                                           4, 13, #forest
                                           5, 16, #grass/shrub - dryp
                                           10, 6, #field/row - row
                                           11, 4, #pasture - ip
                                           12, 17, #alf
                                           13, 10, #grain
                                           14, 2, #corn
                                           15, 3, #rice
                                           20, 9, #orch/vin
                                           30, 18, #semiperm - DUwet
                                           31, 18, #seas - DUwet
                                           32, 8 #other/general wetland
                                           ),
                                         byrow = TRUE, ncol = 2)
                                       )

# allow certain baseline pixels to over-rule scenario pixels: orch (9), dev
# (12), water (14), woodw (15), duwet (18)
scenario_bbau_waterbirds_overrule <- overlay(
  x = scenario_bbau_waterbirds,
  y = veg_baseline_waterbird_fall,
  fun = function(x, y) {
    ifelse(!is.na(y) & y %in% c(9, 12, 14, 15, 18), y, x)
  })

hist(scenario_bbau_waterbirds_overrule, col = 'red', breaks = c(1:19,100))
hist(scenario_bbau_waterbirds, breaks = c(1:19,100), add = TRUE) # FOR COMPARISON: a few more orchard pixels added
stack(scenario_bbau_waterbirds, scenario_bbau_waterbirds_overrule) %>%
  plot(col = hcl.colors(n = 18, palette = 'Set 2'), colNA = 'gray80')

# fill in baseline values where missing (primarily in southwest corner)
scenario_bbau_waterbirds_fill = cover(scenario_bbau_waterbirds_overrule, veg_baseline_waterbird_fall)
stack(scenario_bbau_waterbirds, scenario_bbau_waterbirds_fill) %>%
  plot(col = hcl.colors(n = 18, palette = 'Set 2'), colNA = 'gray90')
hist(scenario_bbau_waterbirds_fill, breaks = c(seq(0,20,1), 100))
hist(scenario_bbau_waterbirds, add = TRUE, col = 'red', breaks = c(seq(0,20,1), 100))

writeRaster(scenario_bbau_waterbirds_fill,
            'data/proposed_scenarios/waterbirds/projected_landuse_2100_BBAU',
            format = 'GTiff')

scenario_bbau_riparian <- reclassify(scenario_bbau_waterbirds_fill,
                                     rcl = matrix(c(2, 20, #corn = AG
                                                    3, 30, #rice = RICE
                                                    4, 50, #ip = GRASSPAS
                                                    5, 20, #wheat = AG
                                                    6, 20, #row = AG
                                                    7, 20, #field = AG
                                                    8, 80, #wet = WETLAND
                                                    9, 10, #orch = ORCHVIN
                                                    10, 20, #grain = AG
                                                    11, 40, #fallow = IDLE
                                                    12, 60, #dev = URBAN
                                                    13, 100, #forest = OAKWOODLAND/WOODLAND/SCRUB
                                                    14, 90, #water = WATER
                                                    15, 70, #woodw = RIPARIAN
                                                    16, 50, #dryp = GRASSPAS
                                                    17, 50, #alfalfa = GRASSPAS
                                                    18, 80, #duwet = WETLAND
                                                    99, 130), #barren = BARREN
                                         byrow = TRUE, ncol = 2)
)
plot(scenario_bbau_riparian)
writeRaster(scenario_bbau_riparian_fill,
            'data/proposed_scenarios/riparian/projected_landuse_2100_BBAU',
            format = 'GTiff')

# SAGA--------

# # test raster with values of 0.09 (representing the area in hectares of a pixel)
# r <- sample(c(0.09, 0), 10000, replace = TRUE, prob = c(0.4, 0.6)) %>%
#   matrix(byrow = TRUE, ncol = 100) %>%
#   raster()
# plot(r)

# bigr <- raster(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_riparian/RIPARIAN.tif'))
#
# library(Rsagacmd)
# saga <- saga_gis('C:/Program Files/saga-7.9.0_x64/saga_cmd.exe')
#
# test_focal_50 = saga$statistics_grid$focal_statistics(
#   grid = bigr,
#   sum = 'GIS/focal50test.tif',
#   # mean = 'file',
#   kernel_type = 1, #circle, the default?
#   kernel_radius = 1 #in number of cells
#   )
# test_focal_2k = saga$statistics_grid$focal_statistics(
#   grid = bigr,
#   sum = 'GIS/focal2ktest.tif',
#   # mean = 'file',
#   kernel_type = 1, #circle, the default?
#   kernel_radius = 67 #in number of cells
# ) #VERY SLOW!

bigr <- terra::rast(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_riparian/RIPARIAN.tif'))
bigr_edit = terra::classify(bigr, rcl = matrix(c(0.08, 0.1, 1), byrow = TRUE, nrow = 1))
plot(bigr_edit)

wt = terra::focalMat(bigr, d = 2000, type = 'circle')

memory.limit(size = 80000)
test_focal_2k = focal(bigr, w = wt, fun = 'sum', na.rm = TRUE,
                      filename = 'GIS/focal2ktest_terra.tif')

# FLOOD RISK-----------
# Delta Adapts flood risk

# first combine separate layers representing flood risk
filelist = paste0('GIS/DeltaAdapts/',
                  list.files('GIS/DeltaAdapts',
                             'baseline_.*shp$|floodrisk2085_.*shp$'))
baseline_rast = raster::raster('data/landcover_waterbirds_fall/baseline_waterbird_fall.tif')
tmp = purrr::map(filelist,
                 ~st_read(.) %>%
                   fasterize::fasterize(veg_baseline_riparian,
                                        field = 'flood_risk')) %>%
  stack()
maxfloodrisk = max(tmp, na.rm = TRUE)
plot(tmp)
plot(maxfloodrisk)

key = data.frame(id = c(0:4),
                 label = c('very low', 'low', 'medium', 'high', 'very high'),
                 risk.annual = c('<0.5%', '0.5-1%', '1-2%', '2-10%', '>10%'),
                 col = c('lightskyblue', 'dodgerblue', 'royalblue', 'blue3', 'midnightblue'))

maxfloodrisk = terra::rast(maxfloodrisk)
levels(maxfloodrisk) <- key
coltab(maxfloodrisk) <- key %>% select(id, col) %>%
  complete(id = c(0:255)) %>% pull(col)
plot(maxfloodrisk)

writeRaster(maxfloodrisk, 'GIS/floodrisk2085.tif')


# suitable veg per elevation:
elev = read_csv('data/SFEI_DSLPT/elevation_notifications_v5.csv')

elev %>%
  dplyr::select(Habitat_type, Elevation_band, Flag_type) %>%
  filter(Habitat_type != 'Stabilized interior dune vegetation') %>%
  pivot_wider(values_from = Flag_type, names_from = Elevation_band) %>%
  dplyr::select(Habitat_type, `-2`, `-1`, `0`, `1`, `2`)

elev %>%
  dplyr::select(Habitat_type, Elevation_zone_DeltaPlan, Flag_type) %>%
  filter(Habitat_type != 'Stabilized interior dune vegetation') %>%
  pivot_wider(values_from = Flag_type, names_from = Elevation_zone_DeltaPlan) %>%
  dplyr::select(Habitat_type, 'Deeply subsided (more than 8 ft below MLLW)',
                'Shallowly subsided (up to 8 feet below MLLW)',
                'Intertidal (between MLLW and MHHW)',
                'Sea level rise projection (0 to +10 ft MHHW)',
                'Floodplain (more than 10 feet MHHW)')


elev %>%
  dplyr::select(Elevation_zone_SFEI, Elevation_zone_DeltaPlan) %>%
  table()
# SFEI: Terrestrial; DeltaPlan: Deeply subsided?
# SFEI: Terrestrial, DeltaPlan: Intertidal


# DSLPT: EcoRestore----------
ecorestore = read_sf('GIS/DSLPT/DSLPT_EcoRestore.shp')
ecorestore_rast = ecorestore %>%
  mutate(shortlab = recode(Habitat_Ty,
                        grassland = 'dryp',
                        'open water' = 'water',
                        'valley foothill riparian' = 'woodw',
                        'non-tidal freshwater emergent wetland' = 'duwet',
                        'tidal freshwater emergent wetland' = 'wet',
                        'wet meadow/seasonal wetland' = 'duwet')) %>%
  left_join(wkey %>% select(shortlab, code), by = 'shortlab') %>%
  st_transform(crs = crs(veg_baseline_waterbird_fall)) %>%
  fasterize::fasterize(raster::raster(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_waterbirds_fall.tif')),
                       field = 'code')
writeRaster(ecorestore_rast, 'GIS/DSLPT_Ecorestore.tif')

