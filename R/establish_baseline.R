# README---------
# After initial distribution modeling of both riparian landbirds and waterbird,
# and creation of a common baseline veg layer for use with Zonation
# prioritization (reflected in the report to CDFW), we decided we needed to
# create a more representative baseline veg layer for use as a baseline in the
# scenario analyses that does not include so much fallow ag land (lots of fallow
# in 2014 Land IQ during drought). We also needed to better identify different
# types of wetlands: tidal, non-tidal, and "managed".

# PACKAGES & FUNCTIONS
source('R/packages.R')
source('R/functions.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
delta_shp = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
delta_buff10k = read_sf('GIS/boundaries/Legal_Delta_boundary_buff10k.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
template = rasterize(vect(delta_buff10k), extend(delta, delta_buff10k))

key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')

# REFINE BASELINE SHP---------
# start with revised primary veg layer - initially created for riparian
# landbirds (stitched together from VegCAMP, broader CV layer, and LandIQ 2014),
# manually updated to include:
# - River Islands development
# - marking "group 1" field  as "WETWATER" for water bodies within/adjacent to
# off-channel wetlands that should be counted as part of the wetland
# - labeling "comment" field to identify probable managed wetlands as SEASONAL
# or SEMIPERM (cross-referencing with water tracker data on frequency of
# inundation and DU layer)
# - labeling "comment" field as SUBSIDENCE REVERSAL for managed wetlands known
# to be specifically aimed at this (often dense emergent veg, so little shows up
# on water tracker)
# - labeling all other WETLAND and WETWATER polygons as CHANNEL (directly
# adjacent to or surrounded by a water channel) or UNMANAGED (off-channel and
# apparently not a managed wetland, often labeled wet meadow and some called
# irrigated pasture by Land IQ)
# - re-labeling CHANNEL wetlands as TIDAL MARSH (fresh or salt), by
# cross-referencing with the National Wetland Inventory layer; remaining CHANNEL
# wetlands are assumed not tidal, e.g. on floodplains or sometimes they are the
# channel themselves (narrow)
# - Also incorporated some replacements of XXX or IDLE with more recent LandIQ
# info from 2018 by updating Crp2014 field in addition to group1
# - keep most detailed classfication possible, for potential use with multiple
# benefits data; can lump together for use with waterbird and riparian models

veg_primary = read_sf('GIS/VEG_Delta10k_update.shp') %>%
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs") %>%
  st_make_valid() %>%
  st_cast(to = 'MULTIPOLYGON')
# Note that typically only group1, comment, and sometimes Crp2014 were manually
# updated, so CWHRCOD, DWR_Stn, group2, and code fields may not match
# --> exception: 3 polygons edited to show as riparian; updated CWHRCOD and
# NVCS_Nm for these to match adjacent riparian polygons (for use in identifying
# riparian sub-types)

# clean up land cover groupings and comments:
veg_clean = veg_primary %>% cleanup_landcovers() %>%
  filter(!is.na(CLASS)) %>% #empty geometry
  select(CLASS, CROP = Crp2014, CWHRCOD, NVCS_Nm)

veg_clean %>% st_drop_geometry() %>% select(CLASS, CROP) %>% distinct() %>%
  arrange(CLASS, CROP) %>% print(n = 61)

write_sf(veg_clean, 'GIS/VEG_Delta10k_baseline.shp', append = FALSE)

# CREATE BASELINE RASTER---------
# generic raster version of baseline shp, coded to include the most detailed
# land cover classes expected to be needed in the models and scenario analyses

veg_codify = veg_clean %>%
  codify_baseline(
    codekey = key) %>%
  select(CODE_BASELINE, CLASS, everything())

veg_codify %>% st_drop_geometry() %>% select(CODE_BASELINE, CLASS) %>%
  distinct() %>% arrange(CODE_BASELINE) %>% print(n = 64)

veg_raster = rasterize(vect(veg_codify), template, field = 'CODE_BASELINE')
writeRaster(veg_raster, 'GIS/landscape_rasters/veg_baseline.tif',
            overwrite = TRUE)

## NASS------
# fill in rest of 10k buffer with NASS raster from 2014
# (same as used in waterbird study)
NASS18 = rast('GIS/original_source_data/CDL/CDL_2018_clip_20211108202757_1802460488.tif') %>%
  crop(template) %>% mask(template) %>%
  codify_NASS(season = 'fall', codekey = key)

plot(NASS18)
freq(NASS18)

# NASS14_fall_bufferfill = rast('GIS/landscape_rasters/CDL_2014_06_fall_prj.tif') %>%
#   crop(template) %>% mask(template, updatevalue = NA) %>%
#   # recode from CODE_WATERBIRD to CODE_BASELINE
#   classify(rcl = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx') %>%
#              select(from = CODE_WATERBIRD, to = CODE_BASELINE) %>%
#              filter(!is.na(from)) %>% arrange(from) %>% as.matrix())
# plot(NASS14_fall_bufferfill)

# update coding for wetlands in Suisun using National Wetlands Inventory to
# separate tidal vs. managed wetlands
nwi = read_sf('GIS/original_source_data/ds2630/ds2630_Delta10k_subset.shp') %>%
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs") %>%
  mutate(code = case_when(NWI_group == 'salt tidal marsh' ~ 86,
                          TRUE ~ 82))
nwi_rast = rasterize(vect(nwi), template, field = 'code')

NASS18_wet = lapp(c(NASS18, nwi_rast),
                  fun = function(x, y) {
                    ifelse(!is.na(y) & x == 80,
                           y,
                           ifelse(x == 80,
                                  87,
                                  x))
                  })
crosstab(c(NASS18, NASS18_wet), useNA = TRUE, long = TRUE)
# check that only values of 80 switch to 82 (seasonal), 86 (tidal), or 87 (other)
# --> note that NASS data includes generic perennial crops and riparian categories

# let NASS layer fill in blank space behind veg_raster
veg_raster_fill = merge(veg_raster, NASS18_wet)
freq(veg_raster_fill)
plot(c(veg_raster, veg_raster_fill))
writeRaster(veg_raster_fill,
            'GIS/landscape_rasters/veg_baseline_fillNASS.tif',
            overwrite = TRUE)

## Land IQ update------
# overlay edited veg data with more recent Land IQ data to replace
# idle/fallow/orchard stringers with actual crop types; also allow more recent
# orchard to overwrite annual crops

landiq_2018 = read_sf('GIS/original_source_data/i15_crop_mapping_2018_shp/i15_Crop_Mapping_2018_deltabuff10k.shp') %>%
  st_make_valid() %>%
  st_transform(crs = st_crs(delta_buff10k)) %>%
  st_cast(to = 'MULTIPOLYGON')

metadat_2018 = readxl::read_excel(
  'GIS/original_source_data/i15_crop_mapping_2018_shp/metadata.xlsx',
  na = 'NA')

# codify landiq codes to same groupings as veg_raster, but separately
# identifying summer/fall vs. winter crops
landiq_2018_simplify = landiq_2018 %>%
  codify_landiq(meta = metadat_2018,
                codekey = key)

# get acreage of specific crop types within each class within the Delta boundary
ag_details = landiq_2018_simplify %>%
  st_make_valid() %>%
  st_intersection(delta_shp) %>%
  select(CODE_BASELINE, CLASS, CLASS_MAIN, SUBCLASS_MAIN) %>%
  mutate(area_ha = as.numeric(st_area(.))/10000) %>%
  st_drop_geometry() %>%
  group_by(CODE_BASELINE, CLASS, SUBCLASS_MAIN) %>%
  summarize(area_ha = sum(area_ha), .groups = 'drop') %>%
  group_by(CODE_BASELINE, CLASS) %>%
  mutate(total_area = sum(area_ha)) %>%
  ungroup() %>%
  mutate(prop = area_ha/total_area)
write_csv(ag_details, 'data/landiq2018_area_detail.csv')
# orchard_citrus&subtropical: 97% olives
# orchard_deciduous: 51% almonds, 14% pears, 13% walnuts, 11% unknown?, 7% cherries
# row: 59% tomatoes (processing); 15% cucurbits; 9% potato
# field (other than corn): 61% safflower, 25% beans (dry), 11% sunflowers

landiq_2018_simplify %>% st_drop_geometry() %>%
  select(CODE_BASELINE, CLASS, CLASS_MAIN, SUBCLASS_MAIN) %>% distinct() %>%
  arrange(CODE_BASELINE, CLASS_MAIN, SUBCLASS_MAIN) %>% print(n = 50)
landiq_2018_simplify %>% st_drop_geometry() %>%
  select(CODE_BASELINE_WINTER, CLASS_WINTER, SUBCLASS_WINTER) %>% distinct() %>%
  arrange(CODE_BASELINE_WINTER, CLASS_WINTER, SUBCLASS_WINTER) %>% print(n = 50)
landiq_2018_simplify %>% st_drop_geometry() %>%
  select(CLASS, CLASS_WINTER) %>% distinct() %>% table()
# fall to winter transitions do NOT include any changes for developed, fallow,
#  or orchard, but do include rice to alfalfa or grain (?)

landiq_2018_stack = c(rasterize(vect(landiq_2018_simplify), template,
                                field = 'CODE_BASELINE'),
                      rasterize(vect(landiq_2018_simplify), template,
                                field = 'CODE_BASELINE_WINTER'))
names(landiq_2018_stack) = c('landiq_2018_fall',
                             'landiq_2018_winter')
writeRaster(landiq_2018_stack,
            filename = paste0('GIS/landscape_rasters/',
                              names(landiq_2018_stack), '.tif'),
            overwrite = TRUE)
plot(landiq_2018_stack)

### fall baseline--------
# fill in ag data from Land IQ:
# where baseline raster is "fallow" or any annual crop, fill in corresponding
# value from land iq 2018
veg_raster_fill_landiq = lapp(c(veg_raster_fill,
                                landiq_2018_stack$landiq_2018_fall),
                              fun = function(x, y) {
                                ifelse(x %in% c(20:56, 130) &
                                         !is.na(y), y, x)
                              })
writeRaster(veg_raster_fill_landiq,
            filename = 'GIS/landscape_rasters/veg_baseline_fall.tif',
            overwrite = TRUE)

# compare to original:
fall_stack = c(veg_raster_fill,
               veg_raster_fill_landiq)
levels(fall_stack[[1]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
levels(fall_stack[[2]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(fall_stack[[1]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
coltab(fall_stack[[2]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(fall_stack) = c('baseline', 'lyr1')
plot(fall_stack, colNA = 'gray90')

tab = crosstab(fall_stack, useNA = TRUE, long = TRUE) %>%
  rlang::set_names(c('baseline', 'new', 'n'))
tab %>%
  left_join(key %>% select(baseline = CODE_BASELINE, from = CODE_NAME)) %>%
  left_join(key %>% select(new = CODE_BASELINE, to = CODE_NAME)) %>%
  arrange(from, desc(n))
tab %>%
  left_join(key %>% select(baseline = CODE_BASELINE, from = CODE_NAME)) %>%
  left_join(key %>% select(new = CODE_BASELINE, to = CODE_NAME)) %>%
  group_by(from) %>%
  mutate(total_baseline = sum(n),
         prop = n/total_baseline) %>%
  ungroup() %>%
  filter(from == to) %>% arrange(desc(prop)) %>%
  print(n = 30)
# 100% agreement on: perennial crops, riparian, wetlands, water, woodland&scrub,
# (none of these allowed to change); high (>80%) agreement on rice, grassland,
# pasture, barren; less agreement on idle fields & annual crops (makes sense)

### winter baseline----------
# where baseline raster is suitable for a second winter crop (annual crops
# besides rice, fallow, grassland & pasture, barren), fill in corresponding
# winter value from land iq 2018
veg_raster_fill_landiq_win = lapp(c(veg_raster_fill_landiq,
                                    landiq_2018_stack$landiq_2018_winter),
                                  fun = function(x, y) {
                                    ifelse(x %in% c(21:28, 40, 51:56, 130) &
                                             !is.na(y), y, x)
                                  })

writeRaster(veg_raster_fill_landiq_win,
            filename = 'GIS/landscape_rasters/veg_baseline_winter.tif',
            overwrite = TRUE)

# compare to fall:
win_stack = c(veg_raster_fill_landiq,
              veg_raster_fill_landiq_win)
levels(win_stack[[1]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
levels(win_stack[[2]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(win_stack[[1]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
coltab(win_stack[[2]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(win_stack) = c('baseline', 'lyr1')
plot(win_stack, colNA = 'gray90')

tab2 = crosstab(win_stack, useNA = TRUE, long = TRUE) %>%
  rlang::set_names(c('baseline', 'new', 'n'))
tab2 %>%
  left_join(key %>% select(baseline = CODE_BASELINE, from = CODE_NAME)) %>%
  left_join(key %>% select(new = CODE_BASELINE, to = CODE_NAME)) %>%
  filter(from != to) %>%
  arrange(from, desc(n))
tab2 %>%
  left_join(key %>% select(baseline = CODE_BASELINE, from = CODE_NAME)) %>%
  left_join(key %>% select(new = CODE_BASELINE, to = CODE_NAME)) %>%
  group_by(from) %>%
  mutate(total_baseline = sum(n),
         prop = n/total_baseline) %>%
  ungroup() %>%
  filter(from == to) %>% arrange(desc(prop)) %>%
  print(n = 30)
# swaps among: pasture/alfalfa, grain/wheat, row, field, corn (except nothing
# converts to corn)


# waterbird key---------
# wkey = veg_codify %>% st_drop_geometry() %>%
#   select(WATERBIRD_CLASS, WATERBIRD_CODE) %>%
#   distinct() %>%
#   arrange(WATERBIRD_CODE) %>%
#   mutate(shortlab = case_when(WATERBIRD_CLASS == 'Irrigated pasture' ~ 'ip',
#                               WATERBIRD_CLASS == 'Dryland pasture' ~ 'dryp',
#                               WATERBIRD_CLASS == 'wetland' ~ 'wet',
#                               WATERBIRD_CLASS == 'orchard' ~ 'orch',
#                               WATERBIRD_CLASS == 'alfalfa' ~ 'alf',
#                               WATERBIRD_CLASS == 'woody wetland' ~ 'woodw',
#                               WATERBIRD_CLASS == 'developed' ~ 'dev',
#                               WATERBIRD_CLASS == 'fallow' ~ 'fal',
#                               WATERBIRD_CLASS == 'DU wetland' ~ 'duwet',
#                               WATERBIRD_CLASS == 'tidal marsh' ~ 'tidal',
#                               TRUE ~ as.character(WATERBIRD_CLASS)),
#          label = recode(shortlab,
#                         'ip' = 'pasture',
#                         'wheat' = 'grain',
#                         'row' = 'field/row',
#                         'field' = 'field/row',
#                         'wet' = 'other wetland',
#                         'orch' = 'orchard/vineyard',
#                         'fal' = 'fallow',
#                         'dev' = 'urban',
#                         'woodw' = 'riparian',
#                         'dryp' = 'grassland',
#                         'alf' = 'alfalfa',
#                         'duwet' = 'managed wetland',
#                         'tidal' = 'tidal marsh',
#                         'forest' = 'forest/shrub'),
#          col = c('#FFFF00', '#FF1493', '#2E8B57', '#FFA54F', '#FA8072',
#                  '#FA8072', '#40E0D0', '#9400D3', '#FFA54F', '#CCCCCC',
#                  '#4D4D4D', '#8B4513', '#87CEFA', '#FF0000', '#FFE4C4',
#                  '#32CD32', '#4169E1', '#EDDD82',
#                  # '#40E0D0',
#                  '#FFFFFF'))
# write_csv(wkey, 'data/landcover_key_waterbirds.csv')
#
#
#
#
# # RIPARIAN BASELINE-------------
#
# ## reclassify the new baseline layer for riparian landcover categories
# veg_raster_rip = classify(veg_raster_fill_landiq,
#                          rcl = matrix(c(2, 20, #corn = AG
#                                         3, 30, #rice = RICE
#                                         4, 50, #ip = GRASSPAS
#                                         5, 20, #wheat = AG
#                                         6, 20, #row = AG
#                                         7, 20, #field = AG
#                                         8, 80, #wet = WETLAND
#                                         9, 10, #orch = ORCHVIN
#                                         10, 20, #grain = AG
#                                         11, 40, #fallow = IDLE
#                                         12, 60, #dev = URBAN
#                                         13, 100, #forest = OAKWOODLAND/WOODLAND/SCRUB
#                                         14, 90, #water = WATER
#                                         15, 70, #woodw = RIPARIAN
#                                         16, 50, #dryp = GRASSPAS
#                                         17, 50, #alfalfa = GRASSPAS
#                                         18, 80, #duwet = WETLAND
#                                         19, 80, #tidal marsh = WETLAND
#                                         99, 130), #barren = BARREN
#                                       byrow = TRUE, ncol = 2)
# )
#
# writeRaster(veg_raster_rip,
#             filename = 'GIS/landscape_rasters/veg_baseline_riparian.tif',
#             overwrite = TRUE)
#
# ## riparian keys----------
# rkey = read_csv('GIS/landscape_rasters/rkey.csv', col_types = cols()) %>%
#   filter(!code %in% c(110, 120, 999)) %>%
#   select(-type) %>%
#   rename(RIPARIAN_CLASS = group, RIPARIAN_CODE = code) %>%
#   mutate(label = recode(RIPARIAN_CLASS,
#                         'ORCHVIN' = 'orchard/vineyard',
#                         'RICE' = 'rice',
#                         'AG' = 'other crops',
#                         'IDLE' = 'fallow',
#                         'GRASSPAS' = 'grassland/pasture',
#                         'URBAN' = 'urban',
#                         'RIPARIAN' = 'riparian',
#                         'WETLAND' = 'wetland',
#                         'WATER' = 'water',
#                         'OAKWOODLAND' = 'forest',
#                         'BARREN' = 'barren',
#                         # riparian subtypes:
#                         'POFR' = 'cottonwood forest',
#                         'QULO' = 'oak forest',
#                         'SALIX' = 'willow forest',
#                         'MIXEDFOREST' = 'mixed forest',
#                         'INTROSCRUB' = 'introduced scrub',
#                         'SALIXSHRUB' = 'willow shrub',
#                         'MIXEDSHRUB' = 'mixed shrub',
#                         'OTHER' = 'other riparian'),
#          col = c('#9400D3', '#32CD32', '#FF1493', '#CCCCCC', '#FFE4C4',
#                  '#4D4D4D', '#FF0000', '#00008B', '#87CEFA', '#8B4513',
#                  '#FFFFFF',
#                  # riparian subtypes:
#                  '#32CD32', '#8B4513', '#FF1493', '#9400D3',
#                  '#CCCCCC', '#FF0000', '#FFA54F', '#FFFFFF',
#                  # wetland subtype:
#                  '#00008B'))
# write_csv(rkey, 'data/landcover_key_riparian.csv')
#
#
# ## riparian detail----------
#
# # classify updated baseline veg layer with riparian detail:
# veg_codify_ripdetail = veg_codify %>% codify_ripdetail() %>%
#   vect() %>%
#   rasterize(template, field = 'RIPDETAIL_CODE')
#
# # fill in rest of delta 10k buffer with riparian pixels from NASS layer
# veg_codify_ripdetail_fill = merge(veg_codify_ripdetail,
#                                   classify(NASS14_fall_bufferfill_wet,
#                                            rcl = matrix(c(15, 78), ncol = 2),
#                                            othersNA = TRUE))
# tab4 = crosstab(c(veg_codify_ripdetail, veg_codify_ripdetail_fill),
#                 useNA = TRUE, long = TRUE)
# # added 11,061 pixels of "other" riparian
#
# writeRaster(veg_codify_ripdetail_fill,
#             filename = 'GIS/landscape_rasters/veg_baseline_ripdetail.tif',
#             overwrite = TRUE)
#
#
# ## wetland detail----------
# veg_codify_wetdetail = veg_codify %>%
#   filter(WATERBIRD_CLASS == 'DU wetland' & comment == 'SEMIPERM') %>%
#   mutate(WETDETAIL = 'PERM',
#          WETDETAIL_CODE = 81) %>%
#   vect() %>%
#   rasterize(template, field = 'WETDETAIL_CODE')
#
# # --> assume no perm/semiperm pixels in rest of 10k buffer (from NASS)
#
# writeRaster(veg_codify_wetdetail,
#             filename = 'GIS/landscape_rasters/veg_baseline_wetdetail.tif',
#             overwrite = TRUE)
#
#
# # MULTIPLE BENEFITS BASELINE-------
# mkey = tibble(MB_CLASS = c('orchard/vineyard', 'citrus', 'orchard', 'vineyard',
#                         'row/field', 'corn', 'grains', 'cotton',
#                         'tomato', 'rice', 'grassland/pasture', 'alfalfa',
#                         'riparian', 'wetland', 'other'),
#               MB_CODE = c(10, 11, 12, 13,
#                        21, 22, 23, 24,
#                        25, 30, 50, 51,
#                        70, 80, 90),
#               col = c('#9400D3', NA, NA, NA,
#                       '#FA8072', '#FFFF00', '#FFA54F', NA,
#                       NA, '#FF1493', '#FFE4C4', '#32CD32',
#                       '#FF0000', '#00008B', '#4D4D4D'))
# write_csv(mkey, 'data/landcover_key_multiplebenefits.csv')
#
# veg_baseline_mb = DeltaMultipleBenefits::reclassify_multiplebenefits(veg_raster_fill_landiq)
#
# writeRaster(veg_baseline_mb,
#             'data/landcover_multiplebenefits/baseline_mb.tif',
#             overwrite = TRUE)
#
#
# # FULL BASELINE STACK----------
# # ensure alignment, set levels and coltab
#
# baseline = c(veg_raster_fill_landiq,
#              veg_raster_fill_landiq_win,
#              veg_raster_rip,
#              veg_codify_ripdetail,
#              veg_codify_wetdetail,
#              veg_baseline_mb)
#
# key1 = wkey %>% select(WATERBIRD_CODE, label) %>% as.data.frame()
# key2 = rkey %>% select(RIPARIAN_CODE, label) %>% as.data.frame()
# key3 = mkey %>% select(MB_CODE, label = MB_CLASS) %>% as.data.frame
# levels(baseline) <- list(key1, key1, key2, key2, key2, key3)
#
# col1 = wkey %>% select(WATERBIRD_CODE, col) %>%
#   complete(WATERBIRD_CODE = c(0:255)) %>% pull(col)
# col2 = rkey %>% select(RIPARIAN_CODE, col) %>%
#   complete(RIPARIAN_CODE = c(0:255)) %>% pull(col)
# col3 = mkey %>% select(MB_CODE, col) %>%
#   complete(MB_CODE = c(0:255)) %>% pull(col)
#
# coltab(baseline, layer = 1) = col1
# coltab(baseline, layer = 2) = col1
# coltab(baseline, layer = 3) = col2
# coltab(baseline, layer = 4) = col2
# coltab(baseline, layer = 5) = col2
# coltab(baseline, layer = 6) = col3
#
# names(baseline) = c('veg_baseline_fall', 'veg_baseline_winter',
#                     'veg_baseline_riparian', 'veg_baseline_ripdetail',
#                     'veg_baseline_wetdetail', 'veg_baseline_mb')
#
# plot(baseline)
#
# writeRaster(baseline,
#             filename = paste0('GIS/landscape_rasters/',
#                               names(baseline), '.tif'),
#             overwrite = TRUE)

# SPLIT BY VALUE--------
# # convert individual cover types into separate layers in a raster stack for use
# # in python focal stats (as an alternative to processing in R); each pixel of
# # the corresponding cover type will contain the value of the area that pixel
# # (0.09 ha)
#
# wkey_update = bind_rows(wkey,
#                         tibble(group = c('DUwetland_tidal', 'wetland_tidal'),
#                                code = c(19, 20),
#                                label = c('duwet_tidal', 'wet_tidal'))
#                         )
#
# split_raster(wkey$code, wkey$label,
#              fall_mode_fill,
#              path = paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_waterbirds_fall'),
#              format = 'GTiff', overwrite = TRUE)
#
# split_raster(wkey$code, wkey$label,
#              winter_mode,
#              path = paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_waterbirds_winter'),
#              format = 'GTiff', overwrite = TRUE)
#
# split_raster(rkey %>% filter(type == 'veg' & group != 'XXX') %>% pull(code),
#              rkey %>% filter(type == 'veg' & group != 'XXX') %>% pull(group),
#              rip_mode,
#              path = paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_riparian'),
#              format = 'GTiff', overwrite = TRUE)
#
# split_raster(rkey %>% filter(type == 'rip') %>% pull(code),
#              rkey %>% filter(type == 'rip') %>% pull(group),
#              veg_ripdetail,
#              path = paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_riparian'),
#              format = 'GTiff', overwrite = TRUE)
#
# split_raster(rkey %>% filter(type == 'wet') %>% pull(code),
#              rkey %>% filter(type == 'wet') %>% pull(group),
#              veg_permwetland_update,
#              path = paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_riparian'),
#              format = 'GTiff', overwrite = TRUE)
