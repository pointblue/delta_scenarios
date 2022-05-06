# README---------
# After initial distribution modeling of both riparian landbirds and waterbird,
# and creation of a common baseline veg layer for use with Zonation
# prioritization (reflected in the report to CDFW), we decided we needed to
# create a more representative baseline veg layer for use as a baseline in the
# scenario analyses that does not include so much fallow ag land (lots of fallow
# in 2014 Land IQ during drought). We also needed to better identify different
# types of wetlands: tidal, non-tidal, and "managed".

# PACKAGES & FUNCTIONS
source('R/0_packages.R')
source('R/0_functions.R')

# reference data:
delta_shp = read_sf('GIS/boundaries/Legal_Delta_Boundary.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
delta_buff10k = st_buffer(delta_shp, dist = 10100)
delta = rast('GIS/boundaries/delta.tif')
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
  arrange(CLASS, CROP) %>% print(n = 65)
veg_clean %>% st_drop_geometry() %>% select(CLASS, CWHRCOD) %>% distinct() %>%
  filter(CLASS %in% c('RIPARIAN', 'WOODLAND', 'OAKWOODLAND', 'IDLE')) %>%
  arrange(CLASS, CWHRCOD) %>% print(n = 65)

write_sf(veg_clean, 'GIS/VEG_Delta10k_baseline.shp', append = FALSE)

# CREATE BASELINE RASTER---------
# generic raster version of baseline shp, coded to include the most detailed
# land cover classes expected to be needed in the models and scenario analyses

veg_codify = veg_clean %>%
  codify_baseline(codekey = key) %>%
  select(CODE_BASELINE, CLASS, everything())

veg_codify %>% st_drop_geometry() %>% select(CODE_BASELINE, CLASS) %>%
  distinct() %>% arrange(CODE_BASELINE) %>% print(n = 64)

veg_raster = rasterize(vect(veg_codify), template, field = 'CODE_BASELINE')
writeRaster(veg_raster, 'GIS/landscape_rasters/veg_baseline_core.tif',
            overwrite = TRUE)

## NASS------
# fill in rest of 10k buffer with NASS raster from 2014
# (same as used in waterbird study)
NASS18 = rast('GIS/original_source_data/CDL/CDL_2018_clip_20211108202757_1802460488.tif') %>%
  crop(template) %>% mask(template) %>%
  codify_NASS(season = 'fall', codekey = key)

plot(NASS18)
freq(NASS18)

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
veg_raster_fill %>% mask(delta) %>% freq() #0 generic riparian pixels within delta boundary!
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

landiq_2018_simplify %>%
  select(CLASS, CLASS_WINTER) %>%
  write_sf('GIS/VEG_LandIQ_2018_simplify.shp', append = FALSE)

# get acreage of specific crop types within each class within the Delta boundary
landiq_2018_simplify_delta = landiq_2018_simplify %>%
  st_make_valid() %>%
  st_intersection(delta_shp) %>%
  select(CODE_BASELINE, CLASS, CLASS_MAIN, SUBCLASS_MAIN,
         CODE_BASELINE_WINTER, CLASS_WINTER, SUBCLASS_WINTER) %>%
  mutate(area_ha = as.numeric(st_area(.))/10000)

ag_details = landiq_2018_simplify_delta %>% st_drop_geometry() %>%
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
crosstab(c(veg_raster_fill, landiq_2018_stack$landiq_2018_fall),
         useNA = TRUE, long = TRUE)

# allow Land IQ to replace pixels where where baseline raster is NA or any ag,
# idle, urban, or barren
veg_raster_fill_landiq = lapp(c(veg_raster_fill,
                                landiq_2018_stack$landiq_2018_fall),
                              fun = function(x, y) {
                                ifelse((x %in% c(11:60, 130) | is.na(x)) &
                                         !is.na(y), y, x)
                              })
writeRaster(veg_raster_fill_landiq,
            filename = 'GIS/landscape_rasters/veg_baseline.tif',
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
# 100% agreement on: riparian, wetlands, water, woodland&scrub (none of these
# allowed to change); high (>80%) agreement on urban, orchard_deciduous,
# vineyard, barren, rice, orchard_citrus&subtropical, grassland, pasture; less
# agreement on idle fields & annual crops (makes sense)

### winter baseline----------
# where baseline raster is suitable for a second winter crop (annual crops
# besides rice, plus fallow, grassland & pasture, barren), fill in corresponding
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
# converts to corn for the winter)

# SUMMARIZE BASELINE LAND USE DATA------
baseline = rast('GIS/landscape_rasters/veg_baseline.tif')
baseline_win = rast('GIS/landscape_rasters/veg_baseline_winter.tif')

# for total area of each class, start with primary baseline layer
baseline_ha = baseline %>% mask(delta) %>%
  calculate_area() %>%
  left_join(key %>% select(class = CODE_BASELINE, label = CODE_NAME),
            by = 'class') %>%
  mutate(label = case_when(grepl('RIPARIAN', label) ~ 'RIPARIAN',
                           grepl('WETLAND_MANAGED', label) ~ 'WETLAND_MANAGED',
                           TRUE ~ label)) %>%
  group_by(label) %>%
  summarize(area = sum(area), .groups = 'drop')

# for details on specific crops, use ag_details from Land IQ layer:
ag_details = read_csv('data/landiq2018_area_detail.csv') %>%
  select(CLASS, SUBCLASS = SUBCLASS_MAIN, area_ha, total_area, prop) %>%
  mutate(SUBCLASS = case_when(
    SUBCLASS %in%
      c('PEACHES AND NECTARINES', 'PLUMS, PRUNES OR APRICOTS', 'CHERRIES') ~
      'STONE FRUIT',
    SUBCLASS == 'PISTACHIOS' ~ 'MISCELLANEOUS DECIDUOUS',
    SUBCLASS == 'KIWIS' ~ 'MISCELLANEOUS SUBTROPICAL FRUIT',
    CLASS == 'ORCHARD_DECIDUOUS' & is.na(SUBCLASS) ~ 'MISCELLANEOUS DECIDUOUS',
    CLASS == 'ORCHARD_CITRUS&SUBTROPICAL' & is.na(SUBCLASS) ~ 'MISCELLANEOUS SUBTROPICAL FRUIT',
    CLASS == 'GRAIN&HAY' & is.na(SUBCLASS) ~ 'MISCELLANEOUS GRAIN AND HAY',
    SUBCLASS %in% c('FLOWERS, NURSERY, AND CHRISTMAS TREE FARMS') ~ 'MISCELLANEOUS TRUCK',
    CLASS == 'ROW' & is.na(SUBCLASS) ~ 'MISCELLANEOUS TRUCK',
    CLASS == 'IDLE' ~ 'IDLE',
    CLASS %in% c('URBAN', 'VINEYARD', 'WATER') ~ CLASS,
    TRUE ~ SUBCLASS)) %>%
  group_by(CLASS, SUBCLASS, total_area) %>%
  summarize(area_ha = sum(area_ha), .groups = 'drop') %>%
  full_join(baseline_ha %>% select(CLASS = label, baseline_total = area)) %>%
  mutate(total_area = case_when(CLASS %in% c('IDLE', 'URBAN') ~ baseline_total,
                                is.na(total_area) ~ baseline_total,
                                TRUE ~ total_area),
         area_ha = case_when(CLASS %in% c('IDLE', 'URBAN') ~ baseline_total,
                             is.na(area_ha) ~ baseline_total,
                             TRUE ~ area_ha),
         prop = area_ha/total_area,
         SUBCLASS = if_else(is.na(SUBCLASS), CLASS, SUBCLASS)) %>%
  select(CLASS, CLASS_AREA = total_area, SUBCLASS, SUBCLASS_AREA = area_ha,
         SUBCLASS_PROP = prop)
write_csv(ag_details, 'data/baseline_area.csv')

# identify additional area of different winter crops
winter_mask = lapp(c(baseline, baseline_win),
                   function(x, y) {ifelse(x == y, NA, y)})
writeRaster(winter_mask,
            filename = 'GIS/landscape_rasters/veg_baseline_winter_mask.tif',
            overwrite = TRUE)

gains = winter_mask %>% mask(delta) %>% calculate_area() %>%
  left_join(key %>% select(class = CODE_BASELINE, label = CODE_NAME),
            by = 'class')
losses = baseline %>% mask(winter_mask %>% subst(c(0:130), 1) %>% mask(delta)) %>%
  calculate_area() %>%
  left_join(key %>% select(class = CODE_BASELINE, label = CODE_NAME),
            by = 'class')
net = bind_rows(gains,
                losses %>% mutate(area = area * -1)) %>%
  group_by(class, label) %>%
  summarize(area = sum(area), .groups = 'drop')
write_csv(net, 'data/baseline_area_winterchange.csv')
