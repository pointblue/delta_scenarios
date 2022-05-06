# README---------
# From baseline land cover layers, compile landcover stats for use with
# riparian spatial models

# PACKAGES & FUNCTIONS
source('R/packages.R')
source('R/functions.R')

# reference data:
# delta = rast('GIS/boundaries/delta.tif')
baseline = rast('GIS/landscape_rasters/veg_baseline.tif')
scenario1 = rast('GIS/scenario_rasters/scenario1_restoration.tif')
scenario2 = rast('GIS/scenario_rasters/scenario2_perennialexpand.tif')
key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')

delta = rast('GIS/boundaries/delta.tif')
delta_buff10k = read_sf('GIS/boundaries/Legal_Delta_boundary_buff10k.shp') %>%
  st_transform(crs = '+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')
delta_buff10k = rasterize(vect(delta_buff10k), extend(delta, delta_buff10k))
delta0 = subst(delta_buff10k, 1, 0)

# RECLASSIFY-------
# reclassify landcover rasters to match encodings used by distribution models

# main classes:
riprasters = c(baseline, scenario1, scenario2) %>%
  classify(
    rcl = key %>% select(from = CODE_BASELINE, to = CODE_RIPARIAN) %>%
      drop_na() %>%
      # exclude riparian and wetland subclass detail
      mutate(to = case_when(from >= 70 & from < 80 ~ 70,
                            from >= 80 & from < 90 ~ 80,
                            TRUE ~ to)) %>%
      as.matrix())
levels(riprasters) <- list(
  key %>% select(CODE_RIPARIAN, LABEL_RIPARIAN) %>%
    filter(CODE_RIPARIAN <= 70 | CODE_RIPARIAN == 80 | CODE_RIPARIAN >= 90) %>%
    drop_na() %>% as.data.frame(),
  key %>% select(CODE_RIPARIAN, LABEL_RIPARIAN) %>%
    filter(CODE_RIPARIAN <= 70 | CODE_RIPARIAN == 80 | CODE_RIPARIAN >= 90) %>%
    drop_na() %>% as.data.frame(),
  key %>% select(CODE_RIPARIAN, LABEL_RIPARIAN) %>%
    filter(CODE_RIPARIAN <= 70 | CODE_RIPARIAN == 80 | CODE_RIPARIAN >= 90) %>%
    drop_na() %>% as.data.frame())
names(riprasters) = c('baseline', 'scenario1', 'scenario2')
plot(riprasters)

# # check conversions
# crosstab(c(baseline, riprasters$baseline), useNA = TRUE, long = TRUE)

# subclasses:
riprasters_detail = c(baseline, scenario1, scenario2) %>%
  classify(
    baseline,
    rcl = key %>% select(from = CODE_BASELINE, to = CODE_RIPARIAN) %>%
      drop_na() %>% filter(from > 70 & from <= 81 & from != 80) %>% as.matrix(),
    othersNA = TRUE)
levels(riprasters_detail) <- list(
  key %>% select(CODE_RIPARIAN, LABEL_RIPARIAN) %>%
    filter(CODE_RIPARIAN > 70 & CODE_RIPARIAN <= 81 & CODE_RIPARIAN != 80) %>%
    as.data.frame(),
  key %>% select(CODE_RIPARIAN, LABEL_RIPARIAN) %>%
    filter(CODE_RIPARIAN > 70 & CODE_RIPARIAN <= 81 & CODE_RIPARIAN != 80) %>%
    as.data.frame(),
  key %>% select(CODE_RIPARIAN, LABEL_RIPARIAN) %>%
    filter(CODE_RIPARIAN > 70 & CODE_RIPARIAN <= 81 & CODE_RIPARIAN != 80) %>%
    as.data.frame())
names(riprasters_detail) = c('baseline', 'scenario1', 'scenario2')
plot(riprasters_detail)
# crosstab(c(baseline, riprasters_detail$baseline), useNA = TRUE, long = TRUE)


# COMPILE MODEL METRICS------
# for baseline and each scenario

## Climate variables-------
# assume no change from baseline

bioclim1_orig = rast('GIS/landscape_rasters/wc2.1_30s_bio_1.tif')
bioclim1 <- bioclim1_orig %>% project(baseline, method = 'bilinear') %>%
  mask(delta_buff10k)
names(bioclim1) = 'bio_1'
writeRaster(bioclim1, 'GIS/landscape_rasters/riparian_predstack/bio_1.tif')

bioclim12_orig = rast('GIS/landscape_rasters/wc2.1_30s_bio_12.tif')
bioclim12 <- bioclim12_orig %>% project(baseline, method = 'bilinear') %>%
  mask(delta_buff10k)
names(bioclim12) = 'bio_12'
writeRaster(bioclim12, 'GIS/landscape_rasters/riparian_predstack/bio_12.tif',
            overwrite = TRUE)


## Distance to stream----
# assume no change from baseline

# first crop to extent of baseline layer
stream = rast('GIS/landscape_rasters/streams.tif') %>% crop(baseline)
writeRaster(stream, 'GIS/landscape_rasters/streams_deltabuff10k.tif')

# # PYTHON APPROACH: (not currently working -- 'No raster store is configurated')
# arcpy$CheckOutExtension("Spatial")
# stream_py = arcpy$sa$Raster('GIS/landscape_rasters/streams_deltabuff10k.tif')
# streamdist = arcpy$sa$EucDistance(stream_py)
#
# streamdist = distance(pathin = 'GIS/landscape_rasters',
#                       filename = 'streams_deltabuff10k.tif')

# # SLOWER METHOD:
# streamdist = terra::distance(stream)
# streamdist_mask = streamdist %>% mask(delta)
# streamdist_mask_sqrt = sqrt(streamdist_mask)
# writeRaster(streamdist_mask_sqrt,
#             filename = 'GIS/landscape_rasters/riparian_predstack/streamdist_sqrt.tif')

#**for now just use old original raster; should be the same:
streamdist = rast('GIS/landscape_rasters/riparian/streamdist_merge.tif') %>%
  crop(delta_buff10k) %>% mask(delta_buff10k)
names(streamdist) = 'streamdist'
writeRaster(streamdist,
            filename = 'GIS/landscape_rasters/riparian_predstack/streamdist.tif')


## Riparian patch shape--------
# Goal: mean shape metric for riparian patches within 2km
memory.limit(size = 48000)

library(landscapemetrics)

# first reclassify veg data so everything but riparian is NA
rip = classify(riprasters, rcl = matrix(c(70, 70), nrow = 1), othersNA = TRUE)

# moving window of radius 2000
mat2k = focalMat(rip$baseline, d = 2000, type = 'circle')
wt = mat2k
wt[wt > 0] = 1

# rip = raster::stack(rip) # convert to raster

# Note: Each of these steps is very slow! It may improve if/when
# the landscapemetrics package adopts terra:focal instead of relying on the
# slower raster:focal function

# baseline
baseline_shape_raw = window_lsm(
  rip$baseline, window = wt, what = 'lsm_l_shape_mn', directions = 8, progress = TRUE)
baseline_shape = rast(baseline_shape_raw[[1]]$lsm_l_shape_mn)
names(baseline_shape) = 'shape'
baseline_shape = cover(baseline_shape, delta0 %>% subst(0, 1)) %>%
  mask(delta_buff10k)
writeRaster(
  baseline_shape,
  filename = 'GIS/landscape_rasters/riparian_predstack/baseline/shape.tif')

# scenario1
scenario1_shape_raw = window_lsm(
  rip$scenario1, window = wt, what = 'lsm_l_shape_mn', directions = 8, progress = TRUE)
scenario1_shape = rast(scenario1_shape_raw[[1]]$lsm_l_shape_mn)
names(scenario1_shape) = 'shape'
scenario1_shape = cover(scenario1_shape, delta0 %>% subst(0, 1)) %>%
  mask(delta_buff10k)
writeRaster(
  scenario1_shape,
  filename = 'GIS/landscape_rasters/riparian_predstack/scenario1/shape.tif')

# scenario 2
scenario2_shape_raw = window_lsm(
  rip$scenario2, window = wt, what = 'lsm_l_shape_mn', directions = 8)
scenario2_shape = rast(scenario2_shape_raw[[1]]$lsm_l_shape_mn)
names(scenario2_shape) = 'shape'
scenario2_shape = cover(scenario2_shape, delta0 %>% subst(0, 1)) %>%
  mask(delta_buff10k)
writeRaster(
  scenario2_shape,
  filename = 'GIS/landscape_rasters/riparian_predstack/scenario2/shape.tif')



## Proportion cover (_50, _2000)-------
# Goals:
# - proportion cover of each land cover class within specified buffer distance

### Python prep---------
# first prep for python focal statistics calculations by splitting raster stacks
# for each scenario, with one layer for each land cover class
# - to support area calculations: fill pixel with 1 if land cover is present,
# and 0 otherwise (python will sum # of pixels by buffer size)

# Note: masklayers, if present, should be same number of layers as stack

# 50m scale: each pixel = 1/13 of area
python_prep(stack = riprasters,
            masklayers = NULL,
            pixel_value = 1/13,
            path = 'GIS/landscape_rasters/riparian',
            subdir = 'buffer50',
            layernames = levels(riprasters)[[1]] %>% na.omit() %>% as.vector(),
            suffix = '_50')

python_prep(stack = riprasters_detail,
            masklayers = NULL,
            pixel_value = 1/13,
            path = 'GIS/landscape_rasters/riparian',
            subdir = 'buffer50',
            layernames = levels(riprasters_detail)[[1]] %>% na.omit() %>% as.vector(),
            suffix = '_50')

# 2km scale: each pixel = 1/14073 of area
python_prep(stack = riprasters,
            masklayers = NULL,
            pixel_value = 1/14073,
            path = 'GIS/landscape_rasters/riparian',
            subdir = 'buffer2000',
            layernames = levels(riprasters)[[1]] %>% na.omit() %>% as.vector(),
            suffix = '_2000')

python_prep(stack = riprasters_detail,
            masklayers = NULL,
            pixel_value = 1/14073,
            path = 'GIS/landscape_rasters/riparian',
            subdir = 'buffer2000',
            layernames = levels(riprasters_detail)[[1]] %>% na.omit() %>% as.vector(),
            suffix = '_2000')

### PYTHON------------
# for faster compilation of focal stats, use python and arcpy (requires python
# installation, ArcGIS license, and Spatial Analyst extension)
# --> may function better if running RStudio as administrator!

library(reticulate)
arcpy <- reticulate::import('arcpy') #seems to work best if done before loading other packages?
arcpy$CheckOutExtension("Spatial")
reticulate::source_python('R/focal_stats.py')

memory.limit(size = 48000)

# for each pixel in each land cover class layer, calculate proportion cover
# within set buffer distance (focal stats) -- NOTE: these actually calculate the
# total number of cells; have to convert to proportion later!

combos = tibble(scenarios = c('baseline', 'scenario1', 'scenario2'),
                scales = c('50', '2000', '50')) %>%
  expand(scenarios, scales)

# NOTE: this may take 2-3 hours to run, and will generate a lot of raster files
# on disk
purrr::map2(.x = combos$scenarios,
            .y = combos$scales,
            python_run,
            type = 'riparian',
            pathin = 'GIS/landscape_rasters/riparian/',
            pathout = 'GIS/landscape_rasters/riparian_predstack/',
            zero_ras = delta0, mask = delta_buff10k)

# # alternate (slow) option, not requiring python:
# summarize_vegdat(pathin = 'GIS/landscape_rasters/riparian/baseline',
#                  pathout = 'GIS/landscape_rasters/riparian_predstack',
#                  buffer = 50)

