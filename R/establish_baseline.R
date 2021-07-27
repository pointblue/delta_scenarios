# Establish baseline land cover, with different encodings relevant to different
# models: waterbirds, riparian landbirds, and multiple benefits.

# PACKAGES & FUNCTIONS
source('R/packages.R')

# reference data:
delta = rast('GIS/delta.tif')

# directory containing layers developed for species distribution modeling
gisdir = 'V:/Project/Terrestrial/cv_riparian/distribution_modeling/'

# waterbirds---------
wkey = read_csv('V:/Project/wetland/Delta/landscape_models/predrasters_baseline/baseline/VegCAMP_crosswalk.csv',
                col_types = cols()) %>%
  dplyr::select(group = WATERBIRD, code) %>%
  distinct() %>%
  bind_rows(tibble(group = 'DUwetland',
                   code = 18)) %>%
  arrange(code) %>%
  mutate(shortlab = case_when(group == 'Irrigated pasture' ~ 'ip',
                              group == 'Dryland pasture' ~ 'dryp',
                              group == 'wetland' ~ 'wet',
                              group == 'orchard' ~ 'orch',
                              group == 'alfalfa' ~ 'alf',
                              group == 'woody wetland' ~ 'woodw',
                              group == 'developed' ~ 'dev',
                              group == 'fallow' ~ 'fal',
                              group == 'DUwetland' ~ 'duwet',
                              TRUE ~ group),
         label = recode(shortlab,
                        'ip' = 'irrigated pasture',
                        'wheat' = 'grain',
                        'row' = 'row/field crop',
                        'field' = 'row/field crop',
                        'wet' = 'wetland',
                        'DUwetland' = 'wetland',
                        'orch' = 'orchard/vineyard',
                        'fal' = 'fallow',
                        'dev' = 'urban',
                        'woodw' = 'riparian',
                        'dryp' = 'dryland pasture',
                        'alf' = 'alfalfa',
                        'duwet' = 'wetland'),
         col = c('#FFFF00', '#FF1493', '#2E8B57', '#FFA54F', '#FA8072',
                 '#FA8072', '#00008B', '#9400D3', '#FFA54F', '#CCCCCC',
                 '#4D4D4D', '#8B4513', '#87CEFA', '#FF0000', '#FFE4C4',
                 '#32CD32', '#00008B', '#FFFFFF'))
write_csv(wkey, 'data/landcover_key_waterbirds.csv')

veg_baseline_waterbird_fall = rast(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_waterbirds_fall.tif'))
levels(veg_baseline_waterbird_fall) <- wkey %>% select(id = code, shortlab) %>%
  as.data.frame()
coltab(veg_baseline_waterbird_fall) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(veg_baseline_waterbird_fall) = 'baseline'
writeRaster(veg_baseline_waterbird_fall,
            'data/landcover_waterbirds_fall/baseline_waterbird_fall.tif')

veg_baseline_waterbird_winter = rast(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_waterbirds_winter.tif'))
levels(veg_baseline_waterbird_winter) <- wkey %>%
  select(id = code, shortlab) %>% as.data.frame()
coltab(veg_baseline_waterbird_winter) <- wkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(veg_baseline_waterbird_winter) = 'baseline'
writeRaster(veg_baseline_waterbird_winter,
            'data/landcover_waterbirds_winter/baseline_waterbird_winter.tif')


# riparian-------
rkey = read_csv(paste0(gisdir, 'GIS/landscape_rasters/key.csv'),
                col_types = cols()) %>%
  filter(type == 'veg' & !code %in% c(110, 120, 999)) %>%
  select(-type) %>%
  mutate(label = recode(group,
                        'ORCHVIN' = 'orchard/vineyard',
                        'RICE' = 'rice',
                        'AG' = 'other crops',
                        'IDLE' = 'fallow',
                        'GRASSPAS' = 'grassland/pasture',
                        'URBAN' = 'urban',
                        'RIPARIAN' = 'riparian',
                        'WETLAND' = 'wetland',
                        'WATER' = 'water',
                        'OAKWOODLAND' = 'forest',
                        'BARREN' = 'barren'),
         col = c('#9400D3', '#32CD32', '#FF1493', '#CCCCCC', '#FFE4C4',
                 '#4D4D4D', '#FF0000', '#00008B', '#87CEFA', '#8B4513',
                 '#FFFFFF'))
write_csv(rkey, 'data/landcover_key_riparian.csv')

veg_baseline_riparian = rast(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_riparian.tif'))
levels(veg_baseline_riparian) <- rkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(veg_baseline_riparian) <- rkey %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(veg_baseline_riparian) = 'baseline'
writeRaster(veg_baseline_riparian,
            'data/landcover_riparian/baseline_riparian.tif')

## riparian detail------
rkey2 = read_csv(paste0(gisdir, 'GIS/landscape_rasters/key.csv'),
                 col_types = cols()) %>%
  filter(type == 'rip') %>%
  select(-type) %>%
  mutate(label = recode(group,
                        'POFR' = 'cottonwood forest',
                        'QULO' = 'oak forest',
                        'SALIX' = 'willow forest',
                        'MIXEDFOREST' = 'mixed forest',
                        'INTROSCRUB' = 'introduced scrub',
                        'SALIXSHRUB' = 'willow shrub',
                        'MIXEDSHRUB' = 'mixed shrub',
                        'OTHER' = 'other riparian'),
         col = c('#32CD32', '#8B4513', '#FF1493', '#9400D3',
                 '#CCCCCC', '#FF0000', '#FFA54F', '#FFFFFF'))
write_csv(rkey2, 'data/landcover_key_riparian_detail.csv')

veg_baseline_ripdetail = rast(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_riparian_detail.tif'))
levels(veg_baseline_ripdetail) <- rkey2 %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(veg_baseline_ripdetail) <- rkey2 %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(veg_baseline_ripdetail) = 'baseline_ripdetail'
plot(veg_baseline_ripdetail)
writeRaster(veg_baseline_ripdetail,
            'data/landcover_riparian/baseline_riparian_detail.tif')

## perm wetlands-------
rkey3 = read_csv(paste0(gisdir, 'GIS/landscape_rasters/key.csv'),
                 col_types = cols()) %>%
  filter(type == 'wet') %>%
  select(-type) %>%
  mutate(label = recode(group,
                        'PERM' = 'permanent wetland'),
         col = c('#00008B'))
write_csv(rkey3, 'data/landcover_key_riparian_permwetland.csv')

veg_baseline_ripperm = rast(paste0(gisdir, 'GIS/landscape_rasters/veg_baseline_riparian_permwetland.tif'))
levels(veg_baseline_ripperm) <- rkey3 %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(veg_baseline_ripperm) <- rkey3 %>% select(code, col) %>%
  complete(code = c(0:255)) %>% pull(col)
names(veg_baseline_ripperm) = 'baseline_ripperm'
plot(veg_baseline_ripperm)
writeRaster(veg_baseline_ripperm,
            'data/landcover_riparian/baseline_riparian_permwetland.tif')


# multiple benefits-------
mkey = tibble(group = c('orchard/vineyard', 'citrus', 'orchard', 'vineyard',
                        'row/field', 'corn', 'grains', 'cotton',
                        'tomato', 'rice', 'grassland/pasture', 'alfalfa',
                        'riparian', 'wetland', 'other'),
              code = c(10, 11, 12, 13,
                       21, 22, 23, 24,
                       25, 30, 50, 51,
                       70, 80, 90),
              col = c('#9400D3', NA, NA, NA,
                      '#FA8072', '#FFFF00', '#FFA54F', NA,
                      NA, '#FF1493', '#FFE4C4', '#32CD32',
                      '#FF0000', '#00008B', '#4D4D4D'))
write_csv(mkey, 'data/landcover_key_multiplebenefits.csv')

veg_baseline_mb = DeltaMultipleBenefits::reclassify_multiplebenefits(veg_baseline_waterbird_fall)
levels(veg_baseline_mb) <- mkey %>%
  select(id = code, label = group) %>% as.data.frame()
coltab(veg_baseline_mb) <- mkey %>% select(code, col) %>% filter(!is.na(col)) %>%
  complete(code = c(0:255)) %>% pull(col)
names(veg_baseline_mb) = 'baseline'
plot(veg_baseline_mb)
writeRaster(veg_baseline_mb,
            'data/landcover_multiplebenefits/baseline_mb.tif')
