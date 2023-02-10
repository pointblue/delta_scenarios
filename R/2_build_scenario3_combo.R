# README---------
# experiment with a third scenario, combining restoration and perennial crop
# expansion

# PACKAGES & FUNCTIONS
source('R/0_packages.R')

# reference data:
delta = rast('GIS/boundaries/delta.tif')
baseline = c(rast('GIS/landscape_rasters/veg_baseline.tif'),
             rast('GIS/landscape_rasters/veg_baseline_winter.tif'))

# key = readxl::read_excel('GIS/VEG_Delta10k_baseline_metadata.xlsx')
key = readxl::read_excel('GIS/VEG_key.xlsx')

# scenario 1 and 2:
restore = rast('GIS/scenario_inputs/restoration_added_ripdetail.tif')
perex = rast('GIS/scenario_inputs/perex_added_detail.tif')

# exclude where perex overlaps riparian and managed wetlands
levels(perex) <- NULL
levels(baseline$baseline) <- NULL
perex_limited = lapp(c(perex, baseline$baseline),
                     function(x, y) {
                       ifelse(y %in% c(70:82), NA, x)
                     })

baseline = c(rast('GIS/landscape_rasters/veg_baseline.tif'),
             rast('GIS/landscape_rasters/veg_baseline_winter.tif'))

scenario_combo = cover(restore, perex_limited) %>% cover(baseline)

# check that restored totals haven't changed
scenario1 = rast('GIS/scenario_rasters/scenario1_restoration.tif')
freq(scenario1) %>% as_tibble() %>% filter(value %in% c(70:82))
freq(scenario_combo$lyr1) %>% as_tibble() %>% filter(value %in% c(70:82))

levels(scenario_combo[[1]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
levels(scenario_combo[[2]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(scenario_combo[[1]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
coltab(scenario_combo[[2]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(scenario_combo) = c('scenario3_combo',
                          'scenario3_combo_win')
plot(scenario_combo)
writeRaster(scenario_combo,
            paste0('GIS/scenario_rasters/', names(scenario_combo), '.tif'),
            overwrite = TRUE)

# ALTERNATE--------
perex_alt = rast('GIS/scenario_inputs/perex_added_detail_alt.tif')

# exclude where perex overlaps riparian and managed wetlands
perex_alt_limited = lapp(c(perex_alt, baseline$baseline),
                     function(x, y) {
                       ifelse(y %in% c(70:82), NA, x)
                     })

scenario_combo_alt = cover(restore, perex_alt_limited) %>% cover(baseline)

freq(scenario1) %>% as_tibble() %>% filter(value %in% c(70:82))
freq(scenario_combo_alt$lyr1) %>% as_tibble() %>% filter(value %in% c(70:82))

levels(scenario_combo_alt[[1]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
levels(scenario_combo_alt[[2]]) <- key %>%
  select(id = CODE_BASELINE, label = CODE_NAME) %>% drop_na() %>%
  as.data.frame()
coltab(scenario_combo_alt[[1]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
coltab(scenario_combo_alt[[2]]) <- key %>% select(CODE_BASELINE, COLOR) %>%
  drop_na() %>% complete(CODE_BASELINE = c(0:255)) %>% pull(COLOR)
names(scenario_combo_alt) = c('scenario3_combo_alt',
                              'scenario3_combo_alt_win')
plot(scenario_combo_alt)
writeRaster(scenario_combo_alt,
            paste0('GIS/scenario_rasters/', names(scenario_combo_alt), '.tif'),
            overwrite = TRUE)
